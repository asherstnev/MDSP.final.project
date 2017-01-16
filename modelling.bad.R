library(data.table)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(e1071)
library(randomForest)
library(xgboost)
library(Matrix)
library(pROC)

sel.data <- fread("data/bad.data.after.FE.tsv")
sel.data[, status.fctr := as.factor(status.fctr)]

# fix derm.debt and credit.score.Term
sel.data[, debt.Term := debt.fix * Term.num]

acc.curve <- function (d, predictor.feature) {
  d$the.pred <- d[[predictor.feature]]
  answer <- data.table(cutoff = -1, acc = nrow(d[status == 1]) / nrow(d))
  bin.n <- 100
  for (the.bin in c(0:bin.n)) {
    cutoff <- the.bin/bin.n
    d[, status.pred := 0]
    d[cutoff < the.pred, status.pred := 1]
    the.acc <- nrow(d[status == status.pred]) / nrow(d)
    b <- data.table(cutoff = cutoff, acc = the.acc)
    answer <- rbind(answer, b)
  }
  the.bas.acc <- round(100 * answer$acc[answer$cutoff == -1],1)
  the.max.acc <- round(100 * max(answer$acc),1)
  the.acc.imp <- round(100 * (max(answer$acc)- answer$acc[answer$cutoff == -1]), 1)
  print(paste0("Baseline accuracy: ", the.bas.acc, " %, Max accuracy: "
               , the.max.acc, "%, acc. improvement: ", the.acc.imp, "%"))
  
  answer
}

balance.data <- function(d) {
  N.status.1 <- nrow(d[status == 1])
  N.status.0 <- nrow(d[status == 0])
  
  if (N.status.0 < N.status.1) {
    d.0 <- d[status == 0]
    sel.ids <- sample(d$loan.id[d$status == 1], N.status.0)
    d.1 <- d[loan.id %in% sel.ids]
  } else {
    d.1 <- d[status == 1]
    sel.ids <- sample(d$loan.id[d$status == 0], N.status.1)
    d.0 <- d[loan.id %in% sel.ids]
  }
  
  balanced.d <- rbind(d.0, d.1)
  balanced.d <- balanced.d[sample(1:nrow(balanced.d)), ]
  balanced.d
}

######################################################################################
# split into train and testing datasets
if (1) {
  rand.seed <- 1234
  set.seed(rand.seed)
  sel.ids <- sample(sel.data$loan.id, 0.7 * nrow(sel.data))
  unbalanced.train.data <- sel.data[loan.id %in% sel.ids]
  unbalanced.test.data <- sel.data[!loan.id %in% sel.ids]
  
  train.data <- balance.data(unbalanced.train.data)
  test.data <-  balance.data(unbalanced.test.data)
  balanced.data <- rbind(train.data, test.data)
  rm(sel.ids)
}

######################################################################################
# importance analysis with RF
if (1) {
  # prepare features
  all.features <- names(sel.data)
  non.predictors <- c("loan.id", "Purpose", "Purpose.improved", "Term", "Home.Ownership"
                      , "status", "status.str", "status.fctr", "status.pred", "Months.since.last.delinquent")
  
  potential.predictors <- all.features[!all.features %in% non.predictors]
  output.features <- "status"
  
  # prepare data
  imp.train <- train.data[, c("loan.id", potential.predictors, output.features), with=F]
  imp.train <- as.data.table(do.call(data.frame,lapply(imp.train, function(x) replace(x, is.infinite(x),NA))))
  imp.train <- na.omit(imp.train)
  
  # set hyper-parameters
  the.ntree <- 1000
  the.nodesize <- 10
  
  # formulate model
  the.fmla <- as.formula(paste(output.features, paste(potential.predictors, collapse="+"), sep="~"))
  
  # train model
  system.time( imp.rf.model <- randomForest(formula = the.fmla, data = imp.train, 
                                            ntree = the.ntree, nodesize = the.nodesize, importance = T))
  
  # analyse importance info
  imp <- importance(imp.rf.model)
  dt.imp <- as.data.table(imp)
  dt.imp[, feature := dimnames(imp)[[1]]]
  dt.imp <- dt.imp[order(-`%IncMSE`)] # for "status", regression
  #dt.imp <- dt.imp[order(-MeanDecreaseAccuracy)] # for "status.fctr", regression
  rm(imp)
}

# importance analysis with XGBoost
if (1) {
  # prepare features
  all.features <- names(balanced.data)
  non.predictors <- c("loan.id", "Purpose", "Purpose.improved", "Term", "Home.Ownership"
                      , "status", "status.str", "status.fctr", "status.pred", "Months.since.last.delinquent")
  exclude.some.features <- c("Mort.Short.Term.num", "Rent.Long.Term.num", "Home.Ownership.num"
                             , "Home.Term.mod", "Home.Term.num", "Term.num")
  potential.predictors <- all.features[!all.features %in% c(non.predictors, exclude.some.features)]
  
  d.in <- train.data[, potential.predictors, with=F]
  d.out <- as.numeric(train.data[,status])
  dtrain <- xgb.DMatrix(data = data.matrix(d.in), label = d.out)
  
  nTrainiSteps <- 1000
  scale.factor <- length(d.out[d.out == 0]) / length(d.out[d.out == 1])
  the.bst.model <- xgb.train(data = dtrain
                             , max_depth = 4 # default: 6
                             , eta = 0.25 # default: 0.3
                             , nthread = 6
                             , nrounds = nTrainiSteps # defualt: 500
                             #, watchlist = watchlist
                             , objective = "binary:logistic"
                             , lambda = 1 # default: 1.0
                             , subsample = 0.8  # default: 1.0
                             , colsample_bytree = 0.8  # default: 1.0
                             
                             , scale_pos_weight = scale.factor # default: 1
                             
                             , gamma = 10 # default: 0.0 
                             , min_child_weight = 7 # default: 1.0
  )
  xgb.imp <- xgb.importance(feature_names = potential.predictors, model = the.bst.model)
  dt.xgb.imp <- as.data.table(xgb.imp[order(-xgb.imp$Gain)])
  
  train.data$prediction.xgb <- predict(the.bst.model, dtrain)
  h0 <- ggplot(train.data, aes(prediction.xgb, fill=as.factor(status))) + 
    geom_histogram(aes(y=..density..),  bins = 100, alpha=0.4, position="identity")
  d.in <- test.data[, potential.predictors, with=F]
  d.out <- as.numeric(test.data[,status])
  dtest <- xgb.DMatrix(data = data.matrix(d.in), label = d.out)
  test.data$prediction.xgb <- predict(the.bst.model, dtest)
  h1 <- ggplot(test.data, aes(prediction.xgb, fill=as.factor(status))) + 
    geom_histogram(aes(y=..density..),  bins = 100, alpha=0.4, position="identity")
  multiplot(h0, h1)
}

######################################################################################
# prepare data for training/testing
if (1) {
  output.features <- "status"
  
  # selected features with important analysis
  selected.features <- dt.imp$feature[1:5]
  selected.features <- dt.xgb.imp$Feature[1:10]
  
  # a subset ad-hoc features
  selected.features <- c("credit.score"
                         , "Term.num"
                         , "amount.to.income"
                         , "Rent.Long.Term.num"
                         , "debt.to.income"
                         , "current.to.max.credit"
                         , "debt.to.Max.Credit"
                         , "amount.fix"
  )
  # another set of ad-hoc features
  selected.features <- c(  "current.to.max.credit"
                          ,"debt.Term"
                          ,"amount.Term"
                          ,"current.to.max.credit.Term"
                          ,"amount.to.max.credit.Term"
                          ,"amount.to.credit.history.Term"
                          ,"amount.to.n.accounts.Term"
                          , "amount.to.credit.history"
                          , "Mort.Short.Term.num", "Rent.Long.Term.num"
  )
  
  g1 <- ggplot(sel.data, aes_string(selected.features[1], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 40, alpha=0.4, position="identity") + guides(fill=FALSE)
  g2 <- ggplot(sel.data, aes_string(selected.features[2], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g3 <- ggplot(sel.data, aes_string(selected.features[3], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g4 <- ggplot(sel.data, aes_string(selected.features[4], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g5 <- ggplot(sel.data, aes_string(selected.features[5], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g6 <- ggplot(sel.data, aes_string(selected.features[6], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g7 <- ggplot(sel.data, aes_string(selected.features[7], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  g8 <- ggplot(sel.data, aes_string(selected.features[8], fill="status.fctr")) + 
    geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  multiplot(g1, g2, g3, g4, g5, g6, g7, g8, cols = 3)
  
  # formulate model
  the.model.fmla <- as.formula(paste(paste(output.features, collapse="+"), paste(selected.features, collapse="+"), sep="~"))
  
  # prepare train/test data
  d.train <- train.data[, c("loan.id", selected.features, output.features), with=F]
  d.train <- as.data.table(do.call(data.frame,lapply(d.train, function(x) replace(x, is.infinite(x),NA))))
  d.train <- na.omit(d.train)
  d.test <- test.data[, c("loan.id", selected.features, output.features), with=F]
  d.test <- as.data.table(do.call(data.frame,lapply(d.test, function(x) replace(x, is.infinite(x),NA))))
  d.test <- na.omit(d.test)
  d.test.all <- unbalanced.test.data[, c("loan.id", selected.features, output.features), with=F]
  d.test.all <- as.data.table(do.call(data.frame,lapply(d.test.all, function(x) replace(x, is.infinite(x),NA))))
  d.test.all <- na.omit(d.test.all)
}

######################################################################################
# RF predictor
if (1) {
  # set hyper-parameters
  the.ntree <- 1000
  the.nodesize <- 25
  
  # training
  system.time( the.rf.model <- randomForest(formula = the.model.fmla, data = d.train, 
                                            ntree = the.ntree, nodesize = the.nodesize, importance = F))
  
  # testing
  d.train$prediction.rf <- predict(the.rf.model, d.train)
  d.test$prediction.rf <- predict(the.rf.model, d.test)
  d.test.all$prediction.rf <- predict(the.rf.model, d.test.all)
  acc.estimation.balanced <- acc.curve(d.test, "prediction.rf")
  acc.estimation.unbalanced <- acc.curve(d.test.all, "prediction.rf")
  
  # visualize predictions
  h1 <- ggplot(d.train, aes(prediction.rf, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity")
  h2 <-ggplot(d.test, aes(prediction.rf, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity")
  h3 <-ggplot(d.test.all, aes(prediction.rf, fill=as.factor(status))) + 
    geom_histogram(alpha=0.4, position="identity")
  multiplot(h1, h2, h3)
  
  # build classifier
  ggplot(acc.estimation.balanced, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("RF predictor, balanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  ggplot(acc.estimation.unbalanced, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("RF predictor, unbalanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
}

######################################################################################
# SVM regression predictor
if (1) {
  # train model
  system.time(the.svm.model <- svm(formula = the.model.fmla, data = d.train, cost = 1, gamma = 0.5))
  # testing model
  d.train$prediction.svm <- predict(the.svm.model, d.train)
  d.test$prediction.svm <- predict(the.svm.model, d.test)
  d.test.all$prediction.svm <- predict(the.svm.model, d.test.all)
  acc.estimation.balanced <- acc.curve(d.test, "prediction.svm")
  acc.estimation.unbalanced <- acc.curve(d.test.all, "prediction.svm")
  
  
  # optimize SVM hyper-parameters
  if (0) {
    svm_tune <- tune(svm, the.model.fmla, data = d.train, 
                     kernel="radial", ranges=list(cost=10^(-2:2), gamma=c(.5,1,2)))
    # results for:status ~ credit.score + Term.num + amount.to.income + Mort.Short.Term.num + 
    #  amount.to.credit.history + amount + Current.to.Max.Credit + 
    #  debt.to.max + Rent.Long.Term.num + debt.to.Max.Credit + debt.to.income + 
    #  debt.to.credit.history + debt
    #- sampling method: 10-fold cross validation 
    #  
    #   - best parameters:
    #    cost gamma
    #  1     2
    #  
    #  - best performance: 0.2596532 
    print(svm_tune)
  }
  
  # show predictions
  h1 <- ggplot(d.train, aes(prediction.svm, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h2 <- ggplot(d.test, aes(prediction.svm, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h3 <- ggplot(d.test, aes(prediction.svm, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  h4 <- ggplot(d.test.all, aes(prediction.svm, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  multiplot(h1, h2, h3, h4, cols = 2)
  
  ggplot(acc.estimation.balanced, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("SVM predictor, balanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  ggplot(acc.estimation, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("SVM predictor, unbalanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
}

######################################################################################
# logistic regression predictor
if (1) {
  # training
  system.time(the.glm.model <- glm(the.model.fmla, family=binomial(link='logit'), data=d.train))
  
  # testing
  d.test$prediction.glm <- predict(the.glm.model, d.test)
  d.test.all$prediction.glm <- predict(the.glm.model, d.test.all)
  
  # show predictions
  ggplot(d.test, aes(prediction.glm, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity")
  ggplot(d.test.all, aes(prediction.glm, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity")
  
  # building classifier
  acc.estimation <- acc.curve(d.test, "prediction.glm")
  ggplot(acc.estimation, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("Logistic predictor, balanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  acc.estimation <- acc.curve(d.test.all, "prediction.glm")
  ggplot(acc.estimation, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("Logistic predictor, unbalanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
}

######################################################################################
# XGBoost predictor
if (1) {
  results.train <- vector("list", nrow(dt.xgb.imp))
  results <- results <- vector("list", nrow(dt.xgb.imp))
  for(n.features in (2:nrow(dt.xgb.imp))) {
    print(paste0("Analyse ", n.features, " features"))
    selected.features <- dt.xgb.imp$Feature[1:n.features]
    
    #d.in <- unbalanced.train.data[, selected.features, with=F]
    #d.out <- as.numeric(unbalanced.train.data[,status])
    #dtrain <- xgb.DMatrix(data = data.matrix(d.in), label = d.out)
    
    d.in <- train.data[, selected.features, with=F]
    d.out <- as.numeric(train.data[,status])
    dtrain <- xgb.DMatrix(data = data.matrix(d.in), label = d.out)
    
    nTrainiSteps <- 10000
    scale.factor <- length(d.out[d.out == 0]) / length(d.out[d.out == 1])
    the.bst.model <- xgb.train(data = dtrain
                               , max_depth = 6 # default: 6
                               , eta = 0.1 # default: 0.3
                               , nthread = 6
                               , nrounds = nTrainiSteps # default: 500
                               #, watchlist = watchlist
                               , objective = "binary:logistic"
                               #, eval_metric = "auc"  # default: "error" for classification
                               , lambda = 1 # default: 1.0
                               
                               , gamma = 5 # default: 0.0 
                               , min_child_weight = 10 # default: 1.0
                               
                               , subsample = 0.5  # default: 1.0
                               , colsample_bytree = 0.5  # default: 1.0
                               
                               # alleviate imbalanceness in the data
                               , scale_pos_weight = scale.factor # default: 1
    )
    
    train.data$prediction.xgb <- predict(the.bst.model, dtrain)
    h0 <- ggplot(train.data, aes(prediction.xgb, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),  bins = 100, alpha=0.4, position="identity")
    results.train[[n.features]] <- h0
    
    d.in <- unbalanced.test.data[, selected.features, with=F]
    d.out <- as.numeric(unbalanced.test.data[,status])
    dtest <- xgb.DMatrix(data = data.matrix(d.in), label = d.out)
    unbalanced.test.data$prediction.xgb <- predict(the.bst.model, dtest)
    h1 <- ggplot(unbalanced.test.data, aes(prediction.xgb, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),  bins = 100, alpha=0.4, position="identity")
    h2 <- ggplot(unbalanced.test.data, aes(prediction.xgb, fill=as.factor(status))) + 
      geom_histogram(bins = 100, alpha=0.4, position="identity")
    multiplot(h1, h2)
    results[[n.features]] <- h2
  }
  
  multiplot(results.train[[2]] + guides(fill=FALSE)
            , results.train[[3]] + guides(fill=FALSE)
            , results.train[[4]] + guides(fill=FALSE)
            , results.train[[5]] + guides(fill=FALSE)
            , results.train[[6]] + guides(fill=FALSE)
            , results.train[[7]] + guides(fill=FALSE)
            , results.train[[8]] + guides(fill=FALSE)
            , results.train[[9]] + guides(fill=FALSE)
            , results.train[[10]] + guides(fill=FALSE)
            , results.train[[11]] + guides(fill=FALSE)
            , cols = 4
  )
  
  multiplot(results[[2]] + guides(fill=FALSE)
            , results[[3]] + guides(fill=FALSE)
            , results[[4]] + guides(fill=FALSE)
            , results[[5]] + guides(fill=FALSE)
            , results[[6]] + guides(fill=FALSE)
            , results[[7]] + guides(fill=FALSE)
            , results[[8]] + guides(fill=FALSE)
            , results[[9]] + guides(fill=FALSE)
            , results[[10]] + guides(fill=FALSE)
            , results[[11]] + guides(fill=FALSE)
            , cols = 4
  )
  
  acc.estimation <- acc.curve(d.test, "prediction.xgb")
  ggplot(acc.estimation, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("XGBoost predictor, balanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  acc.estimation <- acc.curve(d.test.all, "prediction.xgb")
  ggplot(acc.estimation, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("XGBoost predictor, unbalanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
}

######################################################################################
# visualize all predictions
if (1) {
  acc.estimation.rfo <- acc.curve(d.test.all, "prediction.rf")
  acc.estimation.svm <- acc.curve(d.test.all, "prediction.svm")
  acc.estimation.glm <- acc.curve(d.test.all, "prediction.glm")
  acc.estimation.xgb <- acc.curve(d.test.all, "prediction.xgb")
  acc.estimation.rfo[, type := "Random Forest"]
  acc.estimation.svm[, type := "SVM"]
  acc.estimation.glm[, type := "Logistic"]
  acc.estimation.xgb[, type := "XGBoost"]
  acc.estimation.unb <- rbind(acc.estimation.rfo, acc.estimation.svm, acc.estimation.glm, acc.estimation.xgb)
  
  acc.estimation.rfo <- acc.curve(d.test, "prediction.rf")
  acc.estimation.svm <- acc.curve(d.test, "prediction.svm")
  acc.estimation.glm <- acc.curve(d.test, "prediction.glm")
  acc.estimation.xgb <- acc.curve(d.test, "prediction.xgb")
  acc.estimation.rfo[, type := "Random Forest"]
  acc.estimation.svm[, type := "SVM"]
  acc.estimation.glm[, type := "Logistic"]
  acc.estimation.xgb[, type := "XGBoost"]
  acc.estimation.bal <- rbind(acc.estimation.rfo, acc.estimation.svm, acc.estimation.glm, acc.estimation.xgb)
  rm(acc.estimation.rfo)
  rm(acc.estimation.svm)
  rm(acc.estimation.glm)
  rm(acc.estimation.xgb)
  
  ggplot(acc.estimation.bal, aes(x = cutoff, y=acc, color=as.factor(type))) + 
    geom_line() + geom_point() +
    ggtitle("Balanced data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  ggplot(acc.estimation.unb, aes(x = cutoff, y=acc, color=as.factor(type))) + 
    geom_line() + geom_point() +
    ggtitle("Unbalanced data") + ylab("Accuracy") + xlab("Predition cut-off")
}


