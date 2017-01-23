library(data.table)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(e1071)
library(randomForest)
library(xgboost)
library(Matrix)
library(pROC)

######################################################################################
# load data
all.sel.data <- fread("data/small.credict.score.good.data.after.FE.tsv")
all.sel.data[, status.fctr := as.factor(status.fctr)]
sel.data <- all.sel.data[credit.score < 740]
table(all.sel.data$status.str)/nrow(all.sel.data)
table(sel.data.1$status.str)/nrow(sel.data.1)


######################################################################################
# functions
acc.curve <- function (d, predictor.feature) {
  d$the.pred <- d[[predictor.feature]]
  answer <- data.table(cutoff = -1, acc = nrow(d[status == 1]) / nrow(d))
  bin.n <- 200
  for (the.bin in c(0:bin.n)) {
    cutoff <- the.bin/bin.n
    d[, status.pred := 0]
    d[cutoff < the.pred, status.pred := 1]
    the.acc <- nrow(d[status == status.pred]) / nrow(d)
    b <- data.table(cutoff = cutoff, acc = the.acc)
    answer <- rbind(answer, b)
  }
  the.bas.acc <- round(100 * answer$acc[answer$cutoff == -1], 2)
  the.max.acc <- round(100 * max(answer$acc), 2)
  the.acc.imp <- round(100 * (max(answer$acc)- answer$acc[answer$cutoff == -1]), 2)
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
if (0) {
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
  system.time( the.rf.model <- randomForest(formula = the.fmla, data = imp.train, 
                                            ntree = the.ntree, nodesize = the.nodesize, importance = T))
  
  # analyse importance info
  imp <- importance(the.rf.model)
  dt.imp2 <- as.data.table(imp)
  dt.imp2[, feature := dimnames(imp)[[1]]]
  dt.imp2 <- dt.imp2[order(-`%IncMSE`)] # for "status", regression
  #dt.imp <- dt.imp[order(-MeanDecreaseAccuracy)] # for "status.fctr", regression
  rm(imp)
}

######################################################################################
# prepare data for training/testing
if (1) {
  output.features <- "status"
  
  # a subset ad-hoc features
  selected.features <- c("credit.score"
                         , "log.income" 
                         , "current.to.max.credit"
                         , "debt.Term"
                         , "credit.score.Term"
                         #, "amount.Term"
                         , "current.to.max.credit.Term"
                         #, "amount.to.max.credit.Term"
                         #, "amount.to.credit.history.Term"
                         , "income.to.n.accounts.Term"
                         , "Mort.Short.Term.num"
                         , "Rent.Long.Term.num"
  )
  
  selected.features <- dt.imp2$feature[1:15]

  if (0) {
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
  }
  
  # formulate model
  the.model.fmla <- as.formula(paste(paste(output.features, collapse="+"), paste(selected.features, collapse="+"), sep="~"))
  
  # prepare train/test data
  d.train <- train.data[, c("loan.id", selected.features, output.features), with=F]
  d.train <- as.data.table(do.call(data.frame,lapply(d.train, function(x) replace(x, is.infinite(x),NA))))
  d.train <- na.omit(d.train)
  d.train.all <- unbalanced.train.data[, c("loan.id", selected.features, output.features), with=F]
  d.train.all <- as.data.table(do.call(data.frame,lapply(d.train.all, function(x) replace(x, is.infinite(x),NA))))
  d.train.all <- na.omit(d.train.all)
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
  the.ntree <- 10000
  the.nodesize <- 50
  
  # training model
  system.time( the.rf.model <- randomForest(formula = the.model.fmla, data = d.train.all, 
                                            ntree = the.ntree, nodesize = the.nodesize))
  
  # testing model
  d.train$prediction.rf <- predict(the.rf.model, d.train)
  d.train.all$prediction.rf <- predict(the.rf.model, d.train.all)
  d.test$prediction.rf <- predict(the.rf.model, d.test)
  d.test.all$prediction.rf <- predict(the.rf.model, d.test.all)
  acc.estimation.balanced <- acc.curve(d.test, "prediction.rf")
  acc.estimation.unbalanced <- acc.curve(d.test.all, "prediction.rf")
  
  # visualize predictions
  h1 <- ggplot(d.train, aes(prediction.rf, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h2 <- ggplot(d.test, aes(prediction.rf, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h3 <- ggplot(d.train.all, aes(prediction.rf, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  h4 <- ggplot(d.test.all, aes(prediction.rf, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  multiplot(h1, h2, h3, h4, cols = 2)
  
  # visualize accuracy curves
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
  rand.seed <- 1234
  set.seed(rand.seed)
  system.time(the.svm.model <- svm(formula = the.model.fmla, data = d.train, cost = 1, gamma = 0.125))

  # training on both training and testing data
  if (0) {
    d <- rbind(d.train, d.test)
    d.all <- rbind(d.train.all, d.test.all)
    rand.seed <- 1234
    set.seed(rand.seed)
    system.time(the.svm.model <- svm(formula = the.model.fmla, data = d, cost = 1, gamma = 0.125))
    d.all$prediction.svm <- predict(the.svm.model, d.all)
    ggplot(d.all, aes(prediction.svm, fill=as.factor(status))) + 
      geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
    acc.estimation.all <- acc.curve(d.all, "prediction.svm")
  }

  # testing model
  d.train$prediction.svm <- predict(the.svm.model, d.train)
  d.test$prediction.svm <- predict(the.svm.model, d.test)
  d.test.all$prediction.svm <- predict(the.svm.model, d.test.all)
  acc.estimation.balanced <- acc.curve(d.test, "prediction.svm")
  acc.estimation.unbalanced <- acc.curve(d.test.all, "prediction.svm")

  # visualize predictions
  h1 <- ggplot(d.train, aes(prediction.svm, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h2 <- ggplot(d.test, aes(prediction.svm, fill=as.factor(status))) + 
    geom_density(alpha=0.4, position="identity") + guides(fill=FALSE)
  h3 <- ggplot(d.test, aes(prediction.svm, fill=as.factor(status))) + 
    geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  h4 <- ggplot(d.test.all, aes(prediction.svm, fill=as.factor(status))) + 
    geom_histogram( bins = 50, alpha=0.4, position="identity") + guides(fill=FALSE)
  multiplot(h1, h2, h3, h4, cols = 2)
  
  # visualize accuracy curves
  ggplot(acc.estimation.balanced, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("SVM predictor, balanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
  ggplot(acc.estimation.unbalanced, aes(x = cutoff, y=acc)) + 
    geom_line() + geom_point() +
    ggtitle("SVM predictor, unbalanced testing data") + ylab("Accuracy") + xlab("Predition cut-off")
}

######################################################################################
# visualize predictions by both models
if (1) {
  acc.estimation.rfo <- acc.curve(d.test.all, "prediction.rf")
  acc.estimation.svm <- acc.curve(d.test.all, "prediction.svm")
  acc.estimation.rfo[, type := "Random Forest"]
  acc.estimation.svm[, type := "SVM"]
  acc.estimation.unb <- rbind(acc.estimation.rfo, acc.estimation.svm)
  
  acc.estimation.rfo <- acc.curve(d.test, "prediction.rf")
  acc.estimation.svm <- acc.curve(d.test, "prediction.svm")
  acc.estimation.rfo[, type := "Random Forest"]
  acc.estimation.svm[, type := "SVM"]
  acc.estimation.bal <- rbind(acc.estimation.rfo, acc.estimation.svm)
  rm(acc.estimation.rfo)
  rm(acc.estimation.svm)
  
  ggplot(acc.estimation.bal, aes(x = cutoff, y=acc, color=as.factor(type))) + 
    geom_line() + geom_point() +
    ggtitle("Balanced data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  ggplot(acc.estimation.unb, aes(x = cutoff, y=acc, color=as.factor(type))) + 
    geom_line() + geom_point() +
    ggtitle("Unbalanced data") + ylab("Accuracy") + xlab("Predition cut-off")
  
  the.trained.model <- the.svm.model
  the.cut.off <- 0.09
  
  fnl.test.all <- unbalanced.test.data[, c("loan.id", selected.features, output.features), with=F]
  fnl.test.all <- as.data.table(do.call(data.frame,lapply(fnl.test.all, function(x) replace(x, is.infinite(x),NA))))
  fnl.test.all <- merge(fnl.test.all, d.test.all[, .(loan.id, prediction.svm)], by="loan.id", all.x =T)
  fnl.test.all[is.na(prediction.svm), prediction.svm := 1]
  write.table(fnl.test.all, file="data/small.credit.data.test.tsv", sep="\t", row.names = F)
}

######################################################################################
# pack the model
if (1) {
  # 15 features, SVM trained on d.train with cost = 1, gamma = 0.125
  outfile <- "Results/MLM_good_v10.RData"
  # 15 features, SVM trained on d.train+d.test with cost = 1, gamma = 0.125
  outfile <- "Results/MLM_good_v11.RData"
  
  the.model <- list()
  the.model[['the.model']] <- the.trained.model
  the.model[['the.cutoff']] <- the.cut.off
  the.model[['the.model.formula']] <- the.model.fmla
  save(the.model, file = outfile)
  
  the.result <- merge(unbalanced.test.data[, .(loan.id, status.str)]
                      , d.test.all[, .(loan.id, status, prediction.svm)], by="loan.id", all.x=T)
  the.result[is.na(prediction.svm), prediction.svm := 1]
  the.result[, status.predicted := "Fully Paid"]
  the.result[prediction.svm < the.cut.off, status.predicted := "Charged Off"]
  the.result[, status.str := as.character(status.str)]
  nrow(the.result[status.str == status.predicted]) / nrow(the.result)
  
  write.table(the.result, "data/good.data.test.tsv", sep="\t", row.names = F)
  write.table(d.test.all[, c("loan.id", selected.features), with=F], "data/good.data.model.input.tsv", sep="\t", row.names = F)
}
