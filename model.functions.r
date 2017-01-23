library(data.table)
library(e1071)

data.cleansing <- function(in.data) {
  
  # convert potentially character feature into numeric
  in.data[, debt := as.numeric(debt.str)]
  in.data[, income := as.numeric(income.str)]
  in.data[, amount := as.numeric(amount.str)]
  
  # fix features with $ or £
  if (0 < nrow(in.data[is.na(debt)])) {
    in.data[is.na(debt), debt.str := gsub("[$£]","", gsub(",","", debt.str))]
    in.data[is.na(debt), debt := as.numeric(debt.str)]
    in.data[, debt.str := NULL]
  }
  in.data[, income.str := NULL]
  in.data[, amount.str := NULL]
  
  # remove pure duplicates
  sel.data <- in.data[!duplicated(in.data)]
  
  # remove records with wrong amount, income, or credit.score values
  label.wrong.records <- function(amount, income, credit.score) {
    if (1 == length(amount))
      return(TRUE)
    flag <- rep(TRUE, length(amount))
    flag[amount == 99999999 | is.na(income) | is.na(credit.score)] <- FALSE
    return(flag)
  }
  system.time(sel.data[, keep.record := label.wrong.records(amount, income, credit.score), by=.(customer.id)])
  sel.data <- sel.data[T == keep.record]
  sel.data[, keep.record := NULL]

  # convert int features into numeric ones
  # NOT USED sel.data[, Months.since.last.delinquent := as.numeric(Months.since.last.delinquent)]
  sel.data[, Number.of.Open.Accounts      := as.numeric(Number.of.Open.Accounts)]
  sel.data[, Number.of.Credit.Problems    := as.numeric(Number.of.Credit.Problems)]
  sel.data[, Current.Credit.Balance       := as.numeric(Current.Credit.Balance)]
  sel.data[, Maximum.Open.Credit          := as.numeric(Maximum.Open.Credit)]   
  #NOT used sel.data[, Bankruptcies                 := as.numeric(Bankruptcies)]         
  sel.data[, credit.score                 := as.numeric(credit.score)]     
  
  # remove one id
  sel.data[, customer.id := NULL]
  
  sel.data
}

feature.engineering <- function(sel.data) {

  # credit score
  if (1) {
    sel.data[credit.score > 1000, credit.score := credit.score / 10]
    sel.data[credit.score < 1, credit.score := 1]
  }
  
  # income
  if (1) {
    sel.data[, log.income := log10(income)]
    income.limit <- 150000
    sel.data[, income.fix := income]
    sel.data[income.fix > income.limit, income.fix := income.limit]
    sel.data[, income.to.score := income / credit.score^2]
    sel.data[income.to.score > 0.5, income.to.score := 0.5]
  }
  
  # amount
  if (1) {
    sel.data[log10(amount) > 7, amount := 1]
    sel.data[, log.amount := log10(amount)]
    sel.data[, amount.to.income := amount / income]
    sel.data[amount.to.income > 0.5, amount.to.income := 0.5]
    sel.data[, amount.to.score := amount / credit.score]
  }
  
  # debt
  if (1) {
    debt.limit <- 2500
    sel.data[, debt.fix := debt]
    sel.data[debt.fix > debt.limit, debt.fix := debt.limit]
    sel.data[, log.debt := log10(debt)]
    sel.data[is.infinite(log.debt), log.debt := 0]
    
    sel.data[, log.debt.income := log(debt.fix * income.fix)]
    sel.data[, debt.to.income := debt / income]
    sel.data[, debt.to.score := debt / credit.score]
    sel.data[debt.to.score > 5, debt.to.score := 5]
  }
  
  # job years: bad...
  sel.data[, Years.in.current.job := NULL]
  
  # Years.of.Credit.History and Number.of.Open.Accounts
  if (1) {
    the.limit <- 40
    sel.data[, Years.of.Credit.History.fix := Years.of.Credit.History]
    sel.data[Years.of.Credit.History.fix > the.limit, Years.of.Credit.History.fix := the.limit]
    sel.data[, log.Years.of.Credit.History := log(Years.of.Credit.History)]
    the.limit <- 30
    sel.data[, Number.of.Open.Accounts.fix := Number.of.Open.Accounts]
    sel.data[, log.Number.of.Open.Accounts := log(Number.of.Open.Accounts)]
    sel.data[Number.of.Open.Accounts.fix > the.limit, Number.of.Open.Accounts.fix := the.limit]
    
    sel.data[, amount.to.credit.history := amount / sqrt(Years.of.Credit.History)]
    sel.data[amount.to.credit.history > 10000, amount.to.credit.history := 10000]
    sel.data[, amount.to.n.accounts := amount / sqrt(Number.of.Open.Accounts)]
    sel.data[amount.to.n.accounts > 15000, amount.to.n.accounts := 15000]
    
    sel.data[, income.to.credit.history := income / Years.of.Credit.History ]
    sel.data[income.to.credit.history > 10000, income.to.credit.history := 10000]
    sel.data[, income.to.n.accounts := income / Number.of.Open.Accounts]
    sel.data[income.to.n.accounts > 15000, income.to.n.accounts := 15000]
    sel.data[, debt.to.credit.history := debt / Years.of.Credit.History]
    sel.data[debt.to.credit.history > 250, debt.to.credit.history := 250]
    sel.data[, debt.to.n.accounts := debt / Number.of.Open.Accounts]
    sel.data[debt.to.n.accounts > 300, debt.to.n.accounts := 300]
  }
  
  # Bankruptcies and Tax.Liens: bad...
  sel.data[, Bankruptcies := NULL]
  sel.data[, Tax.Liens := NULL]
  
  # Current.Credit.Balance and Maximum.Open.Credit
  if (1) {
    sel.data[, log.Current.Credit.Balance := log10(Current.Credit.Balance)]
    sel.data[, log.Maximum.Open.Credit := log10(Maximum.Open.Credit)]
    sel.data[, current.to.max.credit := Current.Credit.Balance / Maximum.Open.Credit]
    sel.data[current.to.max.credit > 1, current.to.max.credit := 1]
    sel.data[, debt.to.max.credit := debt / sqrt(Maximum.Open.Credit)]
    sel.data[debt.to.max.credit > 20, debt.to.max.credit := 20]
    sel.data[is.na(debt.to.max.credit), debt.to.max.credit := 20]
    
    sel.data[, amount.to.max.credit := amount / sqrt(Maximum.Open.Credit)]
    sel.data[amount.to.max.credit > 300, amount.to.max.credit := 300]
    sel.data[is.na(amount.to.max.credit), amount.to.max.credit := 300]
    
    sel.data[, current.credit.to.income := Current.Credit.Balance / income]
    sel.data[current.credit.to.income > 1, current.credit.to.income := 1]
    sel.data[, max.credit.to.income := Maximum.Open.Credit / income]
    sel.data[max.credit.to.income > 1, max.credit.to.income := 1]
    sel.data[, debt.to.max.credit := debt / Maximum.Open.Credit]
    sel.data[debt.to.max.credit > 0.25, debt.to.max.credit := 0.25]
    sel.data[, debt.to.current.credit := debt / Current.Credit.Balance]
    sel.data[debt.to.current.credit > 1, debt.to.current.credit := 1]
  }
  
  # Number.of.Credit.Problems: bad...
  sel.data[, Number.of.Credit.Problems := NULL]
  
  # Months.since.last.delinquent
  if (1) {
    sel.data[is.na(Months.since.last.delinquent) , Years.since.last.delinquent := -1]
    sel.data[Months.since.last.delinquent <= 12, Years.since.last.delinquent := 0]
    sel.data[12 < Months.since.last.delinquent & Months.since.last.delinquent <= 24, Years.since.last.delinquent := 1]
    sel.data[24 < Months.since.last.delinquent, Years.since.last.delinquent := 2]
    sel.data[, Months.since.last.delinquent := NULL]
    
    sel.data[, credit.score.YSLD := credit.score * Years.since.last.delinquent]
    sel.data[, current.to.max.credit.YSLD := current.to.max.credit * Years.since.last.delinquent]
  }
  
  # quantify categorical features
  if (1) {
    # Home.Ownership
    chr.values <- c("HaveMortgage", "Home Mortgage", "Own Home", "Rent")
    sel.data[Home.Ownership %in% c("HaveMortgage", "Home Mortgage"), Home.Ownership := "Mortgage"]
    sel.data[, Home.Ownership.num := 0]
    sel.data[Home.Ownership == "Rent", Home.Ownership.num := -1]
    sel.data[Home.Ownership == "Mortgage", Home.Ownership.num := 1]
    
    # Term
    chr.values <- c("Long Term", "Short Term")
    sel.data[, Term.num := -1]
    sel.data[Term == "Short Term", Term.num := 1]
    
    # new features: Rent.Long.Term.num & Mort.Short.Term.num
    sel.data[, Rent.Long.Term.num := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Rent.Long.Term.num := -1]
    sel.data[, Mort.Short.Term.num := -1]
    sel.data[Home.Ownership.num == 1 & Term == "Short Term", Mort.Short.Term.num := 1]
    
    # new feature: Ownership.Term
    sel.data[, Ownership.Term := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Ownership.Term := -1]
    sel.data[Home.Ownership == "Mortgage" & Term == "Long Term", Ownership.Term := -1]
    
    # remove: Purpose
    sel.data[, Purpose := NULL]
    
    # new, weighted features
    sel.data[, amount.Term := amount * Term.num]
    sel.data[, amount.to.max.credit.Term := amount.to.max.credit * Term.num]
    sel.data[, amount.to.credit.history.Term := amount.to.credit.history * Term.num]
    sel.data[, amount.to.n.accounts.Term := amount.to.n.accounts * Term.num]
    
    sel.data[, debt.Term := debt.fix * Term.num]
    sel.data[, credit.score.Term := (credit.score - 600) * Term.num]
    sel.data[, current.to.max.credit.Term := current.to.max.credit * Term.num]
    sel.data[, log.income.Term := (log.income - 3.6) * Term.num]
    sel.data[, income.to.n.accounts.Term := income.to.n.accounts * Term.num]
    sel.data[, debt.OT := debt.fix * Ownership.Term]
    sel.data[, credit.score.OT := (credit.score - 600) * Ownership.Term]
    sel.data[, current.to.max.credit.OT := current.to.max.credit * Ownership.Term]
    sel.data[, log.income.OT := (log.income - 4) * Ownership.Term]
    sel.data[, income.to.n.accounts.OT := income.to.n.accounts * Ownership.Term]
  }

  sel.data
}

rename.raw.data <- function(raw.data) {
  # rename some columns
  setnames(raw.data, "Loan ID",                      "loan.id")
  setnames(raw.data, "Customer ID",                  "customer.id")
  setnames(raw.data, "Loan Status",                  "status.str")
  setnames(raw.data, "Current Loan Amount",          "amount.str")
  setnames(raw.data, "Credit Score",                 "credit.score")
  setnames(raw.data, "Years in current job",         "Years.in.current.job")
  setnames(raw.data, "Home Ownership",               "Home.Ownership")
  setnames(raw.data, "Annual Income",                "income.str")
  setnames(raw.data, "Monthly Debt",                 "debt.str")
  setnames(raw.data, "Years of Credit History",      "Years.of.Credit.History")
  setnames(raw.data, "Months since last delinquent", "Months.since.last.delinquent")
  setnames(raw.data, "Number of Open Accounts",      "Number.of.Open.Accounts")
  setnames(raw.data, "Number of Credit Problems",    "Number.of.Credit.Problems")
  setnames(raw.data, "Current Credit Balance",       "Current.Credit.Balance")
  setnames(raw.data, "Maximum Open Credit",          "Maximum.Open.Credit")
  setnames(raw.data, "Tax Liens",                    "Tax.Liens")
  
  # convert charachter status into numeric
  # Charged Off (BAD!) -> 0
  # Fully Paid (GOOD!) -> 1
  raw.data[, status := 0]
  raw.data[status.str == "Fully Paid", status := 1]
  
  raw.data
}

model.launcher <- function(raw.data, m1, m2, cutoff.1 = 0.5, cutoff.2 = 0.5, verbose = 0) {
  
  rep.t <- system.time(in.data <- rename.raw.data(copy(raw.data)))
  if (0 < verbose) {
    print("Feature renaming done")
    if (1 < verbose)
      print(rep.t)
  }
  
  # define input features
  if (1) {
    # bad model features
    features.bad <-c ("current.to.max.credit"
                       , "debt.Term"
                       , "amount.Term"                   # DONE
                       , "amount.to.credit.history"      # DONE
                       , "amount.to.max.credit.Term"     # DONE
                       , "amount.to.credit.history.Term" # DONE
                       , "amount.to.n.accounts.Term"     # DONE
                       , "current.to.max.credit.Term"    # DONE
                       , "Mort.Short.Term.num"
                       , "Rent.Long.Term.num"
    )
    
    # good data with small credit score:
    features.good <- c( "amount.to.income"
                        , "log.income.Term"
                        , "debt.to.income"
                        , "income.to.score"
                        , "log.income.OT"
                        , "credit.score.OT"
                        , "log.debt"
                        , "income.to.n.accounts.Term"
                        , "income.to.n.accounts.OT"
                        , "credit.score"
                        , "log.amount"
                        , "current.to.max.credit.Term"
                        , "credit.score.Term"
                        , "income"
                        , "debt.Term")
  }
  
  # data cleansing step
  rep.t <- system.time(clean.data <- data.cleansing(copy(in.data)))
  clean.data[, group := "good.data"]
  clean.data[is.na(credit.score), group := "bad.data"]
  if (0 < verbose) {
    print("Cleansing done")
    if (1 < verbose)
      print(rep.t)
  }
  
  # feature engineering
  rep.t <- system.time(fe.data <- feature.engineering(copy(clean.data)))
  if (0 < verbose) {
    print("Feature engineering done")
    if (1 < verbose)
      print(rep.t)
  }
  
  # split into good and bad data
  fe.data.good <- fe.data[!is.na(credit.score), c("loan.id", "group", features.good), with=F]
  fe.data.bad <- fe.data[is.na(credit.score), c("loan.id", "group", features.bad), with=F]

  # predict good data
  if (1) {
    fe.data.good1 <- fe.data.good[credit.score < 705]
    fe.data.good1 <- na.omit(fe.data.good1)
    
    if(0) {
      cc <- copy(fe.data.good)
      cc[, group := NULL]
      old.data <- fread("data/small.credit.data.test.tsv")
      old.data <- old.data[, colnames(cc), with=F]
      library(compare)
      compare(cc, old.data, round = T, allowAll = T)
    }
    
    fe.data.good2 <- fe.data.good[!loan.id %in% fe.data.good1$loan.id]
    fe.data.good2[, the.score := 1]
    rep.t <- system.time(fe.data.good1$the.score <- predict(m1, fe.data.good1))
    answer.good <- rbind(fe.data.good1, fe.data.good2)
    answer.good[, status.predicted := "Fully Paid"]
    answer.good[the.score < cutoff.1, status.predicted := "Charged Off"]
    if (0 < verbose) {
      print(paste0("Prediction stage 1 done for ", length(fe.data.good1$loan.id), " loans"))
      if (1 < verbose)
        print(rep.t)
    }
  }
  
  # predict bad data
  fe.data.bad1 <- na.omit(fe.data.bad)
  
  if(0) {
    old.data <- fread("data/bad.data.model.input.tsv")
    old.data <- old.data[, colnames(fe.data.bad1), with=F]
    library(compare)
    compare(fe.data.bad1, old.data, round = T, allowAll = T)
    
    bu <- merge(fe.data.good1[, .(loan.id, amount.to.credit.history.Term)], old.data[, .(loan.id, amount.to.credit.history.Term)], by="loan.id")
    bu[, the.diff := abs(amount.to.credit.history.Term.x - amount.to.credit.history.Term.y)]
    nrow(bu[amount.to.credit.history.Term.x != amount.to.credit.history.Term.y])
  }

  fe.data.bad2 <- fe.data.bad[!loan.id %in% fe.data.bad1$loan.id]
  fe.data.bad2[, the.score := 1]
  rep.t <- system.time(fe.data.bad1$the.score <- predict(m2, fe.data.bad1))
  answer.bad <- rbind(fe.data.bad1, fe.data.bad2)
  answer.bad[, status.predicted := "Fully Paid"]
  answer.bad[the.score < cutoff.2, status.predicted := "Charged Off"]
  if (0 < verbose) {
    print(paste0("Prediction stage 2 done for ", length(fe.data.bad1$loan.id), " loans"))
    if (1 < verbose)
      print(rep.t)
  }
  
  if (0) {
    old.data <- fread("data/bad.data.test.tsv")
    bu <- merge(answer.bad, old.data, by="loan.id")
    library(ggplot2)
    ggplot(answer.bad, aes(the.score)) + 
      geom_histogram( bins = 100, alpha=0.4, position="identity") + guides(fill=FALSE)
  }
  
  # form output dataset
  the.output <- rbind(answer.good[, .(loan.id, group, the.score, status.predicted)]
                    , answer.bad[, .(loan.id, group, the.score, status.predicted)])
  the.output
}

##################################################################################################
# load models
if (1) {
  modelfile1 <- "Results/MLM_bad_v2.RData"
  load(modelfile1)
  the.bad.data.model <- the.model[['the.model']]
  the.bad.data.cutoff <- the.model[['the.cutoff']]
  rm(the.model)
  
  modelfile2 <- "Results/MLM_good_v11.RData"
  load(modelfile2)
  the.good.data.model <- the.model[['the.model']]
  the.good.data.cutoff <- the.model[['the.cutoff']]
  rm(the.model)
}

##################################################################################################
# testing with test data
if (1) {
  if (1) {
    raw.data.all <- fread("data/LoansTrainingSetV2_fixed.csv")
    test.id1 <- fread("data/bad.data.test.tsv")
    test.id2 <- fread("data/small.credit.data.test.tsv")
    raw.data.test <- raw.data.all[`Loan ID` %in% c(test.id1$loan.id, test.id2$loan.id)]
    #raw.data.test <- raw.data.all[`Loan ID` %in% test.id2$loan.id]
  }
  
  # # do calculations
  # raw.data = raw.data.test
  # m1 = the.good.data.model
  # m2 = the.bad.data.model
  # cutoff.1 = the.good.data.cutoff
  # cutoff.2 = the.bad.data.cutoff
  # verbose = 1
  the.answer <- model.launcher(raw.data = raw.data.test
                               , m1 = the.good.data.model
                               , m2 = the.bad.data.model
                               , cutoff.1 = the.good.data.cutoff
                               , cutoff.2 = the.bad.data.cutoff
                               , verbose = 1)
  
  full.answer <- merge(the.answer, raw.data.test, by.x="loan.id", by.y="Loan ID")
  nrow(full.answer[`Loan Status` == status.predicted]) / nrow(full.answer)
}

##################################################################################################
# ultimate test
if (1) {
  raw.data.limited <- fread("data/LoansTrainingSetV2.csv", na.strings=c("NA","N/A","null", "#VALUE!"))
  raw.data.all <- fread("data/big.LoansTrainingSetV2.csv", na.strings=c("NA","N/A","null", "#VALUE!"))

  limited.unique.id <- unique(raw.data.limited$`Loan ID`)
  all.unique.id <- unique(raw.data.all$`Loan ID`)
  unused.id <- all.unique.id[!all.unique.id %in% limited.unique.id]
  used.id <- all.unique.id[all.unique.id %in% limited.unique.id]
  raw.data.test <- raw.data.all[`Loan ID` %in% unused.id]
  
  # raw.data <- raw.data.test
  # m1 <- the.good.data.model
  # m2 <- the.bad.data.model
  # cutoff.1 <- the.good.data.cutoff
  # cutoff.2 <- the.bad.data.cutoff
  # verbose <- 2
  the.answer <- model.launcher(raw.data = raw.data.test
                               , m1 = the.good.data.model
                               , m2 = the.bad.data.model
                               , cutoff.1 = the.good.data.cutoff
                               , cutoff.2 = the.bad.data.cutoff
                               , verbose = 2)
  the.answer <- the.answer[!duplicated(loan.id)]
  
  full.answer <- merge(the.answer, raw.data.test[, .(`Loan ID`, `Loan Status`)], by.x="loan.id", by.y="Loan ID", all.y=T)
  setnames(full.answer, "Loan Status", "status.real")
  
  if (0) {
    library(ggplot2)
    library(Rmisc)
    h1 <- ggplot(full.answer, aes(the.score, fill=as.factor(group))) + geom_histogram(bins = 50)
    h2 <- ggplot(full.answer, aes(the.score, fill=as.factor(status.real))) + geom_histogram(bins = 50)
    h3 <- ggplot(full.answer, aes(the.score, fill=as.factor(status.predicted))) + geom_histogram(bins = 50)
    multiplot(h1, h2, h3)
  }
  
  nrow(full.answer[status.real == status.predicted]) / nrow(full.answer)
}

##################################################################################################
# pack the whole model
if (1) {
  outfile <- "Results/MLM_small.credit.score_v2.RData"
  
  the.model <- list()
  # the models
  the.model[['the.model.bad.data']] <- the.bad.data.model
  the.model[['the.cutoff.bad.data']] <- the.bad.data.cutoff
  the.model[['the.model.good.data']] <- the.good.data.model
  the.model[['the.cutoff.good.data']] <- the.good.data.cutoff

  # the functions
  the.model[['data.cleansing']] <- data.cleansing
  the.model[['feature.engineering']] <- feature.engineering
  the.model[['model.launcher']] <- model.launcher
  the.model[['rename.raw.data']] <- rename.raw.data
  
  save(the.model, file = outfile)
}

