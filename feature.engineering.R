library(data.table)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(FSelector)

all.sel.data <- fread("data/sel.data.tsv")

write.data <- FALSE

dh <- function(d = sel.data, f = "credit.score", nbins = 100) {
  h2 <- ggplot(d, aes_string(f, fill="status.fctr")) + geom_density(alpha=0.4)
  h2h <- ggplot(d, aes_string(f, fill="status.fctr")) + 
    geom_histogram(bins = nbins, alpha=0.4, position="identity")
  multiplot(h2, h2h) 
}

######################################################################################
# select some subsets with definitive outcome
if (1) {
  all.sel.data[, log.amount := log10(amount)]
  
  def.data.1 <- all.sel.data[credit.score > 1000]
  def.data.2 <- all.sel.data[log.amount > 7]
  
  def.ids <- c(unique(def.data.1$loan.id), unique(def.data.2$loan.id))
  def.data <- all.sel.data[loan.id %in% def.ids]
  write.table(def.data, file="data/def.data.tsv", sep="\t", row.names = F)
  rm(def.data.1)
  rm(def.data.2)
  
  # remove subsets with definite status
  sel.data <- all.sel.data[!loan.id %in% def.ids]
  
  # strange data (~21.3%): credit score and income are not available...
  strange.data <- sel.data[is.na(credit.score)]
  good.data <- sel.data[!is.na(credit.score)]
  rm(sel.data)
  rm(all.sel.data)
  rm(def.ids)
}

######################################################################################
# feature engineering for bad data
if (1) {
  sel.data <- copy(strange.data)

  # for visualization
  sel.data[, status.fctr := as.factor(status.str)]
  
  # credit score: DOES NOT EXIST!
  if (0) {
    sel.data[, log.credit.score := log10(credit.score)]
    h1 <- ggplot(sel.data, aes(credit.score, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 50, alpha=0.4, position="identity")
    h2 <- ggplot(sel.data, aes(log.credit.score, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 50, alpha=0.4, position="identity")
    multiplot(h1, h2)
  }
  sel.data[, credit.score := NULL]

  # income: DOES NOT EXIST!!!
  if (0) {
    income.limit <- 150000
    sel.data[, income.fix := income]
    sel.data[income.fix > income.limit, income.fix := income.limit]
    sel.data[, log.income := log10(income)]
    sel.data[, amount.to.income := amount / income]
    sel.data[amount.to.income > 0.5, amount.to.income := 0.5]
    dh(f = "income")
  }
  sel.data[, income := NULL]
  
  # job years
  if (0) 
  sel.data[, job.years := NULL]
  
  # debt
  if (1) {
    debt.limit <- 2500
    sel.data[, debt.fix := debt]
    sel.data[debt.fix > debt.limit, debt.fix := debt.limit]
    sel.data[, log.debt := log10(debt)]
    sel.data[is.infinite(log.debt), log.debt := 0]
    
    sel.data[, log.debt.amount := log(debt * amount)]
    
    dh(f = "debt")
    dh(f = "debt.fix")
    dh(f = "log.debt")
    dh(f = "log.debt.amount")
  }
  
  # amount
  if (1) {
    sel.data[, log.amount := log10(amount)]
    sel.data[, log.amount.2 := (log10(amount))^2]
    sel.data[, debt.to.amount := debt / amount]
    sel.data[debt.to.amount > 0.25, debt.to.amount := 0.25]
    sel.data[, debt.to.amount.log := log.debt / log.amount]
    
    dh(f = "amount")
    dh(f = "log.amount")
    dh(f = "log.amount.2")
    dh(f = "debt.to.amount")
    dh(f = "debt.to.amount.log")
  }
  
  # Years.of.Credit.History and Number.of.Open.Accounts
  if (1) {
    the.limit <- 40
    sel.data[, Years.of.Credit.History.fix := Years.of.Credit.History]
    sel.data[Years.of.Credit.History.fix > the.limit, Years.of.Credit.History.fix := the.limit]
    sel.data[, log.Years.of.Credit.History := log(Years.of.Credit.History)]
    sel.data[, amount.to.credit.history := amount / sqrt(Years.of.Credit.History)]
    sel.data[amount.to.credit.history > 10000, amount.to.credit.history := 10000]
    the.limit <- 30
    sel.data[, Number.of.Open.Accounts.fix := Number.of.Open.Accounts]
    sel.data[, log.Number.of.Open.Accounts := log(Number.of.Open.Accounts)]
    sel.data[Number.of.Open.Accounts.fix > the.limit, Number.of.Open.Accounts.fix := the.limit]
    sel.data[, amount.to.n.accounts := amount / sqrt(Number.of.Open.Accounts)]
    sel.data[amount.to.n.accounts > 15000, amount.to.n.accounts := 15000]

    dh(f = "log.Years.of.Credit.History")
    dh(f = "Years.of.Credit.History.fix")
    dh(f = "Number.of.Open.Accounts")
    dh(f = "log.Number.of.Open.Accounts")
    
    dh(f = "amount.to.credit.history")
    dh(f = "amount.to.n.accounts")
  }
  
  # Bankruptcies and Tax.Liens
  if (0) {
    sel.data[Bankruptcies > 3, Bankruptcies := 3]
    ggplot(sel.data, aes(Bankruptcies, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity")
    
    sel.data[Tax.Liens > 3, Tax.Liens := 3]
    ggplot(sel.data, aes(Tax.Liens, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 20, alpha=0.4, position="identity")
    
    sel.data[, bad.events := Bankruptcies + Tax.Liens]
    #sel.data[, bad.events := Bankruptcies - Tax.Liens]
    sel.data[bad.events > 1, bad.events := 1]
    ggplot(sel.data, aes(bad.events, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 20, alpha=0.4, position="identity")
  }
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
    
    dh(f = "log.Current.Credit.Balance")
    dh(f = "log.Maximum.Open.Credit")
    dh(f = "current.to.max.credit")
    dh(f = "debt.to.max.credit")
    dh(f = "amount.to.max.credit")
  }
  
  # Number.of.Credit.Problems
  if (0) {
    sel.data[, Number.of.Credit.Problems.fix := 0]
    sel.data[0 < Number.of.Credit.Problems , Number.of.Credit.Problems.fix := 1]
    dh(f = "Number.of.Credit.Problems.fix")
  }
  sel.data[, Number.of.Credit.Problems := NULL]
  
  # Months.since.last.delinquent
  if (0) {
    the.limit <- 24
    sel.data[, Months.since.last.delinquent.fix := Months.since.last.delinquent]
    sel.data[is.na(Months.since.last.delinquent) , Months.since.last.delinquent.fix := -10]
    sel.data[Months.since.last.delinquent > the.limit, Months.since.last.delinquent.fix := the.limit]
    sel.data[is.na(Months.since.last.delinquent) , rev.months := 0]
    sel.data[!is.na(Months.since.last.delinquent), rev.months := 1/Months.since.last.delinquent]
    sel.data[is.infinite(rev.months), rev.months := 1]
    sel.data[rev.months > 0.1, rev.months := 0.1]
    
    dh(f = "Months.since.last.delinquent")
  }
  sel.data[, Months.since.last.delinquent := NULL]
  
  # quantify categorical features
  if (1) {
    # Home.Ownership
    chr.values <- c("HaveMortgage", "Home Mortgage", "Own Home", "Rent")
    sel.data[Home.Ownership %in% c("HaveMortgage", "Home Mortgage"), Home.Ownership := "Mortgage"]
    
    sel.data[, Home.Ownership.num := 0]
    sel.data[Home.Ownership == "Rent", Home.Ownership.num := -1]
    sel.data[Home.Ownership == "Mortgage", Home.Ownership.num := 1]
    chr.values <- c("Long Term", "Short Term")
    sel.data[, Term.num := -1]
    sel.data[Term == "Short Term", Term.num := 1]
    h1 <- ggplot(sel.data, aes(Home.Ownership.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    h2 <- ggplot(sel.data, aes(Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    multiplot(h1, h2)
    
    sel.data[, Rent.Long.Term.num := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Rent.Long.Term.num := -1]
    sel.data[, Mort.Short.Term.num := -1]
    sel.data[Home.Ownership.num == 1 & Term == "Short Term", Mort.Short.Term.num := 1]
    h3 <- ggplot(sel.data, aes(Rent.Long.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 6, alpha=0.4, position="identity")
    h4 <- ggplot(sel.data, aes(Mort.Short.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    multiplot(h3, h4)
    
    sel.data[, Home.Term.num := 0]
    sel.data[Home.Ownership == "Own Home" & Term == "Long Term", Home.Term.num := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Home.Term.num := 2]
    sel.data[Home.Ownership == "Mortgage" & Term == "Long Term", Home.Term.num := 3]
    
    sel.data[Home.Ownership == "Own Home" & Term == "Short Term", Home.Term.num := -1]
    sel.data[Home.Ownership == "Rent" & Term == "Short Term", Home.Term.num := -2]
    sel.data[Home.Ownership == "Mortgage" & Term == "Short Term", Home.Term.num := -3]
    ggplot(sel.data, aes(Home.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 7, alpha=0.4, position="identity")
    
    sel.data[, Home.Term.mod := 0]
    sel.data[Home.Term.num %in% c(2,3), Home.Term.mod := -1]
    sel.data[Home.Term.num %in% c(-3), Home.Term.mod := 1]
    ggplot(sel.data, aes(Home.Term.mod, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 3, alpha=0.4, position="identity")
    
    # Purpose
    if (0) {
      sel.data[, Purpose.improved := Purpose]
      sel.data["Other" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Medical Bills" == Purpose.improved, Purpose.improved := "other"]
      sel.data["wedding" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Educational Expenses" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Business Loan" == Purpose.improved | "small_business" == Purpose.improved, Purpose.improved := "business"]
      sel.data["Take a Trip" == Purpose.improved | "vacation" == Purpose.improved, Purpose.improved := "trip"]
      sel.data["moving" == Purpose.improved, Purpose.improved := "trip"]
      sel.data["renewable_energy" == Purpose.improved | "Home Improvements" == Purpose.improved, Purpose.improved := "home"]
      sel.data["Buy a Car" == Purpose.improved | "Buy House" == Purpose.improved, Purpose.improved := "major_purchase"]
      sel.data["Debt Consolidation" == Purpose.improved, Purpose.improved := "debt"]
      
      sel.data[, Purpose.improved.num := 0]
      sel.data["other"                == Purpose.improved, Purpose.improved.num := 1]
      sel.data["home"                 == Purpose.improved, Purpose.improved.num := 2]
      sel.data["business"             == Purpose.improved, Purpose.improved.num := 3]
      sel.data["major_purchase"       == Purpose.improved, Purpose.improved.num := 4]
      sel.data["trip"                 == Purpose.improved, Purpose.improved.num := 5]
      ggplot(sel.data, aes(Purpose.improved.num, fill=as.factor(status))) + 
        geom_histogram(aes(y=..density..), bins = 33, alpha=0.4, position="identity")
    }
    
    sel.data[, debt.Term := debt * Term.num]
    sel.data[, amount.Term := amount * Term.num]
    sel.data[, current.to.max.credit.Term := current.to.max.credit * Term.num]
    sel.data[, amount.to.max.credit.Term := amount.to.max.credit * Term.num]
    sel.data[, amount.to.credit.history.Term := amount.to.credit.history * Term.num]
    sel.data[, amount.to.n.accounts.Term := amount.to.n.accounts * Term.num]
    
    dh(f = "debt.Term")
    dh(f = "amount.Term")
    dh(f = "current.to.max.credit.Term")
    dh(f = "amount.to.max.credit.Term")
    dh(f = "amount.to.credit.history.Term")
    dh(f = "amount.to.n.accounts.Term")
  }
  
  # remove some features:
  #sel.data[]
  #debt
  #debt.fix
  #log.debt
  #Number.of.Open.Accounts
  #log.Number.of.Open.Accounts
  #Number.of.Open.Accounts.fix
  #Current.Credit.Balance
  #log.Current.Credit.Balance
  
  if (write.data) {
    write.table(sel.data, file="data/bad.data.after.FE.tsv", sep="\t", row.names = F)
  }
}

if(1) {
  all.features <- names(sel.data)
  non.predictors <- c("loan.id", "Purpose", "Purpose.improved", "Term", "Home.Ownership"
                      , "status", "status.str", "status.fctr", "status.pred", "Months.since.last.delinquent")
  
  potential.predictors <- all.features[!all.features %in% non.predictors]
  output.features <- "status"
  the.fmla <- as.formula(paste("status", paste(potential.predictors, collapse="+"), sep="~"))
  
  # Information Gain
  ig.weights <- information.gain(the.fmla, sel.data)
  dt.ig.weights <- as.data.table(ig.weights)
  dt.ig.weights[, feature := rownames(ig.weights)]
  dt.ig.weights <- dt.ig.weights[order(-attr_importance)]
  
  # Consistency, strange results...
  con.weights <- consistency(the.fmla, sel.data)
}

######################################################################################
# feature engineering for good data
if (1) {
  sel.data <- copy(good.data)

  # 2.5% data
  if (0) {
    a1 <- 0.975
    a2 <- 1.025
    good.data[, percent25 := -1]
    good.data[
      (35000*a1 < amount & amount <= 35000*a2) | 
        (30000*a1 < amount & amount <= 30000*a2) |
        (25000*a1 < amount & amount <= 25000*a2) | 
        (28000*a1 < amount & amount <= 28000*a2) | 
        (20000*a1 < amount & amount <= 20000*a2) | 
        (15000*a1 < amount & amount <= 15000*a2) | 
        (12000*a1 < amount & amount <= 12000*a2) | 
        (10000*a1 < amount & amount <= 10000*a2) |
        ( 9000*a1 < amount & amount <=  9000*a2) | 
        ( 8000*a1 < amount & amount <=  8000*a2) | 
        ( 7000*a1 < amount & amount <=  7000*a2) | 
        ( 6000*a1 < amount & amount <=  6000*a2) | 
        ( 5000*a1 < amount & amount <=  5000*a2) | 
        ( 4000*a1 < amount & amount <=  4000*a2) | 
        ( 3000*a1 < amount & amount <=  3000*a2) | 
        ( 2000*a1 < amount & amount <=  2000*a2) | 
        ( 1000*a1 < amount & amount <=  1000*a2)
      , percent25 := 1]  
    sel.data <- good.data[percent25 == 1]
    unsel.data <- good.data[percent25 == 1]
    
    ggplot(sel.data, aes(percent25, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 3, alpha=0.4, position="identity")
    dh(sel.data[percent25 == 1], f = "amount")
    dh(sel.data[percent25 == -1], f = "amount")
    dh(sel.data, f = "amount", 199)
  }
  
  #sel.data[, amount.last.digit := amount %% 10]
  #ggplot(sel.data, aes(amount.last.digit, fill=status.fctr)) + 
  #  geom_histogram(aes(y=..density..), bins = 9, alpha=0.4, position="identity")
  
  # for visualization
  sel.data[, status.fctr := as.factor(status.str)]
  
  # credit score
  if (1) {
    sel.data[, log.credit.score := log10(credit.score)]
    dh(f = "credit.score", nbins = 45)
    dh(f = "log.credit.score", nbins = 45)
  }
  
  #ggplot(sel.data, aes(credit.score, fill=as.factor(Purpose))) + geom_histogram(bins = 45, alpha=0.4, position="identity")
  
  
  # income
  if (1) {
    sel.data[, log.income := log10(income)]
    income.limit <- 150000
    sel.data[, income.fix := income]
    sel.data[income.fix > income.limit, income.fix := income.limit]
    sel.data[, income.to.score := income / credit.score^2]
    sel.data[income.to.score > 0.5, income.to.score := 0.5]
    dh(f = "income")
    dh(f = "log.income")
    dh(f = "income.fix")
    dh(f = "income.to.score")
  }
  
  # amount
  if (1) {
    sel.data[, log.amount := log10(amount)]
    sel.data[, amount.to.income := amount / income]
    sel.data[amount.to.income > 0.5, amount.to.income := 0.5]
    sel.data[, amount.to.score := amount / credit.score]
    dh(f = "amount")
    dh(f = "log.amount")
    dh(f = "amount.to.income")
    dh(f = "amount.to.score")
  }
  
  # debt
  if (1) {
    debt.limit <- 2500
    sel.data[, debt.fix := debt]
    sel.data[debt.fix > debt.limit, debt.fix := debt.limit]
    sel.data[, log.debt := log10(debt)]
    sel.data[is.infinite(log.debt), log.debt := 0]
    
    sel.data[, log.debt.amount := log(debt * amount)]
    sel.data[, debt.to.amount := debt / amount]
    sel.data[debt.to.amount > .5, debt.to.amount := .5]
    sel.data[, debt.to.income := debt / income]
    sel.data[, debt.to.score := debt / credit.score]
    sel.data[debt.to.score > 5, debt.to.score := 5]

    dh(f = "debt")
    dh(f = "debt.fix")
    dh(f = "log.debt")
    dh(f = "log.debt.amount")
    dh(f = "debt.to.amount")
    dh(f = "debt.to.income")
    dh(f = "debt.to.score")
  }
  
  # job years: bad
  if (0) {
    sel.data[job.years > 10, job.years := 10]
    sel.data[, job.years.exists := 1]
    sel.data[0 > job.years, job.years.exists := -1]
    dh(f = "job.years")
    ggplot(sel.data, aes(job.years.exists, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 3, alpha=0.4, position="identity")
  }
  sel.data[, job.years := NULL]

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
    sel.data[, income.to.credit.history := income / Years.of.Credit.History]
    sel.data[income.to.credit.history > 10000, income.to.credit.history := 10000]
    sel.data[, income.to.n.accounts := income / Number.of.Open.Accounts]
    sel.data[income.to.n.accounts > 15000, income.to.n.accounts := 15000]
    
    
    dh(f = "log.Years.of.Credit.History")
    dh(f = "Years.of.Credit.History.fix")
    dh(f = "Number.of.Open.Accounts")
    dh(f = "log.Number.of.Open.Accounts")
    
    dh(f = "amount.to.credit.history")
    dh(f = "amount.to.n.accounts")
    dh(f = "income.to.credit.history")
    dh(f = "income.to.n.accounts")
  }
  
  # Bankruptcies and Tax.Liens: bad...
  if (0) {
    sel.data[Bankruptcies > 3, Bankruptcies := 3]
    ggplot(sel.data, aes(Bankruptcies, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity")
    
    sel.data[Tax.Liens > 3, Tax.Liens := 3]
    ggplot(sel.data, aes(Tax.Liens, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 20, alpha=0.4, position="identity")
    
    sel.data[, bad.events := Bankruptcies + Tax.Liens]
    #sel.data[, bad.events := Bankruptcies - Tax.Liens]
    sel.data[bad.events > 1, bad.events := 1]
    ggplot(sel.data, aes(bad.events, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 20, alpha=0.4, position="identity")
  }
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
    sel.data[, max.credit.to.income := Maximum.Open.Credit / income]
    sel.data[max.credit.to.income > 2, max.credit.to.income := 2]
    
    dh(f = "log.Current.Credit.Balance")
    dh(f = "log.Maximum.Open.Credit")
    dh(f = "current.to.max.credit")
    dh(f = "debt.to.max.credit")
    dh(f = "amount.to.max.credit")
  }
  
  # Number.of.Credit.Problems: bad...
  if (0) {
    sel.data[, Number.of.Credit.Problems.fix := 0]
    sel.data[0 < Number.of.Credit.Problems , Number.of.Credit.Problems.fix := 1]
    ggplot(sel.data, aes(Number.of.Credit.Problems.fix, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 2, alpha=0.4, position="identity")
  }
  sel.data[, Number.of.Credit.Problems := NULL]
  
  # Months.since.last.delinquent: bad...
  if (0) {
    Months.since.last.delinquent.limit <- 24
    sel.data[, Months.since.last.delinquent.fix := Months.since.last.delinquent]
    sel.data[is.na(Months.since.last.delinquent) , Months.since.last.delinquent.fix := -10]
    sel.data[Months.since.last.delinquent > Months.since.last.delinquent.limit
             , Months.since.last.delinquent.fix := Months.since.last.delinquent.limit]
    h1 <- ggplot(sel.data, 
                 aes(Months.since.last.delinquent.fix, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 25, alpha=0.4, position="identity")

    sel.data[is.na(Months.since.last.delinquent) , rev.months := 0]
    sel.data[!is.na(Months.since.last.delinquent), rev.months := 1/Months.since.last.delinquent]
    sel.data[is.infinite(rev.months), rev.months := 1]
    sel.data[rev.months > 0.1, rev.months := 0.1]
    h2 <- ggplot(sel.data, aes(rev.months, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 100, alpha=0.4, position="identity")
    multiplot(h1, h2)
  }
  sel.data[, Months.since.last.delinquent := NULL]
  
  # quantify categorical features
  if (1) {
    # Home.Ownership
    chr.values <- c("HaveMortgage", "Home Mortgage", "Own Home", "Rent")
    sel.data[Home.Ownership %in% c("HaveMortgage", "Home Mortgage"), Home.Ownership := "Mortgage"]
    
    sel.data[, Home.Ownership.num := 0]
    sel.data[Home.Ownership == "Rent", Home.Ownership.num := -1]
    sel.data[Home.Ownership == "Mortgage", Home.Ownership.num := 1]
    chr.values <- c("Long Term", "Short Term")
    sel.data[, Term.num := -1]
    sel.data[Term == "Short Term", Term.num := 1]
    h1 <- ggplot(sel.data, aes(Home.Ownership.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    h2 <- ggplot(sel.data, aes(Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    multiplot(h1, h2)
    
    sel.data[, Rent.Long.Term.num := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Rent.Long.Term.num := -1]
    sel.data[, Mort.Short.Term.num := -1]
    sel.data[Home.Ownership.num == 1 & Term == "Short Term", Mort.Short.Term.num := 1]
    h3 <- ggplot(sel.data, aes(Rent.Long.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 6, alpha=0.4, position="identity")
    h4 <- ggplot(sel.data, aes(Mort.Short.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..), bins = 6, alpha=0.4, position="identity")
    multiplot(h3, h4)
    
    sel.data[, Home.Term.num := 0]
    sel.data[Home.Ownership == "Own Home" & Term == "Long Term", Home.Term.num := 1]
    sel.data[Home.Ownership == "Rent" & Term == "Long Term", Home.Term.num := 2]
    sel.data[Home.Ownership == "Mortgage" & Term == "Long Term", Home.Term.num := 3]
    
    sel.data[Home.Ownership == "Own Home" & Term == "Short Term", Home.Term.num := -1]
    sel.data[Home.Ownership == "Rent" & Term == "Short Term", Home.Term.num := -2]
    sel.data[Home.Ownership == "Mortgage" & Term == "Short Term", Home.Term.num := -3]
    ggplot(sel.data, aes(Home.Term.num, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 7, alpha=0.4, position="identity")
    
    sel.data[, Home.Term.mod := 0]
    sel.data[Home.Term.num %in% c(2,3), Home.Term.mod := -1]
    sel.data[Home.Term.num %in% c(-3), Home.Term.mod := 1]
    ggplot(sel.data, aes(Home.Term.mod, fill=as.factor(status))) + 
      geom_histogram(aes(y=..density..),bins = 3, alpha=0.4, position="identity")
    
    # Purpose
    if (0) {
      sel.data[, Purpose.improved := Purpose]
      sel.data["Other" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Medical Bills" == Purpose.improved, Purpose.improved := "other"]
      sel.data["wedding" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Educational Expenses" == Purpose.improved, Purpose.improved := "other"]
      sel.data["Business Loan" == Purpose.improved | "small_business" == Purpose.improved, Purpose.improved := "business"]
      sel.data["Take a Trip" == Purpose.improved | "vacation" == Purpose.improved, Purpose.improved := "trip"]
      sel.data["moving" == Purpose.improved, Purpose.improved := "trip"]
      sel.data["renewable_energy" == Purpose.improved | "Home Improvements" == Purpose.improved, Purpose.improved := "home"]
      sel.data["Buy a Car" == Purpose.improved | "Buy House" == Purpose.improved, Purpose.improved := "major_purchase"]
      sel.data["Debt Consolidation" == Purpose.improved, Purpose.improved := "debt"]
      
      sel.data[, Purpose.improved.num := 0]
      sel.data["other"                == Purpose.improved, Purpose.improved.num := 1]
      sel.data["home"                 == Purpose.improved, Purpose.improved.num := 2]
      sel.data["business"             == Purpose.improved, Purpose.improved.num := 3]
      sel.data["major_purchase"       == Purpose.improved, Purpose.improved.num := 4]
      sel.data["trip"                 == Purpose.improved, Purpose.improved.num := 5]
      ggplot(sel.data, aes(Purpose.improved.num, fill=as.factor(status))) + 
        geom_histogram(aes(y=..density..), bins = 33, alpha=0.4, position="identity")
    }
    
    sel.data[, debt.Term := debt * Term.num]
    sel.data[, credit.score.Term := credit.score * Term.num]
    sel.data[, amount.Term := amount * Term.num]
    sel.data[, current.to.max.credit.Term := current.to.max.credit * Term.num]
    sel.data[, amount.to.max.credit.Term := amount.to.max.credit * Term.num]
    sel.data[, amount.to.credit.history.Term := amount.to.credit.history * Term.num]
    sel.data[, amount.to.n.accounts.Term := amount.to.n.accounts * Term.num]
    
    dh(f = "debt.Term")
    dh(f = "credit.score.Term")
    dh(f = "amount.Term")
    dh(f = "current.to.max.credit.Term")
    dh(f = "amount.to.max.credit.Term")
    dh(f = "amount.to.credit.history.Term")
    dh(f = "amount.to.n.accounts.Term")

    ggplot(sel.data[status == 1], aes(credit.score, fill=as.factor(Term))) + 
      geom_histogram(aes(y=..density..),bins = 45, alpha=0.4, position="identity")
  }
  
  if (write.data) {
    write.table(sel.data, file="data/good.data.after.FE.tsv", sep="\t", row.names = F)
  }
}

if(1) {
  all.features <- names(sel.data)
  non.predictors <- c("loan.id", "Purpose", "Purpose.improved", "Term", "Home.Ownership"
                    , "status", "status.str", "status.fctr", "status.pred", "Months.since.last.delinquent")

  potential.predictors <- all.features[!all.features %in% non.predictors]
  output.features <- "status"
  the.fmla <- as.formula(paste("status", paste(potential.predictors, collapse="+"), sep="~"))
  
  # Information Gain
  ig.weights <- information.gain(the.fmla, sel.data)
  dt.ig.weights <- as.data.table(ig.weights)
  dt.ig.weights[, feature := rownames(ig.weights)]
  dt.ig.weights <- dt.ig.weights[order(-attr_importance)]
  rm(ig.weights)
}