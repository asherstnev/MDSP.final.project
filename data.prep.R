library(data.table)
library(compare)

######################################################################################
# load raw data
load.raw.data <- function(in.file) {
  raw.data <- fread(in.file)

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

raw.data <- load.raw.data("data/LoansTrainingSetV2_fixed.csv")
new.raw.data <- load.raw.data("data/big.LoansTrainingSetV2_fixed.csv")

######################################################################################
# data cleansing function
data.cleansing <- function(in.data) {
  
  # convert potentially character feature into numeric
  in.data[, debt := as.numeric(debt.str)]
  in.data[, income := as.numeric(income.str)]
  in.data[, amount := as.numeric(amount.str)]
  
  # fix features with $ or £
  if (0 < nrow(in.data[is.na(debt)])) {
    in.data[is.na(debt), debt.str := gsub("[$£]","", debt.str)]
    in.data[is.na(debt), debt := as.numeric(debt.str)]
    in.data[, debt.str := NULL]
  }
  #in.data[is.na(income), income.str := gsub("[$£]","", income.str)]
  #in.data[is.na(income), income := as.numeric(income.str)]
  in.data[, income.str := NULL]
  
  #in.data[is.na(amount), amount.str := gsub("[$£]","", amount.str)]
  #in.data[is.na(amount), amount := as.numeric(amount.str)]
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
  sel.data[, keep.record := label.wrong.records(amount, income, credit.score), by=.(customer.id)]
  sel.data <- sel.data[T == keep.record]
  sel.data[, keep.record := NULL]
  sel.data[, N.records := .N, by=.(loan.id)]
  
  if (0) {
    # number of Loan/Customer ID
    sel.data[, N.customers := .N, by=.(loan.id)]
    sel.data[, N.loan.ids := .N, by=.(customer.id)]
    table(sel.data$N.customers)
    table(sel.data$N.loan.ids)
    sel.data[, N.customers := NULL]
    sel.data[, N.loan.ids := NULL]
  }
  
  # convert Years in current job into numerical feature
  sel.data[, job.years := -99]
  sel.data[Years.in.current.job == "n/a", job.years := -1]
  sel.data[Years.in.current.job == "< 1 year", job.years := 0]
  sel.data[Years.in.current.job == "10+ years", job.years := 99]
  f <- function(c){as.numeric(strsplit(c, split = " ")[1][[1]][1])}
  sel.data[job.years == -99, job.years := sapply(Years.in.current.job, f)]
  sel.data[, Years.in.current.job := NULL]
  
  # convert int features into numeric ones
  sel.data[, Months.since.last.delinquent := as.numeric(Months.since.last.delinquent)]
  sel.data[, Number.of.Open.Accounts      := as.numeric(Number.of.Open.Accounts)]
  sel.data[, Number.of.Credit.Problems    := as.numeric(Number.of.Credit.Problems)]
  sel.data[, Current.Credit.Balance       := as.numeric(Current.Credit.Balance)]
  sel.data[, Maximum.Open.Credit          := as.numeric(Maximum.Open.Credit)]   
  sel.data[, Bankruptcies                 := as.numeric(Bankruptcies)]         
  sel.data[, credit.score                 := as.numeric(credit.score)]     
  
  # remove one id
  sel.data[, customer.id := NULL]
  
  sel.data
}

# apply the data cleansing function
system.time(sel.data <- data.cleansing(copy(raw.data)))

# fix problem with credit.score >1000
sel.data[credit.score > 1000, credit.score := credit.score / 10]

if (0) {
  system.time(new.sel.data <- data.cleansing(copy(new.raw.data)))
  
  test1.data <- fread("data/attept.1.csv")
  
  new.sel.data[, old.rec := 0]
  new.sel.data[loan.id %in% sel.data$loan.id, old.rec := 1]
  
  new.sel.data[, test1.rec := 0]
  new.sel.data[loan.id %in% test1.data$`Loan ID`, test1.rec := 1]
  
  used.for.test <- new.sel.data[, test1.rec == 1]
}

# look at the data
summary(sel.data)

######################################################################################
# check with dummy model
if (1) {
  dummy.model <- function(d) {
    d[, status.pred := 1]
    d[credit.score > 1000, status.pred := 0]
    d
  }
  
  # apply dummy model
  dd <- dummy.model(raw.data)
  the.acc <- nrow(dd[status == status.pred])/nrow(dd)
  print(paste0("Dummy model on raw data: acc = ", round(100*the.acc, 1), "%"))
  
  dd <- dummy.model(sel.data)
  the.acc <- nrow(dd[status == status.pred])/nrow(dd)
  print(paste0("Dummy model on clean data: acc = ", round(100 * the.acc, 1), "%"))
  
  dd <- dummy.model(new.sel.data)
  the.acc <- nrow(dd[status == status.pred])/nrow(dd)
  print(paste0("Dummy model on big clean data: acc = ", round(100 * the.acc, 1), "%"))
  rm(dd)
  
  dd <- dummy.model(new.sel.data[old.rec == 0])
  the.acc <- nrow(dd[status == status.pred])/nrow(dd)
  print(paste0("Dummy model on big clean data (excluding real training dat): acc = ", round(100 * the.acc, 1), "%"))
  rm(dd)
}

######################################################################################
# save clean data
sel.data[, status.pred := NULL]
write.table(sel.data, file="data/sel.data.tsv", sep="\t", row.names = F)
