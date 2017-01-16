library(data.table)

all.sel.data <- fread("data/sel.data.tsv")

the.sample <- all.sel.data[1:10]
the.sample[, customer.id := loan.id]

setnames(the.sample, "loan.id", "Loan ID")
setnames(the.sample, "customer.id", "Customer ID")
setnames(the.sample, "status.str", "Loan Status")
setnames(the.sample, "amount", "Current Loan Amount")
setnames(the.sample, "credit.score", "Credit Score")
setnames(the.sample, "job.years", "Years in current job")
setnames(the.sample, "Home.Ownership", "Home Ownership")
setnames(the.sample, "income", "Annual Income")
setnames(the.sample, "debt", "Monthly Debt")
setnames(the.sample, "Years.of.Credit.History", "Years of Credit History")
setnames(the.sample, "Months.since.last.delinquent", "Months since last delinquent")
setnames(the.sample, "Number.of.Open.Accounts", "Number of Open Accounts")
setnames(the.sample, "Number.of.Credit.Problems", "Number of Credit Problems")
setnames(the.sample, "Current.Credit.Balance", "Current Credit Balance")
setnames(the.sample, "Maximum.Open.Credit", "Maximum Open Credit")
setnames(the.sample, "Tax.Liens", "Tax Liens")


official.names <- c("Loan ID"
                    , "Customer ID"
                    , "Loan Status"
                    , "Current Loan Amount"
                    , "Term"
                    , "Credit Score"
                    , "Years in current job"
                    , "Home Ownership"
                    , "Annual Income"
                    , "Purpose"
                    , "Monthly Debt"
                    , "Years of Credit History"
                    , "Months since last delinquent"
                    , "Number of Open Accounts"
                    , "Number of Credit Problems"
                    , "Current Credit Balance"
                    , "Maximum Open Credit"
                    , "Bankruptcies"
                    , "Tax Liens")
                    
the.sample[, `Monthly Debt` := as.character(`Monthly Debt`)]
the.sample[, `Years in current job` := as.character(`Years in current job`)]
the.sample[, Bankruptcies := as.character(Bankruptcies)]
the.sample[, `Tax Liens` := as.character(`Tax Liens`)]
the.sample[, `Maximum Open Credit` := as.character(`Maximum Open Credit`)]

           
the.sample[, `Monthly Debt` := "chr"]
the.sample[, `Years in current job` := "chr"]
the.sample[, Bankruptcies := "chr"]
the.sample[, `Tax Liens` := "chr"]
the.sample[, `Maximum Open Credit` := "chr"]

the.sample <- the.sample[, official.names, with=F]              
write.table(the.sample, "data/schema.sample5.tsv", sep="\t", row.names = F)
