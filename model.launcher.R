library(data.table)
library(e1071)

# testing in R-studio if TRUE
do.test <- F

if (do.test) {
  raw.data <- copy(attempt.data)
  # raw.data <- fread("data/schema.sample5.tsv")
  load("Results/MLM_small.credit.score_v2.RData")
} else {
  raw.data <- as.data.table(maml.mapInputPort(1))
  load("src/MLM_small.credit.score_v2.RData")
}

# prepare models
the.bad.data.model <- the.model[['the.model.bad.data']]
the.bad.data.cutoff <- the.model[['the.cutoff.bad.data']]
the.good.data.model <- the.model[['the.model.good.data']] 
the.good.data.cutoff <- the.model[['the.cutoff.good.data']]

# prepare functions
data.cleansing <- the.model[['data.cleansing']]
feature.engineering <- the.model[['feature.engineering']]
model.launcher <- the.model[['model.launcher']]
rename.raw.data <- the.model[['rename.raw.data']]

# do modelling
system.time(the.answer <- model.launcher(raw.data = raw.data
                             , m1 = the.good.data.model
                             , m2 = the.bad.data.model
                             , cutoff.1 = the.good.data.cutoff
                             , cutoff.2 = the.bad.data.cutoff
                             , verbose = 2))
the.answer <- the.answer[!duplicated(loan.id)]

# return data.frame with the same number of rows as the input one
full.answer <- merge(raw.data[, .(`Loan ID`, `Annual Income`, `Current Loan Amount`, `Credit Score`)], the.answer, by.x="Loan ID", by.y="loan.id", all.x=T)
full.answer[is.na(status.predicted), status.predicted := "Fully Paid"]

if (do.test) {
  test.answer <- merge(raw.data[, .(`Loan ID`, `Loan Status`)], the.answer, by.x="Loan ID", by.y="loan.id", all.x=T)
  setnames(test.answer, "Loan Status", "status.real")
  nrow(test.answer[status.real == status.predicted]) / nrow(full.answer)
}

# FOR TESTS ONLY!!!
#full.answer[, status.predicted := 
#  paste(as.character(`Annual Income`), "/", as.character(`Current Loan Amount`), "/", as.character(`Credit Score`), "/", status.predicted)]

required.answer <- full.answer[, .(`Loan ID`, status.predicted)]
setnames(required.answer, "status.predicted", "Status_Pred")

if (!do.test) {
  maml.mapOutputPort("required.answer")
}
