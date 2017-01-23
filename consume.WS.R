library(AzureML)
library(data.table)
library(compare)
#source("core.consume.R")

Sys.setenv(TZ="Europe/London")
verbose <- 10

############################################################################################################
# WS configuration
if (1) {
  # Workspace name
  WSPname <- "sherstnv-Free-Workspace"
  # Workspace ID
  WSPid <- "29d0b6a2db4848bf84c459f540db0a27"
  # Workspace Primary Token
  WSPauth <- "2b6c9dcd50624bacb82fadfa6a89539c"  # from Workspace Configuration 
  # Working WS name
  ws.name <- "Loan Granting Binary Classification December 2016 [Predictive Exp.]"
  
  # discover all available web services
  ws <- workspace(WSPid, WSPauth)
  webservices <- getWebServices(ws)
  
  # select the WS
  the.ws <- webservices[webservices$Name == ws.name, ]
  if (0 < verbose) {
    print("The WS definition:")
    print(the.ws)
  }
  
  # select endpoint
  eps <- endpoints(ws, the.ws[1, ])
  ep <- eps[eps$Name == "default", ]
}

############################################################################################################
# testing with attempted data
if (1) {
  attempt.output <- fread("data/attempt2.csv")
  attempt.ids <- unique(attempt.output$`Loan ID`)
  
  # prepare input data
  raw.data.all <- fread("data/big.LoansTrainingSetV2.csv", na.strings=c("NA","N/A","null", "#VALUE!"))
  raw.data.all[, `Months since last delinquent` := as.character(`Months since last delinquent`)]
  raw.data.all[, `Maximum Open Credit` := as.character(`Maximum Open Credit`)]
  raw.data.all[, `Tax Liens` := as.character(`Tax Liens`)]
  raw.data.all[, Bankruptcies := as.character(Bankruptcies)]
  
  attempt.data <- raw.data.all[`Loan ID` %in% attempt.ids]
  write.table(attempt.data, file="data/attempt.data.csv", sep = ",", row.names = F)
  write.table(attempt.data[1:100], file="data/attempt.data.small.comma.csv", sep = ",", row.names = F)
  
  # all at once
  system.time(the.response <- as.data.table(consume(ep, attempt.data)))
  
  the.response.shrinked <- unique(the.response)
  
  
  mysplit <- function(s) {
    b <- strsplit(s, " / ")[[1]]
    income <- b[1]
    amount <- b[2]
    credit.score <- b[3]
    pred.status <- b[4]
    list(income, amount, credit.score, pred.status)
  }
  
  the.response.shrinked$Status_Pred[1]
  
  enriched.response <- fread("data/98a8751f-1a70-49cd-9f39-26f3454c5c2b.csv")
  enriched.response <- unique(enriched.response)
  enriched.response[, c("income", "amount", "credit.score", "pred.status") := mysplit(Status_Pred), .(`Loan ID`)]
  enriched.response[, income := as.numeric(income)]
  enriched.response[, amount := as.numeric(amount)]
  enriched.response[, credit.score := as.numeric(credit.score)]
  
  enriched.response.1 <- merge(enriched.response, attempt.data[, .(`Loan ID`, `Annual Income`, `Credit Score`, `Current Loan Amount`)], by="Loan ID")
  
  # one by one
  if (0) {
    the.response2 <- {}
    the.ids <- unique(attempt.data$`Loan ID`)
    for(the.id in the.ids) {
      print(paste0("Try ", the.id))
      d <- attempt.data[`Loan ID` == the.id]
      d[is.na(`Months since last delinquent`), `Months since last delinquent` := "NA"]
      d[is.na(Bankruptcies), Bankruptcies := "NA"]
      d[is.na(`Tax Liens`), `Tax Liens` := "NA"]
      #d[is.na(`Annual Income`), `Annual Income` := NA]
      #d[is.na(`Credit Score`), `Credit Score` := NA]
      r <- as.data.table(consume(ep, d))
      the.response2 <- rbind(the.response, r)
    }
  }
  
  # merge actual and predicted status
  response.eval <- copy(the.response)
  for (the.id in unique(response.eval$Loan.ID)) {
    the.status <- unique(attempt.data$`Loan Status`[attempt.data$`Loan ID` == the.id])
    if (1 != length(the.status))
      print(the.status)
    response.eval[Loan.ID == the.id, Status_Real := the.status]
  }
  
  # calculate accuracy
  nrow(response.eval[Status_Pred == Status_Real]) / nrow(response.eval)
}

############################################################################################################
# testing with non-training data
if (1) {
  raw.data.limited <- fread("data/LoansTrainingSetV2.csv", na.strings=c("NA","N/A","null", "#VALUE!"))
  raw.data.all <- fread("data/big.LoansTrainingSetV2.csv", na.strings=c("NA","N/A","null", "#VALUE!"))
  
  limited.unique.id <- unique(raw.data.limited$`Loan ID`)
  all.unique.id <- unique(raw.data.all$`Loan ID`)
  unused.id <- all.unique.id[!all.unique.id %in% limited.unique.id]
  used.id <- all.unique.id[all.unique.id %in% limited.unique.id]
  raw.data.test <- raw.data.all[`Loan ID` %in% unused.id]
  
  system.time(the.response <- as.data.table(consume(ep, raw.data.test)))

  
  # merge actual and predicted status
  response.eval <- copy(the.response)
  response.eval[, Status_Real := "Fully Paid"]
  for (the.id in unique(response.eval$Loan.ID)) {
    the.status <- unique(raw.data.test$`Loan Status`[raw.data.test$`Loan ID` == the.id])
    if (1 != length(the.status))
      print(the.status)
    response.eval[Loan.ID == the.id, Status_Real := the.status]
  }
  
  # calculate accuracy
  nrow(response.eval[Status_Pred == Status_Real]) / nrow(response.eval)
}