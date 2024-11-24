fn <- "C:/Users/where/Downloads/Accounts_History (1).csv" # activities downloaded from Fidelity
local_config_account_map <- "~/Hai/inv_automation/local_config/account_map.csv"

dt1 <- readLines(fn)
idx <- grep("^ [:0-9:]{2}/", dt1)
dt1<-dt1[c(min(idx)-1,idx)]
fn_processed <- sub(".csv$", "_processed.csv", fn)
fileConn<-file(fn_processed)
writeLines(dt1, fileConn)
close(fileConn)

dt <- read.csv(fn_processed, stringsAsFactors = F)

#Ticker	Account	Active	NumShares	AcqDate	CostBasisPerShare

account_map<-read.csv(local_config_account_map)

dt$AcqDate <- as.Date(dt$Run.Date,"%m/%d/%Y")
idx <- setdiff(grep("^ REINVESTMENT", dt$Action), grep("^ REINVESTMENT CASH", dt$Action))
idx <- c(idx, grep("^ YOU BOUGHT", dt$Action))
dt <- dt[idx,]
dt$Symbol <- trimws(dt$Symbol)
if(!"Account" %in% names(dt)){
  dt$AccountShort <- readline(prompt = sprintf("Enter the account info manually. Choices (%s) ", paste(account_map$Account_Short, collapse = ", ")))
}else{
  dt$AccountShort <- account_map$Account_Short[match(dt$Account,account_map$Account)]
}
dt$Active <- 1
dt$CostBasisPerShare <- round(-dt$Amount..../dt$Quantity, 6)
dt2 <- dt[, c("Symbol", "AccountShort", "Active", "Quantity", "AcqDate", "CostBasisPerShare")]

# post processing
symbols_to_excl <- "SPAXX"
dt2 <- subset(dt2, !Symbol %in% symbols_to_excl)
treasury_symbols <- grep("^91282", dt2$Symbol)
if(length(treasury_symbols)){
  dt2$Symbol[treasury_symbols] <- "USTreasury"
  dt2$Quantity[treasury_symbols] <- dt2$Quantity[treasury_symbols]/100
  dt2$CostBasisPerShare[treasury_symbols] <- dt2$CostBasisPerShare[treasury_symbols]*100  
}

write.csv(dt2, "~/Hai/inv_automation/fidelity_activity.csv",row.names=F)