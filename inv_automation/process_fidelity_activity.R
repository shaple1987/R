fn <- "C:/Users/where/Downloads/History_for_Account_224484923.csv"
#fn <- "C:/Users/where/Downloads/Accounts_History.csv"
AccountShortManual <- "LY Trad"
dt1 <- readLines(fn)
idx <- grep("^ [:0-9:]{2}/", dt1)
dt1<-dt1[c(min(idx)-1,idx)]
fileConn<-file(fn)
writeLines(dt1, fileConn)
close(fileConn)

dt <- read.csv(fn, stringsAsFactors = F)

#Ticker	Account	Active	NumShares	AcqDate	CostBasisPerShare

account_map<-data.frame(Account=c("Traditional IRA 224484923","Joint WROS - TOD X73998402","ROTH IRA 227228937"),
                        Account_Short=c("LY Trad","Joint","Roth"))


dt$AcqDate <- as.Date(dt$Run.Date,"%m/%d/%Y")
idx <- setdiff(grep("^ REINVESTMENT", dt$Action), grep("^ REINVESTMENT CASH", dt$Action))
idx <- c(idx, grep("^ YOU BOUGHT", dt$Action))
dt <- dt[idx,]
dt$Symbol <- trimws(dt$Symbol)
if(AccountShortManual != ""){
  browser()
  dt$AccountShort <- AccountShortManual
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

#write.csv(dt2, "~/Hai/Spreadsheets/inv_automation/fidelity_activity.csv",row.names=F)