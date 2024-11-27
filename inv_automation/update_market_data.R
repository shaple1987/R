rm(list=ls())
source("~/Hai/Code/R/inv_automation/market_data_providers.R")
fn <- "~/Hai/inv_automation/Inv Workbook.xlsx"

update.div <- F
update.px <- T

if(update.div){
  div_out <- update_div_history()
  write.csv(div_out, "~/Hai/inv_automation/market_data/div.csv",row.names=F)
}

if(update.px){
  dates_all <- get_month_end_dates()

  # Load tickers
  tickers<-read_excel(fn,sheet = "Positions",range = "A1:A1000")$Ticker
  tickers<-na.omit(unique(tickers))
  excl_tickers<-c("WORK","USTreasury","UBA","GIGE","PTRAQ","AYX")
  tickers<-setdiff(tickers,excl_tickers)

  message("Loding existing price time series")
  px <- read_xlsx(fn,sheet = "Price", range = "B1:D10000")
  px <- subset(px, !is.na(Ticker))
  px$Date <- as.Date(px$Date)
  new_tickers <- setdiff(tickers, px$Ticker)
  existing_tickers <- setdiff(tickers, new_tickers)
  new_dates <- setdiff(as.Date(dates_all), px$Date)
  new_dates <- as.Date(new_dates, origin = '1970-01-01')
  
  if(length(new_dates)){
    print(sprintf("Getting new price data the existing tickers on these dates: %s", paste(new_dates, collapse = ", ")))
    px_out <- update_px_history(new_dates, tickers = existing_tickers)
  }else{
    px_out <- data.frame()
  }
    
  if(length(new_tickers)){
    print(sprintf("Getting entire time series for these new tickers: %s", paste(new_tickers, collapse = ", ")))
    px_out_new <- update_px_history(dates_all, tickers = new_tickers)
    px_out <- rbind(px_out, px_out_new)
  }
  
  if(nrow(px_out)){
    write.csv(px_out,"~/Hai/inv_automation/market_data/stock_price.csv",row.names=F)
  }else{
    print("No new price data to output")
  }  
}
