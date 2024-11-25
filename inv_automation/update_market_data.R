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
  # note: this list has taken into account weekends and holidays
  # so far cannot be replicated by get_month_end_dates()
  dates_all <- c("2021-12-31","2022-01-31","2022-02-28","2022-03-31","2022-04-29","2022-05-31","2022-06-30","2022-07-29","2022-08-31","2022-09-30","2022-10-31","2022-11-30","2022-12-30","2023-01-31","2023-02-28","2023-03-31","2023-04-28","2023-05-31","2023-06-30","2023-07-31","2023-08-31","2023-09-29","2023-10-31","2023-11-30","2023-12-29","2024-01-31","2024-02-29","2024-03-29","2024-04-30","2024-05-31","2024-06-28","2024-07-31","2024-08-30","2024-09-30","2024-10-31")
  
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
