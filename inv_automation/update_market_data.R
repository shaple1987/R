rm(list=ls())
source("~/Hai/Spreadsheets/inv_automation/market_data_providers.R")

update.div <- F
update.px <- T

if(update.div){
  div_out <- update_div_history()
  write.csv(div_out, "~/Hai/Spreadsheets/inv_automation/market_data/div.csv",row.names=F)
}

if(update.px){
  dates_all <- c("2021-12-31","2022-01-31","2022-02-28","2022-03-31","2022-04-29","2022-05-31","2022-06-30","2022-07-29","2022-08-31","2022-09-30","2022-10-31","2022-11-30","2022-12-30","2023-01-31","2023-02-28","2023-03-31","2023-04-28","2023-05-31","2023-06-30","2023-07-31","2023-08-31","2023-09-29","2023-10-31","2023-11-30","2023-12-29","2024-01-31","2024-02-29","2024-03-29","2024-04-30","2024-05-31","2024-06-28","2024-07-31","2024-08-30","2024-09-30","2024-10-31")
  #px_out <- update_px_history(dates_all, last = NULL, tickers_override = "DOCN")
  px_out <- update_px_history(dates_all, last = 3)
  write.csv(px_out,"~/Hai/Spreadsheets/inv_automation/market_data/stock_price.csv",row.names=F)
}

#sdate <- as.Date("2017-01-01")
#edate <- as.Date("2023-05-31")
#out <- get_prices_by_date_range(tickers, sdate, edate)
#saveRDS(out, "~/Hai/Spreadsheets/inv_automation/market_data/stock_price.rds")
