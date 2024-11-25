rm(list=ls())
source("~/Hai/Code/R/personal_finance/util.R")
source("~/Hai/Code/R/inv_automation/market_data_providers.R")

calc_beta <- function(ticker1, ticker2, as_of = Sys.Date(), lookback = 252,
                      price_data_file = "~/Hai/inv_automation/market_data/stock_price_daily.rds"){

  data_required <- data.frame(ticker = c(ticker1, ticker2),
                              start = as_of - round(lookback*7/5)-30, 
                              end = as_of)
    
  # checking whether new data download is required
  px <- readRDS(price_data_file)
  px <- subset(px, ticker %in% data_required$ticker)
  start_end_by_ticker <- px %>% group_by(ticker) %>% summarise(
    min_date = min(date),
    max_date = max(date)
  )
  data_required <- merge(data_required, start_end_by_ticker, by = "ticker", all.x = T)
  for (i in 1:nrow(data_required)){
    this_ticker <- data_required$ticker[i]
    if(is.na(data_required$min_date[i]) || data_required$start[i] < data_required$min_date[i] || data_required$end[i] > data_required$max_date[i]){
      px_out_new <- get_prices_by_date_range(this_ticker, data_required$start[i], data_required$end[i])
      savedata(px_out_new,Rfile=price_data_file,indvars=c("ticker","date"), RDS = T, saveFlat = F)
    }
  }
  
  px <- readRDS(price_data_file)
  
  data1 <- tail(subset(px, ticker == ticker1), lookback)
  data2 <- tail(subset(px, ticker == ticker2), lookback)
  merged <- merge(data1, data2, by = "date", suffixes = c("1", "2"))
  merged <- merged[order(merged$date), ]
  N <- nrow(merged)
  merged$logret1 <- c(NA, log(merged$close1[2:N]/merged$close1[1:(N-1)]))
  merged$logret2 <- c(NA, log(merged$close2[2:N]/merged$close2[1:(N-1)]))
  merged <- subset(merged, !is.na(logret1) & !is.na(logret2))
  vol1 <- sd(merged$logret1,na.rm=T)
  vol2 <- sd(merged$logret2,na.rm=T)
  corr <- cor(merged$logret1, merged$logret2)
  obj<-summary(lm(logret1~logret2,data=merged))
  obj0<-summary(lm(logret1~logret2+0,data=merged))
  out <- list(ticker1 = ticker1, ticker2 = ticker2, daily_vol1 = round(vol1, 4), daily_vol2 = round(vol2, 4),
              twoweek_vol1 = round(vol1*sqrt(10), 4), twoweek_vol2 = round(vol2*sqrt(10), 4),
              corr_1vs2 = round(corr, 4),
              beta_1vs2 = round(obj$coefficients[2,1], 4),
              beta_1vs2_nointercept = round(obj0$coefficients[1,1], 4)
  )
  print(as.data.frame(out))
  merged
}

# test
if(F){
  out <- calc_beta(ticker1 = "CMCSA", ticker2 = "SPY")
  out2 <- calc_beta(ticker1 = "PATH", ticker2 = "SPY")
  out3 <- calc_beta(ticker1 = "DOCN", ticker2 = "SPY")
  out4 <- calc_beta(ticker1 = "BLK", ticker2 = "SPY")
  out5 <- calc_beta(ticker1 = "TERN", ticker2 = "SPY")
  out6 <- calc_beta(ticker1 = "BAC", ticker2 = "SPY")
  out7 <- calc_beta(ticker1 = "SOFI", ticker2 = "SPY")  
}
