calc_beta <- function(data, ticker1, ticker2, lookback = 252){
  data1 <- subset(data, ticker == ticker1)
  data2 <- subset(data, ticker == ticker2)
  merged <- merge(data1, data2, by = "date")
  merged # temp, more TODO
}

out <- calc_beta(out, "AYX", "SPY")