options(stringsAsFactors = F)
library(quantmod)
library(data.table)
library(readxl)
library(xts)
library(rvest)

HOLIDAY_CALENDAR <- read.delim("~/Hai/Code/R/global_config/holidays.txt", header = FALSE, colClasses = "Date")$V1

extract_div <- function(ticker, start_date = NULL){
  message(sprintf("Getting div history for %s", ticker))
  doc <- rvest::read_html(sprintf("https://www.dividendinformation.com/search_ticker/?identifier=%s", ticker))
  tables <- doc %>% rvest::html_elements("table") %>% rvest::html_table(header = TRUE)
  output <- tables[4] %>% as.data.frame
  output$Date <- as.Date(output$Date)
  output$Amount.Per.Share <- as.numeric(sub("\\$", "", output$Amount.Per.Share))
  output$Ticker <- ticker
  output$Type <- "CASH"
  if(!is.null(start_date)) output <- subset(output, Date >= start_date)
  print(sprintf("%d rows returned", nrow(output)))
  return(output[, c("Ticker", "Date", "Type", "Amount.Per.Share")])
}

get_prices_by_date<-function(tickers, sdate){
  out<-data.frame(ticker=tickers,date=sdate,close=NA)
  
  for(i in 1:length(tickers)){
    tic<-tickers[i]
    ss_stock<-tryCatch(getSymbols(tic,from=sdate,to=sdate+5,auto.assign = T),error=function(e)NA)
    out$close[i]<-tryCatch(get(tic)[1,paste0(tic,".Close")],error=function(e){
      print(sprintf("Cannot get price for %s", tic))
      return(NA)
    }
    )
  }
  
  out$close<-round(out$close,digits = 2)
  out<-subset(out,!is.na(close))
  
}

get_prices_by_date_range<-function(tickers, sdate, edate){
  out<-data.frame()
  
  for(i in 1:length(tickers)){
    tic<-tickers[i]
    ss_stock<-tryCatch(getSymbols(tic,from=sdate,to=edate+5,auto.assign = T),error=function(e)NA)
    obj <- get(tic)
    dates<-tryCatch(index(obj),error=function(e){
      print(sprintf("Cannot get price for %s", tic))
      return(NA)
    })
    close<-tryCatch(obj[,paste0(tic,".Close")],error=function(e){
      print(sprintf("Cannot get price for %s", tic))
      return(NA)
    })
    temp<-data.frame(ticker=tic, date = as.Date(dates), close=as.numeric(close))
    out<-rbind(out,temp)
  }
  
  out$close <- round(out$close, digits = 2)
  out <- subset(out,!is.na(close))
  out <- subset(out, date >= sdate & date <= edate)
  out
}

## Driver funcs

update_div_history <- function(){
  # Load list of div stocks
  div_list<-rbind(
    read_excel("~/Hai/Spreadsheets/inv_automation/Inv Workbook.xlsx",sheet = "Div History",range = "J1:O13"),
    read_excel("~/Hai/Spreadsheets/inv_automation/Inv Workbook.xlsx",sheet = "Div History",range = "S1:X12")
  )
  tickers<-div_list$Ticker
  excl_tickers<-c("UBA","GIGE")
  tickers<-setdiff(tickers,excl_tickers)
  div_list <- subset(div_list, Ticker %in% tickers)
  div_list$`Last Dividend` <- as.Date(div_list$`Last Dividend`)
  print(div_list)
  
  N <- nrow(div_list)
  out <- vector("list", N)
  for (i in 1:N){
    out[[i]] <- tryCatch(extract_div(div_list$Ticker[i], div_list$`Last Dividend`[i] + 1),
                         error = function(e){print("error");NULL}
                         )
  }
  as.data.frame(rbindlist(out))
}

update_px_history <- function(dates, tickers){
  out<-data.frame()
  for(date in dates){
    print(sprintf("Getting prices for %s", as.Date(date)))
    out<-rbind(out,get_prices_by_date(tickers, as.Date(date)))
  }
  out
}

get_month_end_dates <- function(start = as.Date("2021-12-31"), end = as.Date(paste0(format(Sys.Date(),"%Y-%m"),"-01"))-1, weekends = FALSE, holidays =  HOLIDAY_CALENDAR){
  dates <- seq.Date(start, end, by = 1)
  dates <- dates[!dates %in% holidays]
  if(!weekends) dates <- dates[!weekdays(dates,abbreviate =  T) %in% c("Sat","Sun")]
  out <- sapply(split(dates, format(dates, "%Y%m")), function(x) x[length(x)])
  out <- as.Date(as.numeric(out), origin = '1970-01-01')
  out
}
