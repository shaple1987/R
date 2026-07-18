options(stringsAsFactors = F)
library(quantmod)
library(data.table)
library(readxl)
library(xts)
library(rvest)

HOLIDAY_CALENDAR <- read.delim("~/Hai/Code/R/global_config/holidays.txt", header = FALSE, colClasses = "Date")$V1

extract_div <- function(ticker, start_date = NULL,
                        src=c("divinfo","divhist")[2]){
  valid_srcs <- c("divhist")
  message(sprintf("Getting div history for %s from source %s", ticker, src
                  ))
  if(src=='divinfo'){ # no longer working as of Oct 20, 2025
    doc <- rvest::read_html(sprintf("https://www.dividendinformation.com/search_ticker/?identifier=%s", ticker))
    tables <- doc %>% rvest::html_elements("table") %>% rvest::html_table(header = TRUE)
    output <- tables[4] %>% as.data.frame
    output$Ex.Date <- as.Date(output$Date)
    output$Amount.Per.Share <- as.numeric(sub("\\$", "", output$Amount.Per.Share))
    output$Ticker <- ticker
    output$Type <- "CASH"
    output <- output[, c("Ticker", "Ex.Date", "Type", "Amount.Per.Share")]
  }else if(src=='divhist'){
    doc <- rvest::read_html(sprintf("https://dividendhistory.org/payout/%s/", ticker))
    tables <- doc %>% rvest::html_elements("table") %>% rvest::html_table(header = TRUE)
    found <- F
    idx<-0
    while(found==F){
      idx <- idx + 1
      first_col <- names(tables[idx][[1]])[1]
      if(first_col== "Ex-Dividend Date") found <- T
    }
    output <- tables[idx][[1]] %>% as.data.frame
    # remove estimated divs
    names(output)[length(names(output))] <- "pct_chg" # standardize column name
    output <- subset(output, pct_chg != "unconfirmed/estimated")
    output$Ticker <- ticker
    output$Amount.Per.Share <-as.numeric(sub("\\$", "", output$`Cash Amount`))
    output$Ex.Date <- as.Date(output$`Ex-Dividend Date`)
    output$Type <- 'CASH'
    output$Payment.Date <- as.Date(output$`Payout Date`)
    output <- output[,c("Ticker","Ex.Date","Type","Amount.Per.Share","Payment.Date")]
  }else{
    print(sprintf("error! %s is not a supported data source, pick one of: %s", src, paste(valid_srcs, collapse=', ')))
    return(NA)
  }
  if(!is.null(start_date)) output <- subset(output, Ex.Date >= start_date)
  print(sprintf("%d rows returned", nrow(output)))
  return(output)
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
    read_excel("~/Hai/inv_automation/Inv Workbook.xlsx",sheet = "Div History",range = "J1:R13"),
    read_excel("~/Hai/inv_automation/Inv Workbook.xlsx",sheet = "Div History",range = "S1:AA12")
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
