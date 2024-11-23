source("~/Hai/Code/R/inv_automation/inv_automation_core.R")
local_config_excluded_tickers <- "~/Hai/inv_automation/local_config/excluded_tickers.csv"

pivot_driver <- function(pos, as_of = Sys.Date(), excl_tickers = F, console_printout = T, view_pos_only = F){
  
  obj <- list()
  message(sprintf("Main dashboard (as of %s)", as_of))
  obj$pos <- calc_pos_as_of(pos, as_of)
  obj$optops <- calc_pos_as_of(optpos, as_of)
  if(nrow(obj$optops)==0){
    print(sprintf("No option position as of %s", as_of))
    obj$optops <- NULL
  }
  dashboard <- create.main.dashboard(obj$pos, div, px, as_of)
  if(excl_tickers){
    excluded_tickers <- read.csv(local_config_excluded_tickers)$Ticker
    dashboard <- subset(dashboard, !Ticker %in% excluded_tickers)
  }
  rm(pos)
  
  not_covered <- setdiff(dashboard$Ticker, char$Ticker)
  if(length(not_covered)) stop(sprintf("These tickers are not covered: %s", paste(not_covered, collapse=", ")))
  dashboard <- merge(dashboard, char, by="Ticker")
  
  message("Active position summary")
  pos_a <- subset(obj$pos, Active == 1)
  out <- pos_a %>% group_by(Account, Ticker) %>% dplyr::summarise(TotalPos = sum(NumShares))
  out <- dcast(out, Ticker~Account, value.var='TotalPos')
  out$Total <- rowSums(out[,-1],na.rm = T)
  obj$pos_summary <- out
  rm(pos_a, out)
  if(view_pos_only){
    View(obj$pos_summary)
    browser()
  }
  
  message("Dividend summary")
  obj$dividend_summary <- div_summary_enriched(dashboard)
  
  message("Pivots")
  obj$z0<-inv_pivot(dashboard, "TotalPortfolio", optpos = obj$optops)
  if(console_printout) print(obj$z0)
  obj$z_acct<-inv_pivot(dashboard, "Account", optpos = obj$optops)
  obj$z_accttype<-inv_pivot(dashboard, "AccountType", optpos = obj$optops)
  if(console_printout) print(obj$z_accttype)
  obj$z_ticker<-inv_pivot(dashboard, "Ticker", optpos = obj$optops)
  obj$z6<-inv_pivot(dashboard, "VintageYear", optpos = obj$optops)
  if(console_printout) print(obj$z6 %>% subset(VintageYear>=2022))
  #z6b<-inv_pivot(dashboard, "VintageMonth", optpos = optpos)
  #z6b2<-subset(z6b, VintageMonth>="202201")
  obj$z7<-inv_pivot(dashboard, "ClosedYear", optpos = obj$optops)
  #obj$z7n<-inv_pivot(subset(dashboard,Account %in% NON_RETIRE_ACCTS), "ClosedYear", optpos = optpos)
  #obj$z7r<-inv_pivot(subset(dashboard,Account %in% RETIRE_ACCTS), "ClosedYear", optpos = optpos)
  obj$z8<-inv_pivot(dashboard, "Style1", optpos = obj$optops)
  obj$z8g<-inv_pivot(dashboard, "Style1_Granular", optpos = obj$optops)
  obj$z_program<-inv_pivot(dashboard, "Program", optpos = obj$optops)

  obj$z_recent_sale <- obj$z_ticker %>% subset(Ticker %in% subset(obj$pos,SoldDate>=as_of-14 & Ticker!="USTreasury")$Ticker)
  if(console_printout){
    message("Tickers with sale within 14 days")
    print(obj$z_recent_sale)
  }

  obj$t_holding <- subset(obj$pos, Ticker=='USTreasury') %>% arrange(SoldDate)
  if(console_printout){
    message("Treasury holdings")
    print(obj$t_holding)
  }
  
  obj$dashboard <- dashboard
  
  return(obj)
  
  # PNL decomp by month (not quite working now)
  
  # zz0<-pnl.decomp(pos, div, px, as.Date("2022-01-01"), as.Date("2022-01-31"))
  # zz<-pnl.decomp(pos, div, px, as.Date("2022-02-01"), as.Date("2022-02-28"))
  # zz2<-pnl.decomp(pos, div, px, as.Date("2022-03-01"), as.Date("2022-03-31"))
  # zz3<-pnl.decomp(pos, div, px, as.Date("2022-01-01"), as.Date("2022-04-29"))
  # zz4<-pnl.decomp(pos, div, px, as.Date("2022-04-01"), as.Date("2022-04-29"))
  
  
}

format_output <- function(allobjs, element){
  as.data.frame(rbindlist(lapply(allobjs,function(x){
    out <- x[[element]]
    temp <- names(out)
    out$date <- x$date
    out[, c("date", temp)]
  })))
}

if(F){
  dates_all<-c("2021-12-31","2022-01-31","2022-02-28","2022-03-31","2022-04-29","2022-05-31","2022-06-30","2022-07-29","2022-08-31","2022-09-30","2022-10-31","2022-11-30","2022-12-30","2023-01-31","2023-02-28","2023-03-31","2023-04-28","2023-05-31","2023-06-30","2023-07-31","2023-08-31","2023-09-29","2023-10-31","2023-11-30","2023-12-29","2024-01-31","2024-02-29","2024-03-29","2024-04-30","2024-05-31","2024-06-28","2024-07-31")
  allobjs <- lapply(as.list(dates_all), function(x){
    out <- pivot_driver(pos, as.Date(x), view_pos_only = F, console_printout = F)
    out$date <- as.Date(x)
    out
  })
  zz <- format_output(allobjs, "z0")
  zz1 <- format_output(allobjs, "z_ticker")
  write.csv(zz,sprintf("~/Hai/inv_automation/monthly_summary_%s2.csv", dates_all[length(dates_all)]),row.names=F)
  write.csv(zz1,sprintf("~/Hai/inv_automation/monthly_summary_by_ticker%s2.csv", dates_all[length(dates_all)]),row.names=F)
}

obj1 <- pivot_driver(pos, view_pos_only = F)
#obj2 <- pivot_driver(pos, as_of = as.Date("2023-08-31"))

#print(rbind(obj1$z0, obj2$z0))