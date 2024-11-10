library(readxl)
library(dplyr)
library(data.table)
library(reshape2)
RETIRE_ACCTS<-c("LY Trad","Roth")
NON_RETIRE_ACCTS<-c("LY Ally","Joint","ESPP","RSU_BLK","LY Ind")
fn <- "~/Hai/inv_automation/Inv Workbook.xlsx"


pos_expand <- function(pos,offset=1){
  # use offset=1 for calculating dividend eligibility
  #https://www.investopedia.com/articles/02/110802.asp
  pos<-subset(pos,AcqDate<=SoldDate-offset)
  out<-vector("list", nrow(pos))
  for(i in 1:nrow(pos)){
    out[[i]]<-data.frame(ID=pos$ID[i],Ticker=pos$Ticker[i],Account=pos$Account[i],NumShares=pos$NumShares[i],
                         Date=seq.Date(pos$AcqDate[i]+offset,pos$SoldDate[i],by=1))
  }
  as.data.frame(rbindlist(out))
}

calc.div.by.pos <- function(pos, div, by = c("month", "year")[1]){
  if(by=="month") date_format<-"%Y%m"
  else if (by=="year") date_format<-"%Y"
  else stop("By col not supported")
  
  pos_e <- pos_expand(pos)
  out <- merge(pos_e, div, by.x = c("Ticker", "Date"), by.y=c("Ticker","ExDate"), all.y = T)
  out <- subset(out, !is.na(ID))
  out$groupbycol <- format(out$Date, date_format)
  out2 <- out %>% group_by(ID, groupbycol) %>% dplyr::summarise(TotalDiv = sum(`CASH AMOUNT`))
  out3<-dcast(out2, ID~groupbycol, value.var="TotalDiv", FUN=sum)
  names(out3)[-1] <- paste0("Div", names(out3)[-1]) 
  pos <- merge(pos, out3, by="ID", all.x = T)
  div_cols <- grep("^Div", names(pos))
  for(col in div_cols){
    pos[is.na(pos[,col]),col] <- 0
  }
  pos
}

fill.sold.price.for.active.pos <- function(pos, as.of, px){
  pos$SoldDate[pos$Active==1] <- as.of
  px <- subset(px, Date <= as.of)
  max_px_date <- sapply(split(px$Date, px$Ticker), max)
  px_current <- data.frame(Ticker=names(max_px_date),
                           Date=as.Date(max_px_date, origin='1970-01-01'))
  px_current <- merge(px_current, px, by=c("Ticker", "Date"), all.x=T)
  idx_active_non_treasury<-which(pos$Active==1 & pos$Ticker!="USTreasury")
  pos$SoldPricePerShare[idx_active_non_treasury] <- px_current$Price[match(pos$Ticker[idx_active_non_treasury], px_current$Ticker)]
  # treasury: held at cost
  idx_active_treasury<-which(pos$Active==1 & pos$Ticker=="USTreasury")
  pos$SoldPricePerShare[idx_active_treasury] <- pos$CostBasisPerShare[idx_active_treasury]
  pos  
}

create.main.dashboard <- function(pos, div, px, as.of = Sys.Date()){
  
  # fill in price info for active positions
  pos <- pos %>% fill.sold.price.for.active.pos(as.of, px)
  # calculate dividend
  dashboard <- calc.div.by.pos(pos, div, by = "year")
  div_cols <- grep("^Div", names(dashboard))
  if(length(div_cols) == 0) dashboard$TotalDiv <- 0
  else if (length(div_cols) == 1) dashboard$TotalDiv <- dashboard[, div_cols[1]]
  else dashboard$TotalDiv <- apply(dashboard[,grep("^Div", names(dashboard))], 1, sum)
  
  dashboard$TotalCostBasis <- dashboard$CostBasisPerShare * dashboard$NumShares
  dashboard$TotalPNL <- (dashboard$SoldPricePerShare + dashboard$TotalDiv - dashboard$CostBasisPerShare) * dashboard$NumShares
  # Div (TTM)
  # Div Yield (Cost Basis, TTM)
  # Div Yield (Current Px, TTM)
  # Target Yield Implied Price
  dashboard$PctPNL <- dashboard$TotalPNL / dashboard$TotalCostBasis
  dashboard$PctDivYield <- dashboard$TotalDiv / dashboard$CostBasisPerShare
  dashboard$PctCapGain <- dashboard$SoldPricePerShare / dashboard$CostBasisPerShare - 1
  dashboard$AnnualizedReturn <- (dashboard$PctPNL + 1)^(1/as.numeric(dashboard$SoldDate-dashboard$AcqDate)*365) - 1
  dashboard
}


pnl.decomp <- function(pos, div, px, start, end){
  # determine price date
  start_px_date <- max(px$Date[px$Date<start])
  end_px_date <- max(px$Date[px$Date<=end])
  px_start <- subset(px, Date == start_px_date)
  px_end <- subset(px, Date == end_px_date)
  print(sprintf("Start of period price date: %s, end of period price date: %s ", start_px_date, end_px_date))
  
  # start of period pos
  pos_s <- subset(pos, AcqDate < start & (SoldDate >= start | is.na(SoldDate)))
  pos_s$AcqDate <- start - 1
  pos_s$CostBasisPerShare <- px_start$Price[match(pos_s$Ticker, px_start$Ticker)]
  pos_s <- pos_s %>% fill.sold.price.for.active.pos(end, px)
  browser()
  pos_s <- create.main.dashboard(pos_s, div, px)
  
  # end of period pos
  pos_e <- subset(pos, AcqDate <= end & (SoldDate >= start | is.na(SoldDate)))
  
  # position categories
  new_pos <- which(pos_e$AcqDate>=start & pos_e$SoldDate>end)
  closed_pos_old <- which(pos_e$AcqDate<start & pos_e$SoldDate<=end)
  closed_pos_new <- which(pos_e$AcqDate>=start & pos_e$SoldDate<=end)
  unchanged_pos <- setdiff(1:nrow(pos_e), c(new_pos, closed_pos_old, closed_pos_new))
  unchanged_non_treasury_pos <- base::intersect(unchanged_pos, which(pos_e$Ticker != 'USTreasury'))
  
  if(length(unchanged_pos)){
    pos_e$AcqDate[unchanged_pos] <- start - 1
    if(length(unchanged_non_treasury_pos)){
      pos_e$CostBasisPerShare[unchanged_non_treasury_pos] <- px_start$Price[match(pos_e$Ticker[unchanged_non_treasury_pos], px_start$Ticker)]
    }
    pos_e[unchanged_pos, ] <- pos_e[unchanged_pos, ] %>% fill.sold.price.for.active.pos(end, px)
  }
  
  if(length(new_pos)){
    print("New positions (not sold by end date):")
    print(pos_e[new_pos, ])
    pos_e[new_pos, ] <- pos_e[new_pos, ] %>% fill.sold.price.for.active.pos(end, px)
  }
  
  if(length(closed_pos_old)){
    print("Closed positions (acquired prior to start date):")
    print(pos_e[closed_pos_old, ])
    pos_e$AcqDate[closed_pos_old] <- start - 1
    pos_e$CostBasisPerShare[closed_pos_old] <- px_start$Price[match(pos_e$Ticker[closed_pos_old], px_start$Ticker)]
  }
  
  if(length(closed_pos_new)){
    print("Closed positions (acquired on or after start date):")
    print(pos_e[closed_pos_new, ])
  }
  browser()
  pos_e <- create.main.dashboard(pos_e, div, px)
  
  pos$TotalPNL <- as.character(NA)
  pos$TotalPNL_startpos <- as.character(NA)
  pos$TotalPNL <- pos_e$TotalPNL[match(pos$ID, pos_e$ID)]
  pos$TotalPNL_startpos <- pos_s$TotalPNL[match(pos$ID, pos_s$ID)]
  pos$TotalPNL[is.na(pos$TotalPNL)]<-0
  pos$TotalPNL_startpos[is.na(pos$TotalPNL_startpos)]<-0
  pos$TradingPNL <- pos$TotalPNL-pos$TotalPNL_startpos
  
  pos <- subset(pos, TotalPNL_startpos!=0 | TradingPNL!=0)
  pos
}

inv_pivot <- function(db, group_by_var=c("Ticker", "Account", "VintageYear", "ClosedYear",
                                         "Style1","Style1_Granular","TotalPortfolio", "VintageMonth","AccountType"),
                      highlight_groups=NULL,
                      optpos=NULL){
  message(sprintf("Group var = %s", group_by_var))
  
  # pre-processing
  if(group_by_var == "ClosedYear"){
    db <- subset(db, Active == 0)
    db$ClosedYear <- format(db$SoldDate, "%Y")
    if(!is.null(optpos)) optpos$ClosedYear <- format(optpos$SoldDate, "%Y")
  }else if(group_by_var == "VintageYear"){
    db$VintageYear <- format(db$AcqDate, "%Y")
    if(!is.null(optpos)) optpos$VintageYear <- format(optpos$AcqDate, "%Y")
  }else if(group_by_var == "VintageMonth"){
    db$VintageMonth <- format(db$AcqDate, "%Y%m")
    if(!is.null(optpos)){
      optpos$VintageMonth <- format(optpos$AcqDate, "%Y%m")
      missing <- setdiff(optpos$VintageMonth[optpos$Active==0], db$VintageMonth[db$Active==0])
      if(length(missing)){
        #print(sprintf("Warning: these months do not have closed cash stock positions.  Realized PNL calc may be off: %s", paste(missing, collapse=", ")))
        idx_dummy <- 1:length(missing)
        db <- rbind(db[idx_dummy, ], db) # insert dummy entries in the dashboard
        db$VintageMonth[idx_dummy] <- missing
        db$TotalPNL[idx_dummy] <- 0
      }
    }
  }else if(group_by_var=="TotalPortfolio"){
    db$TotalPortfolio <- "All"
    if(!is.null(optpos)) optpos$TotalPortfolio <- "All"
  }else if(group_by_var=="AccountType"){
    db$AccountType <- "Other"
    db$AccountType[db$Account %in% RETIRE_ACCTS] <- "RetirementAccts"
    db$AccountType[db$Account %in% NON_RETIRE_ACCTS] <- "NonRetirementAccts"
    if(!is.null(optpos)){
      optpos$AccountType <- "Other"
      optpos$AccountType[optpos$Account %in% RETIRE_ACCTS] <- "RetirementAccts"
      optpos$AccountType[optpos$Account %in% NON_RETIRE_ACCTS] <- "NonRetirementAccts"
      
    }
  }
  
  if(!is.null(highlight_groups)){
    db[which(!db[,group_by_var] %in% highlight_groups), group_by_var] <- "Other"
  }
    
  out <- db %>% group_by_at(c(group_by_var, "Active")) %>%
    summarise(SumNumShares=sum(NumShares),
              CostBasisPerShare=weighted.mean(CostBasisPerShare, NumShares),
              SoldPricePerShare=weighted.mean(SoldPricePerShare, NumShares),
              SumTotalCostBasis=sum(TotalCostBasis),
              SumTotalPNL=sum(TotalPNL),
              AvgPctPNL=SumTotalPNL/SumNumShares/CostBasisPerShare,
              AvgPctCapGain=SoldPricePerShare/CostBasisPerShare-1,
              AvgPctDivYield=AvgPctPNL-AvgPctCapGain,
              DivCapGainRatio=AvgPctDivYield/AvgPctCapGain,
              SumTotalDiv=SumTotalCostBasis*AvgPctDivYield
    ) %>%
    as.data.frame
  

  # for supported columns, add in option PNLs
  if(!is.null(optpos) && group_by_var %in% names(optpos)){
    
    if(!is.null(highlight_groups)){
      optpos[which(!optpos[,group_by_var] %in% highlight_groups), group_by_var] <- "Other"
    }
    
    optpos <- optpos %>%
      subset(Active == 0) %>%
      group_by_at(group_by_var) %>%
      mutate(PNL = (SoldPricePerShare - CostBasisPerShare)*NumShares) %>%
      dplyr::summarise(TotalOptionPNL = sum(PNL)) %>%
      as.data.frame
    out$SumOptionPNL <- 0
    idx_add_opt <- which(out$Active==0 & out[,group_by_var] %in% optpos[,group_by_var])
    out$SumOptionPNL[idx_add_opt] <- optpos$TotalOptionPNL[match(out[idx_add_opt,group_by_var], optpos[,group_by_var])]
    out$SumTotalPNL <- out$SumTotalPNL + out$SumOptionPNL
    out$AvgPctPNL <- out$SumTotalPNL / out$SumTotalCostBasis
    out$AvgPctOptionPNL <- out$SumOptionPNL / out$SumTotalCostBasis
    out$SumOptionPNL <- NULL
  }else{
    out$AvgPctOptionPNL <- 0
  }
  
  # hacky
  round_precision <- c(2, 2, 2, 2, 4, 4, 4, 2, 2, 4)
  for(i in 4:ncol(out)){
    out[,i] <- round(out[,i], round_precision[i-3])
  }

  # alternative definition of cost basis
  if(group_by_var != "ClosedYear"){
    out$CostBasisPerShareAlt <- as.numeric(NA)
    out$SoldPricePerShareAlt <- as.numeric(NA)
    # alternative cost basis per share
    out$CostBasisPerShareAlt[out$Active==1] <- out$CostBasisPerShare[out$Active==1]
    out_closed <- subset(out, Active == 0)
    idx_both <- intersect(out[out$Active==1, group_by_var], out_closed[, group_by_var])
    idx_to_adj <- which(out$Active==1 & out[, group_by_var] %in% idx_both)
    match_rows <- match(out[idx_to_adj, group_by_var], out_closed[, group_by_var])
    out$CostBasisPerShareAlt[idx_to_adj] <- out$CostBasisPerShareAlt[idx_to_adj] - out_closed$SumTotalPNL[match_rows] / out$SumNumShares[idx_to_adj]
    # alternative sold price per share
    out$SoldPricePerShareAlt[out$Active==1] <- (out$SumTotalCostBasis[out$Active==1]+out$SumTotalPNL[out$Active==1])/out$SumNumShares[out$Active==1]
    # rounding
    out$PctSoldPriceToCostBasisAlt <- round(out$SoldPricePerShareAlt / out$CostBasisPerShareAlt, 4)
    out$CostBasisPerShareAlt <- round(out$CostBasisPerShareAlt, 2)
    out$SoldPricePerShareAlt <- round(out$SoldPricePerShareAlt, 2)
  }
  
  # summary
  invested <- sum(out$SumTotalCostBasis[out$Active==1])
  unrealized_pnl <- sum(out$SumTotalPNL[out$Active==1])
  realized_pnl <- sum(out$SumTotalPNL[out$Active==0])

  # calculate % acct info
  out$PctCostBasis <- 0
  out$PctCostBasis[out$Active==1] <- round(100 * out$SumTotalCostBasis[out$Active==1] / invested, 2)
    
  print(sprintf("Invested: $%.0f, Total PF Value: $%.0f, Unrealized P&L: $%.0f, Realized P&L: $%.0f",
                invested, invested+unrealized_pnl, unrealized_pnl, realized_pnl))
  
  out
}

inv_pivot_summary <- function(db){
  out <- subset(db, !is.na(PctSoldPriceToCostBasisAlt))
  out <- out[order(out$PctSoldPriceToCostBasisAlt), ]
  out$PNLContri <- round((out$PctSoldPriceToCostBasisAlt - 1) * out$PctCostBasis / 100, 4)
  bycol <- names(out)[1]
  out[, paste0(bycol, "1")] <- out[, bycol] # repeating ID column on the right hand side for better display
  out
}

div_summary<-function(dashboard){
  div_cols<-grep("^Div",names(dashboard))
  n_shares<-dashboard$NumShares
  sapply(dashboard[,div_cols], function(x)round(sum(x*n_shares),2))
}

div_summary_enriched<-function(dashboard, by = "Account"){
  total <- div_summary(dashboard)
  out <- sapply(split(dashboard,dashboard[, by]),div_summary)
  out <- as.data.frame(out)
  if(by == "Account"){
    out$Retire <- rowSums(out[, intersect(RETIRE_ACCTS, names(out))])
    out$Non_Retire <- rowSums(out[, intersect(NON_RETIRE_ACCTS, names(out))])
  }
  out <- cbind(out, Total=total)
  colsum <- sapply(out, sum)
  out <- out[,names(colsum)[colsum > 0]]
  out
}

calc_pos_as_of <- function(pos, as.of){
  pos <- subset(pos, AcqDate <= as.of)
  idx_sold_after_as_of <- which(pos$SoldDate > as.of)
  pos$Active[idx_sold_after_as_of] <- 1
  pos$SoldDate[idx_sold_after_as_of] <- as.Date(NA)
  pos$SoldPricePerShare[idx_sold_after_as_of] <- as.numeric(NA)
  pos
}

cost.basis.wif<-function(z5, ticker, price_pts, new_share){
  this_record <- subset(z5, Ticker == ticker & Active == 1)
  out <- rbind(
    data.frame(ticker = ticker, price = 0, new_share = 0),
    data.frame(ticker = ticker, price = price_pts, new_share = new_share)
  )
  out$new_cost_basis <- this_record$CostBasisPerShareAlt * this_record$SumNumShares + out$price * out$new_share
  out$total_share <- this_record$SumNumShares + out$new_share
  out$new_cost_basis_per_share <- round(out$new_cost_basis / out$total_share, 2)
  out$new_cost_basis <- round(out$new_cost_basis, 2)
  out
}

## CONVENIENCE functions
activeopt <- function(obj1){
  obj1$z_ticker %>% filter (Ticker %in% subset(obj1$optops, Active==1)$Ticker)  
}
closedopt <- function(obj1){
  obj1$z_ticker %>% filter (Ticker %in% subset(obj1$optops, Active==0)$Ticker)  
}

message("Characteristics and Styles")
char <- read_xlsx(fn,sheet = "Characteristics", range = "A1:C10000")
char <- subset(char, !is.na(Ticker))

message("Positions")
pos <- read_xlsx(fn,sheet = "Positions", range = "A1:I10000")
pos <- subset(pos, !is.na(Ticker))
pos$AcqDate <- as.Date(pos$AcqDate)
pos$SoldDate <- as.Date(pos$SoldDate)
pos$ID <- 1:nrow(pos)
pos$Program[is.na(pos$Program)] <- "REGULAR"
print(table(pos$Program))

message("Option Positions")
optpos <- read_xlsx(fn,sheet = "OptionsPos", range = "A1:K10000")
optpos <- subset(optpos, !is.na(Ticker))
optpos <- subset(optpos, Active %in% c(0, 1)) # excl. hypothetical trade (for analysis only, coded as Active == 2)
optpos$AcqDate <- as.Date(optpos$AcqDate)
optpos$SoldDate <- as.Date(optpos$SoldDate)
optpos$ExpiryDate <- as.Date(optpos$ExpiryDate)
optpos$Program <- "REGULAR"
optpos <- merge(optpos, char, by="Ticker")

message("Dividend History")
div <- read_xlsx(fn,sheet = "Div History", range = "A1:G10000")
div <- subset(div, !is.na(Ticker))
div$ExDate<-as.Date(div$`Ex/EFF DATE`)
div$`Ex/EFF DATE`<-NULL

message("Price time series")
px <- read_xlsx(fn,sheet = "Price", range = "B1:D10000")
px <- subset(px, !is.na(Ticker))
px$Date <- as.Date(px$Date)
