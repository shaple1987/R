source("~/Hai/Code/R/personal_finance/util.R")
source("~/Hai/Code/R/personal_finance/core_functions.R")

checking_types<<-read.csv("~/Hai/raw_records/config/checking_types.csv")$Type
CC_ACCTS<<-read.csv("~/Hai/raw_records/config/CC_ACCTS.csv")$Acct
DC_ACCTS<<-read.csv("~/Hai/raw_records/config/DC_ACCTS.csv")$Acct
EXP_TYPES<<-c("Exp","Exp(Default)","Exp(Reimbursement)")
INC_TYPES<<-c("Income","Inv Income","Fam Income","Gift")
TRANSFER_TYPES<<-c("Transfer","Transfer(Default)")
INVESTMENT_TYPES<<-c("Investments")

############ Main Script #######
force.run <- T
update.csv <- F
update.total.by.cat.overtime <- F
message("Last updated: 7/2/2025, Last Date Capped at 6/30/2025")

if(!"all_accts" %in% ls() || force.run){
  all_accts<-list()
  all_accts$fidelity<-raw_data_process(card="fidelity")
  all_accts$boa_cash<-raw_data_process(card="7375",type="BOA")
  all_accts$boa_travel<-raw_data_process(card="2695",type="BOA")
  all_accts$chase_sapphire<-raw_data_process(card="7309",type="Chase")
  all_accts$amex<-raw_data_process(card="Amex")
  all_accts$discover<-raw_data_process(card="Discover")
  all_accts$chase_chk_sav<-raw_data_process(card="ChaseCheckingSaving")
  all_accts$boa_chk<-raw_data_process(card="BOAChecking",type="BOA Checking") # INCL. BOA SAVING 
  all_accts$pnc<-raw_data_process(card="PNC", type="BOA Checking") # CLOSED AS OF JUN 2015
  all_accts$ally<-raw_data_process(card="ally")
}

all_records2<-assign.acct2(all_accts, debug = F)
message("Plug summary")
print(subset(all_records2, Note=='PLUGPLUGPLUG'))
all_records2<-standardize.text(all_records2,"Note","Category","categorizer.csv",filler="uncategorized")
# alternative categories
all_records2$Category2<-map$Category2[match(all_records2$Category,map$Category)]

# summary by account
as<-sapply(all_accts,function(x)c(N=nrow(x), min_date=min(x$Date), max_date=max(x$Date)))
as<-as.data.frame(t(as))
as$min_date<-as.Date(as$min_date, origin='1970-01-01')
as$max_date<-as.Date(as$max_date, origin='1970-01-01')
print(as)

# key dates
first_month_w_complete_record<-"201007"
last_date_w_complete_record<-min(as$max_date[rownames(as)!="pnc"])
last_month_w_complete_record<-format(as.Date(paste(year(last_date_w_complete_record), month(last_date_w_complete_record), "1", sep="/"), "%Y/%m/%d")-1, "%Y%m")
allyms<-first_month_w_complete_record:last_month_w_complete_record
allyms<-allyms[allyms %% 100 >= 1 & allyms %% 100 <= 12]

# Income and expense analysis

inc<-subset(all_records2,acct2 %in% INC_TYPES)
inc_summary<-pnl.summary.net(inc,colname = "Inc", type="inc", allyms = allyms) # monthly inc total
inc_by_note<-pnl.summary.by.note(inc)
inc_by_note<-standardize.text(inc_by_note,"Note","Category","categorizer.csv",filler="uncategorized",silent=T,zero.warning=F)
# inc since 2022
inc_by_note_ytd<-pnl.summary.by.note(subset(inc,Date>='2022-01-01'))
inc_by_note_ytd<-standardize.text(inc_by_note_ytd,"Note","Category","categorizer.csv",filler="uncategorized",silent=T,zero.warning=F)
if(update.csv) write.csv(inc,"~/Hai/raw_records/outfiles/income.csv",row.names=F)

exps<-subset(all_records2,acct2 %in% EXP_TYPES)
exps_summary<-pnl.summary.net(exps,colname = "Exp", type="exp", allyms = allyms) # monthly exp total
#exps_summary_byyear<-pnl.summary.net(exps,"%Y",colnames = "Exp") # annual exp total
exps_by_note<-pnl.summary.by.note(exps)
exps_by_note<-standardize.text(exps_by_note,"Note","Category","categorizer.csv",filler="uncategorized",silent=T,zero.warning=F)
#print(head(subset(exps_by_note,Category=='uncategorized')))
# exps since 2022
exps_by_note_ytd<-pnl.summary.by.note(subset(exps,Date>='2022-01-01'))
exps_by_note_ytd<-standardize.text(exps_by_note_ytd,"Note","Category","categorizer.csv",filler="uncategorized",silent=T,zero.warning=F)
#print(head(subset(exps_by_note_ytd,Category=='uncategorized')))
exps$Group <- map$Group_since202406[match(exps$Category, map$Category)]
daily_exp_by_group <- exps %>% group_by(Date, Group) %>% summarise(exp = sum(Credit)-sum(Debit))
daily_exp_by_group <- dcast(daily_exp_by_group, "Date ~ Group",value.var="exp",fill=0)
exps$Group<-NULL

if(update.csv) write.csv(exps,"~/Hai/raw_records/outfiles/expenses.csv",row.names=F)

message("Overall inc by category")
print(pnl.summary.by.note(inc,"Category"))
#message("Since 2022 inc by category")
#print(pnl.summary.by.note(subset(inc,Date>='2022-01-01'),"Category"))

message("Overall exp by category")
print(pnl.summary.by.note(exps,"Category"))
#message("Since 2022 exp by category")
#print(pnl.summary.by.note(subset(exps,Date>='2022-01-01'),"Category"))

zz3<-pnl.summary.by.note(subset(exps,Date>=last_date_w_complete_record-364 & Date<=last_date_w_complete_record),"Category")
zz3$Net_Monthly <- round(zz3$Net / 12, 2)

# Investments

investments<-subset(all_records2,acct2 %in% INVESTMENT_TYPES)
investments_by_note<-pnl.summary.by.note(investments)
investments_by_note_ytd<-pnl.summary.by.note(subset(investments,Date>='2022-01-01'))

inv_summary<-pnl.summary.net(investments,colname = "Inv", type="exp", allyms = allyms) # monthly inv total

all_summary<-merge(inc_summary, exps_summary, by="period", all=T)
all_summary<-merge(all_summary, inv_summary, by="period", all=T)
all_summary$Inc[is.na(all_summary$Inc)] <- 0
all_summary$Exp[is.na(all_summary$Exp)] <- 0
all_summary$Inv[is.na(all_summary$Inv)] <- 0
all_summary$Net <- all_summary$Inc - all_summary$Exp - all_summary$Inv
all_summary$NetBeforeInv <- all_summary$Inc - all_summary$Exp
rm(inc_summary, exps_summary, inv_summary)

# plot trend
all_summary$plotdate<-as.Date(paste0(all_summary$period,"01"), "%Y%m%d")
#plot(all_summary$plotdate, all_summary$Inc_rolling12m, ylim=range(all_summary$Inc_rolling12m, all_summary$Exp_rolling12m, na.rm=T),type='l',main="Rolling 12m Inc and Exp", xlab="Date", ylab="Amount")
#lines(all_summary$plotdate, all_summary$Exp_rolling12m, col=2)
#legend("topright", legend = c("Inc","Exp"), col=1:2, lty = 1)

all_summary_since2022<-subset(all_summary,period>=202201)

#all_summary <- all_summary %>% add.cumsum.cols
all_summary_since2022 <- all_summary_since2022 %>% add.cumsum.cols
#sapply(split(all_summary,substring(all_summary$period,1,4)),function(x)summary(x$Net))

print(sapply(all_summary[,c("Inc", "Exp", "Inv", "Net")],sum))

# Transfers

transfers<-subset(all_records2,acct2 %in% TRANSFER_TYPES)
transfers_byym<-sapply(split(transfers,format(transfers$Date,"%Y%m")),function(x)c(debit=sum(x$Debit),credit=sum(x$Credit)))
transfers_byym<-as.data.frame(t(transfers_byym))
transfers_byym$ym<-as.numeric(rownames(transfers_byym))
transfers_byym$net<-transfers_byym$debit-transfers_byym$credit
print(summary(transfers_byym$net))
rmsd_transfer <- sqrt(sum(transfers_byym$net*transfers_byym$net)/nrow(transfers_byym))
print(sprintf("%d non-zero entries out of %d, RMSD = %.02f", length(which(transfers_byym$net!=0)), nrow(transfers_byym), rmsd_transfer))
transfers_byym_nonzero <- subset(transfers_byym, net!=0)

# monthly summary by year
years <- 2020:year(max(all_records2$Date))
allobjs <- vector("list", length(years))
names(allobjs) <- paste0("y", years)
for (i in 1:length(years)){
  temp <- output.monthly.summary(start_period = years[i] * 100 + 1, end_period = years[i] * 100 + 12)
  write.csv(temp$summary, sprintf("~/Hai/raw_records/outfiles/annual_summary/%s_summary.csv", years[i]))
  write.csv(temp$inc, sprintf("~/Hai/raw_records/outfiles/annual_summary/%s_inc.csv", years[i]))
  write.csv(temp$exp, sprintf("~/Hai/raw_records/outfiles/annual_summary/%s_exp.csv", years[i]))
  allobjs[[i]] <- temp
}
rm(temp)

# Monthly exp by category
if(update.total.by.cat.overtime){
  zz2 <- total.by.cat.overtime(exps)
  zz2 <- zz2[order(zz2$Period, decreasing = TRUE), ]
  print("Uncategorized exp summary by year")
  print(sapply(split(zz2,substring(zz2$Period,1,4)),function(x)summary(x$uncategorized)))
  write.csv(zz2, "~/Hai/raw_records/outfiles/monthly_exp_summary.csv", row.names=F)
}

if(F){
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/ally_prehist.csv","~/Hai/raw_records/ally/fullhistory.RDS","ally_prehist_",as.Date('2020-06-16'))
  print_summary(zz,card="ally",type="ally");print(dim(zz))
  #saveRDS(zz,"~/Hai/raw_records/ally/fullhistory.RDS") # manually execute after reconciliation!!
  
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/Amex_prehist.csv","~/Hai/raw_records/Amex/fullhistory.RDS","Amex_prehist_",as.Date('2020-11-09'))
  print_summary(zz,card="Amex",type="Amex");print(dim(zz))
  #saveRDS(zz,"~/Hai/raw_records/Amex/fullhistory.RDS") # manually execute after reconciliation!!
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/ChaseCheckingSaving_prehist.csv","~/Hai/raw_records/ChaseCheckingSaving/fullhistory.RDS","ChaseCheckingSaving_prehist_",as.Date("2019-06-06"))
  print_summary(zz,card="ChaseCheckingSaving",type="ChaseCheckingSaving")
  #saveRDS(zz,"~/Hai/raw_records/ChaseCheckingSaving/fullhistory.RDS")
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/7309_prehist.csv","~/Hai/raw_records/7309/fullhistory.RDS","7309_prehist_",as.Date('2019-06-01'))
  print_summary(zz,card="7309",type="Chase");print(dim(zz))
  #saveRDS(zz,"~/Hai/raw_records/7309/fullhistory.RDS")
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/7375_prehist.csv","~/Hai/raw_records/7375/fullhistory.RDS","7375_prehist_",as.Date('2020-05-28'))
  print_summary(zz,card="7375",type="BOA");print(dim(zz))
  #saveRDS(zz,"~/Hai/raw_records/7375//fullhistory.RDS") # manually execute after reconciliation!!
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/2695_prehist.csv","~/Hai/raw_records/2695/fullhistory.RDS","2695_prehist_",as.Date('2020-05-13'))
  print_summary(zz,card="2695",type="BOA");print(dim(zz))
  #saveRDS(zz,"~/Hai/raw_records/2695//fullhistory.RDS") # manually execute after reconciliation!!
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/BOAChecking_prehist.csv","~/Hai/raw_records/BOAChecking/fullhistory.RDS","BOAChecking_prehist_",as.Date('2019-08-20'))
  print_summary(zz[,1:5],card="BOAChecking",type="BOA Checking");print(dim(zz))
  #saveRDS(zz[,1:5],"~/Hai/raw_records/BOAChecking/fullhistory.RDS") # manually execute after reconciliation!!


  zz<-load.prehistory("~/Hai/raw_records/prehist/Discover_prehist.csv","~/Hai/raw_records/Discover/fullhistory.RDS","Discover_prehist_",as.Date('2020-08-24'))
  print_summary(zz[,1:5],card="Discover",type="Discover");print(dim(zz))
  #saveRDS(zz[,1:5],"~/Hai/raw_records/Discover/fullhistory.RDS") # manually execute after reconciliation!!
  
  zz<-load.prehistory("~/Hai/raw_records/prehist/PNC_prehist.csv","~/Hai/raw_records/PNC/fullhistory.RDS","PNC_prehist_",as.Date('2015-06-14'))
  print_summary(zz[,1:5],card="PNC",type="BOA Checking");print(dim(zz))
  #saveRDS(zz[,1:5],"~/Hai/raw_records/PNC/fullhistory.RDS") # manually execute after reconciliation!!
  
  
    # one off analysis for finding out type mapping rules
  
  a1<-subset(all_records,format(all_records$Date,"%Y%m")=="202102")
  a2<-subset(all_records,acct1 %in% CC_ACCTS & Debit>0)
  a22<-a2 %>% group_by(Note,acct1) %>% summarise(total=sum(Debit))
  write.csv(a22,"~/Hai/raw_records/note1.csv",row.names=F)
  all_records<-as.data.frame(rbindlist(all_accts))
  # a2<-subset(all_records,acct1 %in% DC_ACCTS & Debit>0)
  # a2 <- standardize.text(a2,"Note","Note")
  # a22<-a2 %>% group_by(Note,acct1) %>% summarise(total=sum(Debit))
  # write.csv(a22,"~/Hai/raw_records/note2.csv",row.names=F)
  a2<-subset(all_records,acct1 %in% DC_ACCTS & Credit>0)
  a2 <- standardize.text(a2,"Note","Note")
  a22<-a2 %>% group_by(Note,acct1) %>% summarise(total=sum(Credit))
  write.csv(a22,"~/Hai/raw_records/note3.csv",row.names=F)

  zz2<-trend.by.cat.overtime(exps) %>% subset(Category=="linye cc");zz2t<-subset(zz2,Note=="TOTAL")
  zz2t$Net_rolling_12m_mean <- zoo::rollmean(zz2t$Net,k=12,fill=NA,align="right")
    
}
