options(stringsAsFactors = F)
library(readxl)

disambiguate_cc<-function(all_records, old_note="AMEX PAYBACK", new_note = "AMEX PAYBACK [LINYE]", rec_acct="Amex", pay_accts=DC_ACCTS){
  data<-subset(all_records,Note==old_note)
  data$ym<-format(data$Date,"%Y%m")
  data_rec<-subset(data,acct1==rec_acct)
  data_pay<-subset(data,acct1 %in% pay_accts)
  merged<-merge(data_rec, data_pay, by.x = c("ym", "Debit"), by.y = c("ym", "Credit"), suffixes = c("_rec", "_pay"), all = T)
  non_matched_keys<-merged$Key_pay[is.na(merged$Key_rec)]
  all_records$Note[which(all_records$Key %in% non_matched_keys)]<-new_note
  print(sprintf("%d records changed from %s to %s", length(non_matched_keys), old_note, new_note))
  all_records
}

assign.acct2<-function(all_accts,debug=F){
  
  all_records <- as.data.frame(rbindlist(all_accts))
  
  # split certain records
  split_configfile <- "splits.csv"
  split_config <- read.csv(sprintf("~/Hai/raw_records/config/%s",split_configfile))
  split_config$Date <- as.Date(split_config$Date, "%m/%d/%Y")
  split_config$Credit[is.na(split_config$Credit)] <- 0
  split_config$Debit[is.na(split_config$Debit)] <- 0
  
  # validate if the net amount in the split txs match with the original tx
  split_net <- split_config %>%
    group_by(Key) %>% dplyr::summarise(Net = sum(Credit) - sum(Debit)) %>% as.data.frame
  orig_net <- all_records %>% subset(Key %in% split_net$Key) %>%
    group_by(Key) %>% dplyr::summarise(Net = sum(Credit) - sum(Debit)) %>% as.data.frame
  split_net$net_orig <- orig_net$Net[match(split_net$Key, orig_net$Key)]
  if(all.equal(split_net$Net, split_net$net_orig)[1] != TRUE){
    split_net <- subset(split_net, Net != net_orig)
    View(split_net)
    browser() # check which key has mismatched amt
    stop("error")
  }
  
  split_config$Key2 <- paste0(split_config$Key, "_split_", 1:nrow(split_config))
  cols_to_update <- c("Note", "Date", "Credit", "Debit")
  to_split <- merge(split_config, all_records[, setdiff(names(all_records), cols_to_update)], by = "Key")
  to_split$Key <- to_split$Key2 # update keys
  all_records <- rbind(subset(all_records, !Key %in% split_config$Key), 
                       to_split[, names(all_records)])
  
  # Check key uniqueness
  n_all_records<-nrow(all_records)
  n_unique_keys<-length(unique(all_records$Key))
  message(sprintf("%d total records, %d unique keys", n_all_records, n_unique_keys))
  if(n_all_records!=n_unique_keys) stop("Error! Duplicative keys")
  
  all_records$Note <- toupper(all_records$Note)
  all_records <- standardize.text(all_records,"Note","Note")
  
  
  # special handling for BOA Checking
  idx_internal_transfer<-c(grep("^ONLINE SCHEDULED TRANSFER FROM CHK 6175",all_records$Note),
                           grep("^ONLINE BANKING TRANSFER TO CHK 6175",all_records$Note),
                           grep("^ONLINE SCHEDULED TRANSFER TO SAV 8117",all_records$Note),
                           grep("^ONLINE BANKING TRANSFER FROM SAV 8117",all_records$Note),
                           grep("^KEEP THE CHANGE TRANSFER TO ACCT 8117",all_records$Note),
                           grep("^KEEP THE CHANGE TRANSFER FROM CHK 6175",all_records$Note))
  #check
  if(length(idx_internal_transfer)>0){
    out1<-all_records[idx_internal_transfer,]
    if(sum(out1$Debit)!=sum(out1$Credit)) stop("Debit and credit amount should match")
    all_records<-all_records[-idx_internal_transfer,]
    print(sprintf("%d internal transfer entries between BOA checking and saving removed",length(idx_internal_transfer)))
  }
  
  
  # special handling for AMEX PAYBACK and SAPPHIRE PAYBACK
  all_records <- all_records %>%
    disambiguate_cc("AMEX PAYBACK", "AMEX PAYBACK [LINYE]", "Amex") %>%
    disambiguate_cc("SAPPHIRE PAYBACK", "FREEDOM PAYBACK [LINYE]", "7309")
  
  # special handling for mortgage principal repayment
  all_records <- all_records %>%
    mortgage.split("LENDER ESCROW DEPOSIT (FREEDOM)", "FREEDOM") %>%
    mortgage.split("LENDER ESCROW DEPOSIT (BETTER)", "BETTER") %>%
    mortgage.split("LENDER ESCROW DEPOSIT (SLS)", "SLS") %>%
    mortgage.split("LENDER ESCROW DEPOSIT (AIM)", "AIM")
  
  # special handling for ESPP investment
  espp <- readRDS("~/Hai/raw_records/config/espp.rds")
  espp$Company <- "BLK"
  espp$Company[which(espp$Type == "espp_mmc")] <- "NERA"

  espp_inc <- data.frame(Note = toupper(paste(espp$Company, "PAYROLL")),
                         acct1 = espp$Type, 
                         Key = paste0("espp_inc_", espp$Date), 
                         Date = espp$Date, 
                         Debit = espp$Amount, Credit = 0)
  
  espp_inv <- data.frame(Note = toupper(paste("CONTRIBUTION TO", espp$Type)), 
                      acct1 = espp$Type, 
                      Key = paste0("espp_inv_", espp$Date), 
                      Date = espp$Date, 
                      Debit = 0, Credit = espp$Amount)

  # special handling for 401Ks
  four01k <- read_xlsx("~/Hai/Spreadsheets/Payroll.xlsx",sheet = "401K", range = "A1:D1000")
  four01k$Range <- NULL
  four01k$Date <- as.Date(four01k$Date)
  four01k <- subset(four01k, !is.na(Date))
  four01k <- melt(four01k,id.vars = "Date")
  four01k <- subset(four01k, value!=0)

  four01k_inc <- data.frame(Note = paste0("BLK PAYROLL (", toupper(four01k$variable),")"),
                         acct1 = "401k",
                         Key = paste0(tolower(four01k$variable),"_inc_", four01k$Date), 
                         Date = four01k$Date, 
                         Debit = four01k$value, Credit = 0)
  
  four01k_inv <- data.frame(Note = paste0("CONTRIBUTION TO BLK ", toupper(four01k$variable)),
                            acct1 = "401k",
                            Key = paste0(tolower(four01k$variable),"_inv_", four01k$Date), 
                            Date = four01k$Date, 
                            Debit = 0, Credit = four01k$value)

  all_records <- rbind(all_records, espp_inc, espp_inv, four01k_inc, four01k_inv)  

  config<-read.csv("~/Hai/raw_records/config/config.csv")
  # resolve <all> accounts
  config_all<-subset(config,acct1=="<all>")
  config_all_adj<-data.frame()
  accts_all<-c(DC_ACCTS,CC_ACCTS)
  for(i in 1:nrow(config_all)){
    config_all_adj<-rbind(config_all_adj,
                          data.frame(type=config_all$type[i],
                                     Note=config_all$Note[i],
                                     acct1=accts_all,
                                     acct2=config_all$acct2[i]))
  }
  config<-rbind(config_all_adj,subset(config,acct1!="<all>"))
  
  config_CC_DEBIT<-subset(config,type=="CC_DEBIT")
  config_CC_CREDIT<-subset(config,type=="CC_CREDIT")
  config_DC_DEBIT<-subset(config,type=="DC_DEBIT")
  config_DC_CREDIT<-subset(config,type=="DC_CREDIT")
  
  out1<-subset(all_records,acct1 %in% CC_ACCTS & Debit>0)
  out1<-merge(out1,config_CC_DEBIT,by=c("Note","acct1"),all.x=T)
  out1$acct2[is.na(out1$acct2)]<-"Exp(Default)"

  out2<-subset(all_records,acct1 %in% CC_ACCTS & Credit>0)
  out2<-merge(out2,config_CC_CREDIT,by=c("Note","acct1"),all.x=T)
  out2$acct2[is.na(out2$acct2)]<-"Exp(Default)"
  
  out3<-subset(all_records,acct1 %in% DC_ACCTS & Debit>0)
  out3<-merge(out3,config_DC_DEBIT,by=c("Note","acct1"),all.x=T)
  idx_reimb <- c(grep("REIMBURSE", out3$Note), grep("OVER RETURN", out3$Note),
                 grep("REFUND", out3$Note), grep("RETURNED$", out3$Note),
                 grep("RECALLED$", out3$Note)) 
  out3$acct2[intersect(which(is.na(out3$acct2)), idx_reimb)] <- "Exp(Reimbursement)"
  idx_inc <- grep("CASHBACK$", out3$Note)
  out3$acct2[intersect(which(is.na(out3$acct2)), idx_inc)] <- "Income"
  out3$acct2[is.na(out3$acct2)]<-"Transfer(Default)"

  out4<-subset(all_records,acct1 %in% DC_ACCTS & Credit>0)
  out4<-merge(out4,config_DC_CREDIT,by=c("Note","acct1"),all.x=T)
  out4$acct2[is.na(out4$acct2)]<-"Exp(Default)"

  if(debug) browser()
  
  out<-rbind.fill(out1,out2,out3,out4)
  out
}

standardize.text<-function(data,incol,outcol=paste0(incol,".standardized"),
                           configfile="standardizer.csv",filler=c("incol","uncategorized","na")[1],silent=T,zero.warning=T){
  config<-read.csv(sprintf("~/Hai/raw_records/config/%s",configfile))
  if(filler=="incol") data[,outcol]<-data[,incol]
  else if(filler=="na") data[,outcol]<-NA
  else data[,outcol]<-filler
  for(i in 1:nrow(config)){
    idx<-grep(config$pattern[i],data[,incol])
    std<-config$standardized[i]
    data[idx,outcol]<-std
    if(!silent || (length(idx)==0 && zero.warning)) print(sprintf("%d records standardized to %s",length(idx),std))
  }
  data
}

print.top.tx<-function(data,n=10){
  if("Cancelled" %in% names(data)){
    canceled.tx<-which(data$Cancelled=="Y")
    if(length(cancelled.tx)) data<-data[-canceled.tx,]
  }
  print(head(data[order(-data$Credit),],n))
  print(head(data[order(-data$Debit),],n))
  return(NULL)
}

find.cancelled.tx<-function(data,fileloc){
  if(!"Cancelled" %in% names(data)) data$Cancelled<-NA
  data_processed<-subset(data,!is.na(Cancelled))
  data_toprocess<-subset(data,is.na(Cancelled))
  d1<-data_toprocess[data_toprocess$Credit>0,c("Key","Date","Note","Credit")]
  d2<-data_toprocess[data_toprocess$Debit>0,c("Key","Date","Note","Debit")]
  merged<-merge(d1,d2,by.x="Credit",by.y="Debit",all=F)
  if(!nrow(merged)){
    print("Cannot find any more cancelled transaction.")
    return(data)
  }
  names(merged)[1]<-"Amount"
  merged$cancelled<-"TBD"
  unique.amount<-rev(unique(merged$Amount))
  for(amt in unique.amt){
    idx<-which(merged$Amount==amt)
    print(merged[idx,c("Amount","Date.x","Note.x","Date.y","Note.y")])
    for(i in idx){
      merged$cancelled[i]<-record.user.input(prompt=sprintf("Is Line %d a cancelled transaction?",i),
                                             allow.blank=F,allowed.opt=c("Y","N")) # NEED TO IMPORT
    }
  }
  key_cancelled<-unique(c(merged$Key.x[merged$cancelled=="Y"]),merged$Key.y[merged$cancelled=="Y"])
  key_not_cancelled<-unique(c(merged$Key.x[merged$cancelled=="N"]),merged$Key.y[merged$cancelled=="N"])
  key_not_cancelled<-setdiff(key_not_cancelled,intersect(key_cancelled,key_not_cancelled))
  data_toprocess$Cancelled[which(data_toprocess$Key %in% key_cancelled)]<-"Y"
  data_toprocess$Cancelled[which(data_toprocess$Key %in% key_not_cancelled)]<-"N"
  data<-rbind(data_processed,data_toprocess)
  data<-data[order(data$Date,data$Key),]
  saveRDS(data,fileloc)
  return(data)
}

#discover<-discover %>% find.cancelled.tx("~/GAM/raw_records/Discover/fullhistory.RDS")

read.csv.ignore.header<-function(file,header,nrows=-1,search.keyword=NULL,offset=0,...){
  temp<-readLines(file)
  if(!is.null(search.keyword)) nrows<-grep(search.keyword,temp)[1]+offset
  if(is.na(nrows)) nrows<-max(nrows+offset,1)
  if(nrows>1) temp<-temp[-(1:(nrows-1))]
  tempfile<-paste0("temp",format(Sys.time(),"%y%m%d%H%M%S"),".out")
  con <- file(tempfile, "w")
  writeLines(temp, con) # CAN BE OPTIMIZED
  close(con)
  out<-read.csv(tempfile,header=header,...)
  file.remove(tempfile)
  return(out)
}

print_summary<-function(outfull,card,type=card,startbal=0){
  if("Cancelled" %in% names(outfull)) cancel.loc<-which(outfull$Cancelled=="Y")
  else cancel.loc<-c()
  if(length(cancel.loc)) outfull<-outfull[-cancel.loc,]
  endbal<-startbal+sum(outfull$Credit)-sum(outfull$Debit)
  if(type %in% checking_types) endbal<-startbal-sum(outfull$Credit)+sum(outfull$Debit)
  print(sprintf("%s: %d transactions captured from %s to %s.  Total debit: %.02f.  Total credit: %.02f.  Starting bal: %.02f.  Ending bal: %.02f.", card, nrow(outfull), min(outfull$Date), max(outfull$Date), sum(outfull$Debit), sum(outfull$Credit), 
                startbal, endbal))
}

raw_data_process<-function(card,type=card,startbal=0,fullhistfile="fullhistory.RDS",debug=F){
  if(debug) browser()
  rootdir<-"C:/Users/where/Documents/Hai/raw_records"
  setwd(file.path(rootdir,card))
  signature<-card
  header<-T
  if(type=="BOA Checking") signature<-"stmt"
  if(type=="Amex") signature<-".csv"
  if(type=="ChaseCheckingSaving") signature<-"Chase"
  
  files<-grep(signature,dir(),value=T)
  if(length(files)==0){
    print("No new file to be processed")
    outfull<-readRDS(fullhistfile)
    print_summary(outfull,card,type,startbal)
    outfull$acct1<-card
    return(outfull)
  }
  out<-data.frame()
  if(!dir.exists("processed")) dir.create("processed")
  for(file in files){
    if(type=="BOA Checking"){
      temp<-read.csv.ignore.header(file,header=header,search.keyword="Date,Description,Amount")
      temp$Amount<-as.numeric(gsub(",","",temp$Amount))
    }else if(type=="ChaseCheckingSaving"){
      temp<-read.csv(file,header=header,row.names = NULL)
      colnames.temp<-names(temp)
      temp<-temp[,-ncol(temp)]
      names(temp)<-colnames.temp[-1]
    }else{
      temp<-read.csv(file,header=header)
    }
    if(type=="BOA"){
      temp$Date<-as.Date(temp$Posted.Date,"%m/%d/%Y")
      temp$Key<-paste0(format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),"_",nrow(temp):1)
    }else if(type %in% c("Chase")){
      temp$Date<-as.Date(temp$Post.Date,"%m/%d/%Y") # to align with statement
      temp$Key<-paste0(format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),"_",nrow(temp):1)
    }else if(type %in% c("ChaseCheckingSaving")){
      temp$Date<-as.Date(temp$Posting.Date,"%m/%d/%Y") # to align with statement
      temp$Key<-paste0(substring(file,6,9),format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),"_",nrow(temp):1)
    }else if(type=="Discover"){
      temp$Date<-as.Date(temp$Trans..Date,"%m/%d/%Y")
      temp$Key<-paste0(format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),"_",nrow(temp):1)
    }
    else if(type=="Amex"){
      temp$Date<-as.Date(temp$Date,"%m/%d/%Y")
      temp$Key<-paste0(card,"_",format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),"_",nrow(temp):1)
    }else if(type=="BOA Checking"){
      temp$Date<-as.Date(temp$Date,"%m/%d/%Y")
    }else if(type=="ally"){
      temp$Date<-as.Date(temp$Date)
    }else if(type=="fidelity"){
      #temp$Date<-as.Date(temp$Date,"%m/%d/%Y")
      temp$Date<-as.Date(temp$Date)
    }
    now<-format(Sys.time(),"%y%m%d%H%M%S")
    processed_now<-file.path("processed", now)
    if(!dir.exists(processed_now)) dir.create(processed_now)
    newfile <- file.path(processed_now,paste0(sub(".csv$","",file),"_",format(min(temp$Date),"%y%m%d"),"_",format(max(temp$Date),"%y%m%d"),".csv"))
    if(debug){
      print(newfile)
      browser()
    }
    file.rename(file, newfile)
    out<-rbind(out,temp)
  }
  if(type=="BOA"){
    out$Note<-out$Payee
  }else if(type=="Chase"){
    out$Note<-out$Description
  }else if(type=="ChaseCheckingSaving"){
    out$Note<-out$Description
  }else if (type=="BOA Checking"){
    out$Note<-out$Description
    out$Key<-paste0(format(out$Date,"%Y%m%d"),"_",out$Running.Bal.)
    out<-subset(out,!is.na(Amount))
  }else if(type=="Discover"){
    out$Amount<-(-1)*out$Amount
    out$Note<-out$Description
  }else if(type=="Amex"){
    out$Amount<-(-1)*out$Amount
    out$Note<-out$Description
  }else if(type=="ally"){
    out$Note<-out$Description
    out$Key<-paste(out$Date,out$Time,sep="_")
  }else if(type=="fidelity"){
    out$Note<-out$Name
    out$Key<-paste(out$Date,out$Memo,sep="_")
  }else{
    stop("Card type not supported yet")
  }
  out$Debit<-0
  out$Credit<-0
  out$Debit[out$Amount>0]<-abs(out$Amount[out$Amount>0])
  out$Credit[out$Amount<0]<-abs(out$Amount[out$Amount<0])
  
  out$Note<-trim(toupper(out$Note))
  out<-out[order(out$Date,out$Key),]
  out<-out[,c("Key","Date","Note","Debit","Credit")]
  
  # special post-process for ally
  if(card=="ally"){
    t1<-table(out$Key)
    t1<-t1[t1>1]
    out2<-out[out$Key %in% names(t1),] #debug
    netbal<-sapply(split(out2,out2$Key),function(x) sum(x$Credit)-sum(x$Debit))
    out2<-subset(out,Key %in% names(netbal)[netbal!=0])
    if(nrow(out2)){
      out2$Key<-paste0(out2$Key,"_",1:nrow(out2))
      print(out2)
      out<-rbind(out[!out$Key %in% names(t1),],out2)
    }
  }
  
  # special post-process for ChaseCheckingSaving
  if(card=="ChaseCheckingSaving"){
    idx_internal_transfer<-c(grep("^ONLINE TRANSFER TO[[:print:]]*7203",out$Note),
                             grep("^ONLINE TRANSFER FROM[[:print:]]*7203",out$Note),
                             grep("^ONLINE TRANSFER TO[[:print:]]*8898",out$Note),
                             grep("^ONLINE TRANSFER FROM[[:print:]]*8898",out$Note))
    #check
    if(length(idx_internal_transfer)>0){
      out1<-out[idx_internal_transfer,]
      browser()
      if(sum(out1$Debit)!=sum(out1$Credit)) stop("[chase] Debit and credit amount should match")
      out<-out[-idx_internal_transfer,]
      print(sprintf("%d internal transfer entries removed",length(idx_internal_transfer)))
    }

    #standardize select entries
    idx_blkpay<-grep("^BLACKROCK FINANC PAYROLL",out$Note)
    if(length(idx_blkpay)>0){
      out$Note[idx_blkpay]<-"BLK PAYROLL"
      print(sprintf("%d BLK payroll entries standardized",length(idx_blkpay)))
    }
  }
  
  # key uniqueness check
  if(length(unique(out$Key))!=nrow(out)){
    browser()
    #tt<-table(out$Key);print(tt[tt>1])
    stop("Duplicate key found.  Please correct it")
  }
  
  if(debug) browser()

  outfull<-savedata(out,RDS=T,Rfile=fullhistfile,saveFlat=F,update=F,R=list(gen=T,save=T,return=T),indvars="Key",keepallcol=T)
  outfull<-outfull[order(outfull$Date,outfull$Key),]
  print_summary(outfull,card,type,startbal)
  
  # key uniqueness check
  if(length(unique(outfull$Key))!=nrow(outfull)){
    stop("Duplicate key found.  Please correct it")
  }
  outfull$acct1<-card
  return(outfull)
}

print.bal.history<-function(outfull,type,dates=NULL,startbal=0){
  if(is.null(dates)){
    dates<-as.Date(paste0(as.numeric(format(Sys.Date(),"%Y%m"))-11:0,"01"),format="%Y%m%d")-1    
  }
  if("Cancelled" %in% names(outfull)) cancel.loc<-which(outfull$Cancelled<-"Y") else cancel.loc<-c()
  if(length(cancel.loc)) outfull<-outfull[-cancel.loc,]
  out<-data.frame(date=dates,endbal=as.numeric(NA))
  for(i in 1:length(dates)){
    idx<-which(outfull$Date<=dates[i])
    if(type=="BOA Checking"){
      out$endbal[i]<startbal-sum(outfull$Credit[idx])+sum(outfull$Debit[idx])
    }else{
      out$endbal[i]<startbal+sum(outfull$Credit[idx])-sum(outfull$Debit[idx])
    }
  }
  out
}
#print.bal.history(boa_chk,"BOA Checking",startbal=9687.01)

pnl.summary<-function(data,by="%Y%m",colnames){
  out<-sapply(split(data,format(data$Date,by)),function(x)c(sum(x$Credit),sum(x$Debit)))
  out<-as.data.frame(t(out))
  names(out)<-colnames
  out$period<-rownames(out)
  out[,c("period",colnames)]
}
#pnl.summary(boa_chk,colnames=c("Expense","Income"))

pnl.summary.net<-function(data,by="%Y%m",colname,type=c("exp","inc")[1],allyms=NULL){
  out<-sapply(split(data,format(data$Date,by)),function(x)sum(x$Credit)-sum(x$Debit))
  out<-as.data.frame(out)
  names(out)<-colname
  if(type=="inc") out[,colname] <- (-1) * out[,colname]
  out$period<-rownames(out)
  out <- out[,c("period",colname)]
  if(!is.null(allyms)){
    out<-merge(out,data.frame(period=allyms),all=T)
    out[is.na(out[, colname]), colname] <- 0
  }
  out[, paste0(colname, "_rolling12m")] <- zoo::rollmean(out[, colname],k=12,fill=NA,align="right")
  out
}

pnl.summary.by.note<-function(data,by="Note"){
  out<-sapply(split(data,data[,by]),function(x)c(sum(x$Credit>0),round(sum(x$Credit),2),sum(x$Debit>0),round(sum(x$Debit),2)))
  out<-as.data.frame(t(out))
  names(out)<-c("Expense.Count","Expense","Payback.Count","Payback")
  out$Net<-out$Expense-out$Payback
  out <- cbind(rownames(out), out)
  names(out)[1] <- by
  out$NetPct<-out$Net/sum(out$Net)
  out <- out %>% arrange(-NetPct)
  out$CumNetPct<-cumsum(out$NetPct)
  totaldf <- out[1, ]
  totaldf[1, 1] <- "total"
  cols_to_sum <- 2:(ncol(out)-1)
  totaldf[1, cols_to_sum] <- sapply(out[, cols_to_sum], sum)
  totaldf[1, ncol(out)] <- 1
  out <- rbind(out, totaldf)
  out$NetPct<-round(100*out$NetPct,2)
  out$CumNetPct<-round(100*out$CumNetPct,2)
  out
}

load.prehistory<-function(prehistfile,fullhistfile,keyprefix,histstart=NULL){

  if(file.exists(fullhistfile)){
    fullhist<-readRDS(fullhistfile)
    if(is.null(histstart)) histstart<-min(fullhist$Date)
    fullhist<-subset(fullhist, Date>=histstart)
  }
  else{
    fullhist<-data.frame()
  }
  
  prehist<-read.csv(prehistfile,stringsAsFactors = F,colClasses = c("character","character","numeric","numeric"))
  prehist$Date<-as.Date(prehist$Date,"%m/%d/%Y")
  prehist<-subset(prehist, Date<histstart)

  for(col in c("Credit","Debit")){
    prehist[,col]<-as.numeric(prehist[,col])
    prehist[is.na(prehist[,col]),col]<-0
  }
  
  idx1<-which(prehist$Credit<0)
  prehist$Debit[idx1]<-(-1)*prehist$Credit[idx1]
  prehist$Credit[idx1]<-0
  
  idx2<-which(prehist$Debit<0)
  prehist$Credit[idx2]<-(-1)*prehist$Debit[idx2]
  prehist$Debit[idx2]<-0
  
  existingcols<-names(prehist)
  prehist$Key<-paste0(keyprefix,1:nrow(prehist))
  prehist<-prehist[,c("Key",existingcols)]
  
  print(sprintf("%d records prior to %s to be loaded", nrow(prehist), histstart))
  rbind.fill(prehist,fullhist)
}

categorize_entries<-function(data,type,only.use.uncategorized=T,saveto=NULL){
  if(type=="BOA Checking"){
    opts<-data.frame(opts=as.character(1:7),
                     desc=c("Income",
                            "Income from Families",
                            "Inv Income",
                            "Transfer",
                            "Expense",
                            "CC Payment",
                            "Uncategorized")
    )
  }else{
    opts<-data.frame(opts=as.character(4:7),
                     desc=c("Transfer",
                            "Expense",
                            "CC Payment",
                            "Uncategorized")
    )
  }
  if(!"type" %in% names(data)) data$type<-"Uncategorized"
  if(only.use.uncategorized){
    data.orig<-data
    data<-subset(data.orig,type=="Uncategorized")
    data.cat<-subset(data.orig,type!="Uncategorized")
    rm(data.orig)
  }else{
    data.cat<-NULL
  }
  if(!nrow(data)){
    print("All entries have been previously categorized.")
    return(NULL)
  }
  data<-data[order(data$Note),]
  i<-1
  while(i<=nrow(data)){
    range<-i:min(i+4,nrow(data))
    message.t(sprintf("Records %d to %d (total %d)",range[1],range[length(range)],nrow(data)))
    print(data[range,])
    print(opts)
    input<-record.user.input(prompt="Pick one from above",allowed.opt=NULL)
    data$type[range]<-opts$desc[match(unlist(strsplit(input,"")),opts$opt)]
    print(data[range,])
    i<-i+5
  }
  data<-rbind(data,data.cat)
  data<-data[order(data$Date,data$Key),]
  if(!is.null(saveto)) saveRDS(data,saveto)
  return(data)
}
#boa_travel2<-categorize_entries(boa_travel,"",saveto="~/GAM/raw_records/2695/fullhistory_withcategoriztion.RDS")

# Search functionalities

search.by.amount<-function(amt){
  subset(all_records2,Credit==amt | Debit==amt)
}

search.by.note<-function(regex){
  out <- all_records2[grep(regex,all_records2$Note),]
  print(sprintf("Net amount: %.02f", sum(out$Credit)-sum(out$Debit)))
  out
}

# Extract certain records

extract_records_per_period<-function(this_period){
  rbind(subset(inc,format(Date,"%Y%m")==this_period),
        subset(exps,format(Date,"%Y%m")==this_period),
        subset(investments,format(Date,"%Y%m")==this_period),
        subset(transfers,format(Date,"%Y%m")==this_period))
}

extract_exp_per_period_type<-function(this_period, categories = "uncategorized"){
  out <- subset(exps,format(Date,"%Y%m")==this_period & Category %in% categories)
  print(sprintf("%d records totaling $%.02f", nrow(out), sum(out$Credit)-sum(out$Debit)))
  out
}

recon_transfer_help<-function(test1){
  t11<-test1[grep("^Transfer",test1$acct2),c("Note","acct1","Date","Debit","Credit")]
  t12<-subset(t11, Debit>0)
  t13<-subset(t11, Credit>0)
  t12<-t12[order(-t12$Debit),]
  t13<-t13[order(-t13$Credit),]
  rowdiff<-nrow(t12)-nrow(t13)
  if(rowdiff>0){
    for(i in 1:rowdiff) t13<-rbind(t13,NA)
  }else if(rowdiff<0){
    for(i in 1:(-rowdiff)) t12<-rbind(t12,NA)
  }
  cbind(t12, t13)
}

all_summary_ytdnet<-function(all_summary,year=year(Sys.Date())){
  start<-100*year+1
  end<-100*year+12
  all_summary_ytd<-subset(all_summary,period>=start & period<=end)
  all_summary_ytd$NetYTD<-cumsum(all_summary_ytd$Net)
  all_summary_ytd
}

trend.by.cat.overtime <- function(exps, time_interval = "%Y%m", class_var = "Category", top_n = 3){
  exps$Period <- format(exps$Date, time_interval)
  exps1 <- exps %>%
    group_by_at(c("Period", class_var)) %>%
    dplyr::summarise(Net = sum(Credit) - sum(Debit)) %>%
    mutate(Note = "TOTAL", Seq = 0) %>%
    as.data.frame
  exps2 <- exps %>%
    group_by(Note, Period) %>%
    dplyr::summarise(Net = sum(Credit) - sum(Debit),
              Category = Category[1]) %>%
    arrange(Period, Category, -Net) %>%
    group_by(Category, Period) %>%
    mutate(Seq = 1:n()) %>% 
    as.data.frame
  idx_non_top <- which(exps2$Seq > top_n)
  exps2$Note[idx_non_top] <- paste0(exps2$Category[idx_non_top], "_ALL_OTHER")
  exps2$Seq[idx_non_top] <- pmax(99, top_n + 1)
  
  exps2 <- exps2 %>%
    group_by(Note, Period, Seq) %>%
    dplyr::summarise(Net = sum(Net),
              Category = Category[1]) %>%
    arrange(Period, Category, Seq) %>%
    as.data.frame
  
  exps2 <- exps2[names(exps1)] # align columns
  
  exps_all <- rbind(exps1, exps2) %>%
    arrange(Period, Category, Seq)
  
  exps_all
}

total.by.cat.overtime <- function(exps){
  zz<-trend.by.cat.overtime(exps)
  zz1<-subset(zz,Seq==0)
  zz2<-dcast(zz1,"Period ~ Category",value.var="Net",fill=0)
  zz2
}

process.espp.data <- function(){
  
  # run when new entries are entered in Payroll.xlsx
  
  nera_sheets <- 2012:2014
  blk_sheets <- c("2014_BLK", 2015:2023)
  
  out <- data.frame()
  
  for(i in 1:length(nera_sheets)){
    espp <- read_xlsx("~/Hai/Spreadsheets/Payroll.xlsx",sheet = paste("Payroll", nera_sheets[i]), range = "A1:AM50", col_types = "numeric")
    espp <- subset(espp, !is.na(Date) & Date!="YTD")
    espp <- espp[, c('Date','After-tax Deduction - Others (see EXP)')] # TODO: number incorrect
    names(espp)[2] <- "Amount"
    espp <- subset(espp, Amount != 0)
    espp$Date <- as.Date(espp$Date, origin = "1899-12-30")
    espp$Type <- "espp_mmc"
    out <- rbind(out, espp)
  }
  
  for(i in 1:length(blk_sheets)){
    if(blk_sheets[i] <= 2018) espp_col <- 'After-tax Deduction - Others (see EXP)' # TODO: number incorrect
    else if(blk_sheets[i] <= 2020) espp_col <- 'ESPP'
    else espp_col <- 'After-tax Deduction - ESPP'
    espp <- read_xlsx("~/Hai/Spreadsheets/Payroll.xlsx",sheet = paste("Payroll", blk_sheets[i]), range = "A1:AM50", col_types = "numeric")
    espp <- subset(espp, !is.na(Date) & Date!="YTD")
    espp <- espp[, c('Date',espp_col)]
    names(espp)[2] <- "Amount"
    espp <- subset(espp, Amount != 0)
    espp$Date <- as.Date(espp$Date, origin = "1899-12-30")
    espp$Type <- "espp_blk"
    out <- rbind(out, espp)
  }
  
  # sense check
  print(sapply(split(out$Amount, format(out$Date, "%Y")), sum))
  saveRDS(out, "~/Hai/raw_records/config/espp.rds")
}

yoy.comp.by.note <- function(exps, y1 = data.table::year(Sys.Date()), y0 = y1 - 1){
  s0 <- pnl.summary.by.note(subset(exps,format(Date, "%Y") == y0),"Category")
  s1 <- pnl.summary.by.note(subset(exps,format(Date, "%Y") == y1),"Category")
  cols_to_keep <- c("Category", "Net", "NetPct")
  col_suffixes <- paste0("_", c(y0, y1))
  s <- merge(s0[, cols_to_keep], s1[, cols_to_keep], by = "Category", all = T, suffixes = col_suffixes)
  s$Diff <- s[, paste0("Net", col_suffixes[2])] - s[, paste0("Net", col_suffixes[1])]
  s$Ratio <- round(100 * s[, paste0("Net", col_suffixes[2])] / s[, paste0("Net", col_suffixes[1])], 2)
  s
}

add.cumsum.cols<-function(all_summary){
  first_period <- min(all_summary$period)
  all_summary$CumInc <- cumsum(all_summary$Inc)
  all_summary$CumExp <- cumsum(all_summary$Exp)
  all_summary$CumInv <- cumsum(all_summary$Inv)
  all_summary[,paste0("NetSince", first_period)]<-cumsum(all_summary$Net)
  all_summary[,paste0("NetBeforeInv", first_period)]<-cumsum(all_summary$NetBeforeInv)
  all_summary
}

output.monthly.summary <- function(start_period, end_period, byvar = "Category2"){
  start_date <- as.Date(paste0(start_period, "01"), "%Y%m%d")
  offset <- ifelse(end_period %% 100 == 12, 89, 1) 
  end_date <- as.Date(paste0(end_period + offset, "01"), "%Y%m%d") - 1
  #print(start_date)
  #print(end_date)
  obj <- list()
  # high-level summary
  obj$summary <- subset(all_summary, period >= start_period & period <= end_period)
  obj$summary <- obj$summary %>% add.cumsum.cols
  # inc / exp by category
  obj$inc <- pnl.summary.by.note(subset(inc,Date>=start_date & Date<=end_date), byvar)
  obj$exp <- pnl.summary.by.note(subset(exps,Date>=start_date & Date<=end_date), byvar)
  return(obj)
}

mortgage.split <- function(all_records, note, company){
  # special handling for mortgage principal repayment
  loan <- subset(all_records, Note == note & Credit > 0)
  loan_repay_schedule <- read_xlsx("~/Hai/raw_records/config/Mortgage Repayment.xlsx",sheet = company, range = "A1:E1000")
  loan$ym <- format(loan$Date, "%Y%m")
  loan_repay_schedule$ym <- format(loan_repay_schedule$Date, "%Y%m")
  loan_repay_schedule$Date <- NULL
  loan <- merge(loan, loan_repay_schedule, by = "ym", all.x = T)
  loan$PrincipalRepay[is.na(loan$PrincipalRepay)] <- 0
  loan$Interest[is.na(loan$Interest)] <- 0
  loan$Credit <- loan$Credit - loan$PrincipalRepay - loan$Interest
  
  loan2 <- loan[!is.na(loan$StartingBal), ]
  loan2$Key <- paste0(loan2$Key, "_prin_repay")
  loan2$Note <- sprintf("PRINCIPAL REPAYMENT (%s)", company)
  loan2$Credit <- loan2$PrincipalRepay
  
  loan3 <- loan[!is.na(loan$StartingBal), ]
  loan3$Key <- paste0(loan2$Key, "_int")
  loan3$Note <- sprintf("MORTGAGE INTEREST (%s)", company)
  loan3$Credit <- loan3$Interest
  
  loan <- rbind(loan, loan2, loan3)
  loan <- loan[names(all_records)]

  all_records <- rbind(subset(all_records, !(Note == note & Credit > 0)), loan)
  all_records
}

# load config
map <- read.csv("~/Hai/raw_records/config/groupings.csv")
map$Group[which(map$Group=="")] <- "Other"
map$Group_since202406[which(map$Group_since202406=="")] <- "Other"
map$Category2[which(map$Category2=="")] <- map$Category[which(map$Category2=="")]