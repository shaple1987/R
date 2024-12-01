options(stringsAsFactors = F)
library(plyr)
library(data.table)
library(dplyr)

trim<-function(x) gsub("^\\s+|\\s+$", "", x)

savedata<-function(newdata,name='data',R=list(gen=T,save=T,return=F),saveFlat=T,Rfile=NA,flatfile=NA,purge=F,update=F,
                   datevar='date',indvars,startdate=NA,enddate=NA,showcolnames=T,sep="\t",RDS=F,orderpst=T,...){
  if (R$gen){
    need.to.save<-T
    original_name<-NULL
    if(!purge && file.exists(Rfile)){
      if(RDS) R.hist.old<-readRDS(Rfile) else{
        R.hist.old<get(load(Rfile))
        original_name<-load(Rfile)[1]
        if(length(original_name)>1) stop('Error: Multiple objects saved in the Rfile.')
      }
      if(dim(newdata)[1]) R.hist <- data.append(R.hist.old,newdata,update=update,indvars=indvars, ...)
      if(identical(R.hist.old,R.hist)) need.to.save<-F # if the old and new datasets are identical, skip saving
      rm(R.hist.old)
    }else{ # purge all the existing data!  CAREFUL!
      R.hist <- newdata
    }
    if(!RDS){
      name<-ifelse(is.null(original_name),name,original_name)
      temp <- list(data=R.hist, unique_tag = name)
      assign(temp$unique_tag,temp$data)
    }
    if(need.to.save & R$save) {
      message(paste('Saving data to',Rfile))
      if(RDS) saveRDS(R.hist, file=Rfile) else save(list=temp$unique_tag, file=Rfile)
    }
  }
  if(saveFlat){
    if(R$gen) outfile<-R.hist else outfile<-newdata
    flat.out<-outfile
    if(!is.na(startdate)) flat.out<-outfile[outfile[,datevar]>=startdate]
    if(!is.na(enddate)) flat.out<-outfile[outfile[,datevar]<=enddate]
    if(orderpst) for (i in length(indvars):1) flat.out<-flat.out[order(flat.out[,indvars[i]]),]
    write.table(flat.out,flatfile,col.names=showcolnames, row.names=F, quote=F, sep=sep, na='')
  }
  if(R$gen & R$return) return(R.hist)
}

data.append <- function(data.old, data.new, indvars,
                        check.names = T, orderpst = T, update = T, nondup = T, keepallcol = F){
  missing.new <- names(data.old)[!(names(data.old) %in% names(data.new))]
  missing.old <- names(data.new)[!(names(data.new) %in% names(data.old))]
  if(check.names){
    if (length(missing.new)) warning(paste('Names different', paste(missing.new.collapse=', '), 'from old not in new'))
    if (length(missing.old)) warning(paste('Names different', paste(missing.old.collapse=', '), 'from new not in old'))
  }
  # creating missing indvar columns
  fill.indvar.old<-intersect(missing.old,indvars)
  if(length(fill.indvar.old)){
    message(paste0('Creating these missing indvar columns in the old dataset:',paste(fill.indvar.old,collapse = ', ')))
    for (col in fill.indvar.old) data.old[,col]<-NA
  }
  fill.indvar.new<-intersect(missing.new,indvars)
  if(length(fill.indvar.new)){
    message(paste0('Creating these missing indvar columns in the new dataset:',paste(fill.indvar.new,collapse = ', ')))
    for (col in fill.indvar.new) data.new[,col]<-NA
  }
  
  data.old$pst<-do.call(paste,data.old[indvars])
  data.new$pst<-do.call(paste,data.new[indvars])
  
  if(keepallcol){
    library(plyr)
    if(update){
      data.out <- rbind.fill(data.old[!(data.old$pst %in% data.new$pst),],data.new)
    }else{
      data.out <- rbind.fill(data.old,data.new[!(data.new$pst %in% data.old$pst),])
    }
  }else{
    names.out <- intersect(names(data.old), names(data.new))
    if(update){
      data.out <- rbind(data.old[!(data.old$pst %in% data.new$pst),names.out],data.new[,names.out])
    }else{
      data.out <- rbind(data.old[,names.out], data.new[!(data.new$pst %in% data.old$pst),names.out])
    }
  }
  if(nondup) data.out <- data.out[!duplicated(data.out[,indvars]),]
  if(orderpst) data.out <- data.out[order(data.out$pst),]
  data.out$pst <- NULL
  data.out
}
