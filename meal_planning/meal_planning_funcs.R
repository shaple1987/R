### libraries and functions
library(readxl)
library(readr)
library(dplyr)

gen_list <- function(vector, collapse="、"){
  paste(vector, collapse=collapse)
}

gen_organized_list <- function(vector, collapse="、", optional_handling = c("merge", "remove")[1]){
  if(optional_handling == "merge") vector <- unique(vector %>% remove_optional)
  else if(optional_handling == "remove"){
    optional_idx <- grep("（可选）$", vector)
    if(length(optional_idx)>0) vector <- vector[-optional_idx]
  }
  out <- data.frame(item = vector)
  n_items <- nrow(out)
  out$type <- DB$Type[match(out$item, DB$Name)]
  out$type[is.na(out$type)] <- "未分类"
  out <- out[order(out$type), ]
  out_string <- c()
  for(this_type in unique(out$type)){
    temp <- paste0(this_type, " (", gen_list(subset(out, type == this_type)$item, collapse = collapse), ")")
    out_string <- c(out_string, temp)
  }
  sprintf("%d items: %s", n_items, paste(out_string, collapse = ", "))
}

gen_vector <- function(string, split="、"){
  unlist(strsplit(string, split=split))
}

remove_optional <- function(x){
  sub("（可选）$","", x)
}

gen_ranked_list <- function(inventory, history = NULL, look_back, Type_Include = c("蔬菜", "混合", "荤菜", "汤"), recommend_unit_value = -200, existing_item_scaler = 1, soft_remove_history = TRUE){
  
  list <- subset(masterlist, Type %in% Type_Include)
  list$Recommended[is.na(list$Recommended)] <- 0
  list$rn <- sample(1:nrow(list), nrow(list))
  list$rn <- list$rn + recommend_unit_value * list$Recommended
  
  if(!is.null(history)){
    history2 <- subset(history, Date >= Sys.Date()-look_back)
    message(sprintf("Trim down masterlist based on %d recently made dishes", nrow(history2)))
    if(!soft_remove_history){
      list <- subset(list, !Name %in% history2$Name)
    }
    else{
      idx_history <- which(list$Name %in% history2$Name)
      list$rn[idx_history] <- list$rn[idx_history] + recommend_unit_value * (-5)
    }
    rm(history2)
  }
  
  print(sprintf("%d dishes remain to be considered", nrow(list)))
  print(table(list$Type, list$Type2))
  
  
  # further change recommended value based on in-stock items (rationale: want to first consume remaining in-stock items)
  items_to_ignore <- c("鸡蛋")
  inventory2 <- subset(inventory, !Name %in% items_to_ignore)
  inventory2 <- subset(inventory2, Type %in% c("蔬菜","腌制品","蛋奶豆","海产","禽","肉","丸子汤料"))
  # control for maximum usage of in-stock items
  inventory2$UsageRemaining <- 2
  inventory2$UsageRemaining[which(inventory2$Used==0.5)] <- 1
  list$NumOfExistingIngredients <- 0
  
  for(i in 1:nrow(list)){
    ingredients_used <- list$Ingredients[i] %>% gen_vector %>% remove_optional
    used_indicator <- which(inventory2$Name %in% ingredients_used)
    eligible_in_stock_items <- inventory2$Name[is.na(inventory2$Used) & inventory2$UsageRemaining>0]
    ineligible_in_stock_items <- inventory2$Name[is.na(inventory2$Used) & inventory2$UsageRemaining<=0]
    eligible_low_stock_items <- inventory2$Name[which(inventory2$Used==0.5 & inventory2$UsageRemaining>0)]
    ineligible_low_stock_items <- inventory2$Name[which(inventory2$Used==0.5 & inventory2$UsageRemaining<=0)]
    list$NumOfExistingIngredients[i] <- sum(ingredients_used %in% eligible_in_stock_items) +
      2 * sum(ingredients_used %in% eligible_low_stock_items) # expedite usage of low-stock items
    list$NumOfExistingIngredients[i] <- list$NumOfExistingIngredients[i] - sum(ingredients_used %in% ineligible_in_stock_items) - 
      sum(ingredients_used %in% ineligible_low_stock_items)
    inventory2$UsageRemaining[used_indicator] =  inventory2$UsageRemaining[used_indicator] - 1
  }
  
  list$rn <- list$rn + log(pmax(0.1, list$NumOfExistingIngredients + 1)) * existing_item_scaler * recommend_unit_value    
  list <- list[order(list$rn), ]
  
  # append info on when dishes was used last time
  last_used_by_item <- sapply(split(history, history$Name), function(x) c(LastUsed = max(x$Date), TimeUsed = nrow(x)))
  last_used_by_item <- as.data.frame(t(last_used_by_item))
  last_used_by_item$Name <-rownames(last_used_by_item)
  last_used_by_item$LastUsed <- as.Date(last_used_by_item$LastUsed, origin = as.Date('1970-01-01'))
  list <- merge(list, last_used_by_item, by = "Name", all.x = T)
  #browser()
  return(list)
}

gen_weekly_schedule <- function(list, ndays = 7,startdate = Sys.Date(), Type_Include = c("蔬菜", "混合", "荤菜", "汤"), inventory){
  
  list <- list[order(list$rn), ]
  out <- data.frame(date=seq.Date(startdate, startdate + ndays-1, by=1))
  for(This_Type in Type_Include){
    sl <- subset(list, Type==This_Type)
    # special logic for meat dishes
    if(This_Type == "荤菜") sl <- rbind(sl, tail(subset(list, Type=="混合"), -ndays))
    sl <- head(sl, ndays)
    if(nrow(sl)<ndays) for(i in 1:(ndays-nrow(sl))) sl <- rbind(sl, NA)
    out[, This_Type] <- sl$Name
    out[, paste0(This_Type,"原料")] <- sl$Ingredients
    out[, paste0(This_Type,"备注")] <- sl$Instruction
  }
  return(out)
}

reformat_output <- function(out){
  n_types <- (ncol(out) - 1) / 3
  types <- names(out)[seq(2,ncol(out), by = 3)]
  out_long <- data.frame()
  for (i in 1:n_types){
    start <- 3*(i - 1) + 2
    cols <- c(1, start:(start + 2))
    temp <- out[,cols]
    names(temp) <- c("Date", "Item", "Ingredients", "Notes")
    temp$Type <- types[i]
    out_long <-rbind(out_long, temp)
  }
  out_long <- out_long[order(out_long$Date), ]
  out_long <- subset(out_long, !is.na(Item))
  out_long
}

gen_ingredient_list <- function(out){
  ingredient_list <- c()
  Type_Include <- sub("原料", "", grep("原料$", names(out), value = T))
  for(This_Type in Type_Include){
    ingredient_list <- c(ingredient_list, unlist(strsplit(gen_list(out[, paste0(This_Type,"原料")]),split="、")))  
  }  
  ingredient_list <- table(ingredient_list)
  ingredient_list <- data.frame(Item = names(ingredient_list), UsedFrequency = as.numeric(ingredient_list))
  ingredient_list <- subset(ingredient_list, Item!="NA")
  ingredient_list$UsedAt <- "NA"
  for(j in 1:nrow(ingredient_list)){
    for(This_Type in Type_Include){
      for(i in 1:nrow(out)){
        temp_list <- out[i, paste0(This_Type, "原料")]
        if(is.na(temp_list)) next
        if(length(grep(paste0("^", ingredient_list$Item[j], "$"), temp_list %>% gen_vector))>0) ingredient_list$UsedAt[j] <- paste(ingredient_list$UsedAt[j], out[i,This_Type], sep="、")
      }
    }
  }
  ingredient_list$UsedAt <- sub("^NA、", "", ingredient_list$UsedAt)
  return(ingredient_list)
}

load_history <- function(){
  message("Load meal history")
  history <- read_xlsx("meal_planning.xlsx",sheet = "MealHistory", range = "A1:B1000")
  history <- subset(history, !is.na(Name))
  history$Date <- as.Date(history$Date)
  history
}

post_process_ingredient_list <- function(ingredient_list, inventory){
  # post process to avoid using one particular ingredient too much TODO
  ingredient_list <- merge(ingredient_list, DB, by.x = "Item", by.y = "Name", all.x = TRUE)
  ingredient_list$WeeklyLimit[is.na(ingredient_list$WeeklyLimit)] <- 2
  #browser()
  
  # label in-stock status
  ingredient_list$InStock <- "None"
  ingredient_list$InStock[which(remove_optional(ingredient_list$Item) %in% inventory$Name[which(inventory$Used==0.5)])]<-"Low"
  ingredient_list$InStock[which(remove_optional(ingredient_list$Item) %in% inventory$Name[is.na(inventory$Used)])]<-"InStock"
  
  return(ingredient_list)
}

expand_delimited_column <- function(df, delimited_column, other_columns = setdiff(names(df), delimited_column), delimiter = "、"){
  delim <- df[, delimited_column]
  if(length(other_columns)>1) df <- df[, other_columns]
  else df <- data.frame(df[, other_columns]); names(df) <- other_columns
  out <- data.frame()
  for(i in 1:nrow(df)){
    expanded_delimited_column <- unlist(strsplit(delim[i], split = delimiter))
    out <- rbind(out, cbind(df[i,], Ingredient=expanded_delimited_column, row.names=NULL))
  }
  if(length(other_columns) == 1) names(out)[1] <- other_columns
  out
}