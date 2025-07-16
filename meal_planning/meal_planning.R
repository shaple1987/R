rm(list=ls())
Sys.setlocale("LC_ALL","chs")
# run the above two lines, then run the following:
# source("~/Hai/Code/R/meal_planning/meal_planning.R", encoding='UTF-8')
options(stringsAsFactors = F)

START_DATE <- Sys.Date() + 1
LOOK_BACK <- 7
NDAYS <- 6
REMOVE_OUT_OF_STOCK_ITEMS <- T
write.output.file <- T

message("Check settings before proceeding")
print(sprintf("Generating meal plan from %s to %s", START_DATE, START_DATE + NDAYS - 1))
print(sprintf("Historical dish cool down period: %d days", LOOK_BACK))
print(sprintf("Removing out of stock items? %s | Print output file? %s", REMOVE_OUT_OF_STOCK_ITEMS, write.output.file))
browser()

# overrides
ITEMS_TO_EXCLUDE <- c()# c("卤牛肉", "土豆苹果午餐肉色拉", "清炒藕片")
ITEMS_MUST_HAVE <- c()#c("番茄莲藕排骨汤", "板栗烧鸡", "茭白肉丝")
banned_items <- c("海带","肥牛片","牛尾","猪肝","黄豆芽","绿豆芽","豆苗","牛腩","叉烧肉") #"紫菜","裙带菜",

END_DATE <- START_DATE + NDAYS - 1
out_file <- sprintf("%s_to_%s.csv", START_DATE, END_DATE)
out_file_long <- sprintf("%s_to_%s_longform.csv", START_DATE, END_DATE)
processed_masterlist_file <- sprintf("%s_to_%s_processed_masterlist.rds", START_DATE, END_DATE)

source("~/Hai/Code/R/meal_planning/meal_planning_funcs.R", encoding = "UTF-8")
setwd("~/Hai/Meal Planning/")

### main script

message("Load master dish list")
masterlist <<- read_xlsx("meal_planning.xlsx",sheet = "MasterList", range = "A1:G1000") %>% as.data.frame
masterlist <- subset(masterlist, !is.na(Name))

message("Load ingredient database")
DB <<- read_xlsx("meal_planning.xlsx",sheet = "Ingredients_NEW", range = "A1:G1000") %>% as.data.frame
DB <- subset(DB, !is.na(Name))

message("Load ingredient database from GoogleSheet (NEW)")
DB2 <<- tryCatch(read_gsheet_ingredients(),
              error=function(e){
              message(sprintf("Error condition: {%s}", e))
              print("Cannot connect to Googlesheet, using local cache as backup")             
              DB
            }
)

history <- load_history()

message("Load inventory")
inventory <- DB2
inventory_full <- inventory
inventory <- subset(inventory, Remaining > 0)
# TODO print out warning for expired / soon-to-expire food
#inventory$Expiry <- as.Date(inventory$Expiry) # not implemented
#inventory <- subset(inventory, Expiry >= Sys.Date())

all_items <- gen_vector(gen_list(masterlist$Ingredients))
all_items <- setdiff(all_items, "NA")
all_items <- remove_optional(all_items)
all_items <- unique(all_items)
additional_in_stock_items <- c()
if(length(additional_in_stock_items)){
  print(sprintf("These %d items are also in stock (not in inventory sheet yet): %s", length(additional_in_stock_items), gen_list(additional_in_stock_items)))
}
print(sprintf("These %d items are not currently allowed: %s", length(banned_items), gen_organized_list(banned_items)))
not_used_items <- setdiff(inventory$Name, all_items)
print(sprintf("These %d items are not used: %s", length(not_used_items), gen_organized_list(not_used_items)))
no_inventory <- setdiff(all_items, unique(c(additional_in_stock_items, inventory$Name)))

print(sprintf("These %d items are not in stock: %s", length(no_inventory), gen_organized_list(no_inventory)))
removed_list <- data.frame()

if(REMOVE_OUT_OF_STOCK_ITEMS){
  message("Trim down masterlist by excluding out-of-stock and banned items")
  items_to_remove <- unique(c(no_inventory, banned_items))
}else{
  message("Trim down masterlist by excluding banned items")
  items_to_remove <- banned_items
}

temp <- masterlist %>% expand_delimited_column("Ingredients", "Name")
temp <- temp[-grep("（可选）$", temp$Ingredient), ]
temp <- subset(temp, Ingredient %in% items_to_remove)
masterlist_full <- masterlist
masterlist <- subset(masterlist_full, !Name %in% temp$Name)
removed_list <- subset(masterlist_full, Name %in% temp$Name)
remove(temp, masterlist_full)

message("Generate weekly schedule")

if(file.exists(processed_masterlist_file)){
  message(sprintf("Loading pre-stored processed masterlist file: %s", processed_masterlist_file))
  processed_masterlist <- readRDS(processed_masterlist_file)
}else{
  processed_masterlist <- gen_ranked_list(inventory = inventory, history = history, look_back = LOOK_BACK)  
}
if(write.output.file){
  saveRDS(processed_masterlist, processed_masterlist_file)
}
View(processed_masterlist)

message("Apply ad-hoc adjustments")
processed_masterlist2 <- subset(processed_masterlist, !Name %in% ITEMS_TO_EXCLUDE)
idx_must_have <- which(processed_masterlist2$Name %in% ITEMS_MUST_HAVE)
processed_masterlist2$rn[idx_must_have] <- processed_masterlist2$rn[idx_must_have] - 2000


if(file.exists(out_file)){
  message(sprintf("Loading pre-stored output csv file: %s", out_file))
  out <- read_csv(out_file) %>% as.data.frame
}else{
  out <- gen_weekly_schedule(processed_masterlist2, ndays=NDAYS, startdate=START_DATE, inventory = inventory)
}

out_long <- reformat_output(out)

if(write.output.file){
  write_excel_csv(out, path = out_file)
  write_excel_csv(out_long, path = out_file_long)
}
View(out)

ingredient_list <- gen_ingredient_list(out)

il <- ingredient_list %>% post_process_ingredient_list(inventory = inventory)
message("Existing Items (that will be used)")
print(gen_organized_list(subset(il, InStock %in% c("InStock","Low"))$Item))
message("Existing Items (that will NOT be used) -- NEED ATTENTION")
print(gen_organized_list(setdiff(inventory$Name, remove_optional(il$Item))))
message("Shopping List")
print(gen_organized_list(subset(il, InStock=="None")$Item, optional_handling = "remove"))
message("Shopping List (low stock item that will be used more than once)")
print(gen_organized_list(subset(il, InStock=="Low" & UsedFrequency>1)$Item, optional_handling = "remove"))
message("Shopping List (常备佐料)")
print(gen_organized_list(setdiff(DB$Name[DB$Type=="常备佐料"], subset(inventory_full, Remaining > 0 & Type == "常备佐料")$Name)))

message("These items might have been used too frequently")
print(subset(il, UsedFrequency > WeeklyLimit))
# low stock item (need to revamp)
#View(subset(il, InStock=="Low"))

