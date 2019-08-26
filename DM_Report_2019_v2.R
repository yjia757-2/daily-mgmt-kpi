library(dplyr)
library(sjmisc)
library(gsubfn)
library(tidyverse)
library(zoo)
library(ggplot2)
library(modelr)
library(splines)
library(hexbin)
library(stringr)
library(plotly)
library(reshape2)
library(bpa)
library(stringr)


# Date: Jan 11, 2019
# Updated On: Jan 21, 2019
# Author: Yiran Jia
# Project Name: DM Report for 2019
#
# After a year we clearly know what charts we need, and they are: stacked_all_cycl for P&D; stacked_idn_penetration for I; stacked_tcv for T; 
# and stacked_funnel_health for F. That's all we need to generate the new Tableau report. 


# Step One: set up the dataset you need for this time report
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Phil/KIFCT/20190101KIFCT/Reports")
filenames <- list.files(pattern = '*.csv', full.names = TRUE)
filenames <- data.frame(substr(filenames, 3, nchar(filenames)), stringsAsFactors = FALSE)
colnames(filenames) <- "Names"
temp <- unlist(strsplit(filenames$Names, "_"))
temp <- data.frame(matrix(temp, ncol = 2, byrow = TRUE))
filenames <- filenames %>% mutate(Import_Date = temp$X1)
last_report_date <- nrow(filenames)
last_report_date <- filenames[last_report_date, 2]
last_report_date <- as.Date(last_report_date, "%Y%m%d") # use for tcv and funnel reports after the for loop. bcz we only care about reporting the 
                                                        # last month performance for these two

# ADJUST NEEDED BASED ON THE CURRENT YEAR GOAL. create a df which includes the current year taget value 
target <- function(q1, q2, q3, q4) {
  temp <- data.frame(Actual_Close = c("2019-01-01",
                                      "2019-04-01",
                                      "2019-07-01",
                                      "2019-10-01"),
                     Target_TCV = c(q1, q2, q3, q4))
  return(temp)
}
target_curr <- target(106, 210, 140, 246) # this number need to be updated every year based on the target the leader group set up for LSP team.
win_rate <- 2.5 # need to adjust based on the average win rate of previous year 
target_curr$Actual_Close <- strptime(target_curr$Actual_Close,format="%Y-%m-%d")
target_curr$Actual_Close  <- as.yearqtr(target_curr$Actual_Close ,format="%Y-%m-%d")
target_funnel <- target_curr %>% mutate(Planned_Close = Actual_Close, Target_Funnel = win_rate*Target_TCV) %>% select(-Actual_Close, -Target_TCV)

# this is a big for loop since we need get extract many different metrics from it. each loop will process one file, which is listed in 
# filenames df. We named new metrics xxx_each, and then named the all time period df stacked_xxx. After the end of the for loop, 
# you should have stacked_xxx metrics available. export them and use to generate DM report. 
for (i in 1:nrow(filenames)) {
  # only left useful information 
  each <- read.csv(filenames[i,1], stringsAsFactors = FALSE, na.strings=c("", "NA")) %>% mutate(Import_Date = filenames[i,2]) %>% 
    select(Oppy = Opportunity.Name, Stage, Value = Value.in.M, Planned_Close = Planned.Close.Date, Actual_Close = Actual.Close.Date, CM = Capture.Manager, 
           BD = Business.Developer, AE = Account.Executive, ADB0 = Actual.DB.0, ADB1 = Actual.DB.1, ADB2 = Actual.DB.2, ADB3 = Actual.DB.3, 
           ADB4 = Actual.DB.4, District, Golden_Id = Golden.Oppy.ID, Created, Modified, Complexity = Complexity.Level, 
           Import_Date, Win_Perc = Win..)
  
  # clean dataset. change character date to date type 
  each$Created <- as.Date(each$Created, "%m/%d/%Y %H:%M")
  each$Modified <- as.Date(each$Modified, "%m/%d/%Y %H:%M")
  each$Import_Date <- as.Date(each$Import_Date, "%Y%m%d")
  each$Planned_Close <- as.Date(each$Planned_Close, "%m/%d/%Y")
  each$Actual_Close <- as.Date(each$Actual_Close, "%m/%d/%Y")
  each$ADB0 <- as.Date(each$ADB0, "%m/%d/%Y")
  each$ADB1 <- as.Date(each$ADB1, "%m/%d/%Y")
  each$ADB2 <- as.Date(each$ADB2, "%m/%d/%Y")
  each$ADB3 <- as.Date(each$ADB3, "%m/%d/%Y")
  each$ADB4 <- as.Date(each$ADB4, "%m/%d/%Y")
  each$Value <- as.numeric(gsub('[$]', '', each$Value))
  each$Win_Perc <- as.numeric(gsub('[%]', '', each$Win_Perc)) * 0.01
  change_create <- function(id, new_created) {
    each[!is.na(each$Golden_Id) & each$Golden_Id == id, "Created"] <- as.Date(new_created, format = "%m/%d/%Y")
    return(each)
  }
  each <- change_create("1440900", "1/19/2015")
  each <- change_create("1920000", "3/14/2016")
  each <- change_create("1366004", "11/4/2014")
  each <- change_create("2466900", "9/7/2016")
  # some won deals don't have db4, so I'm using actual close date to replace db4
  incorrect <- subset(each, is.na(ADB4) & Stage == "06 - Won")
  each[rownames(incorrect), 'ADB4'] <- each[rownames(incorrect), "Actual_Close"]  
  
  # a won deal has to have both planned closed date and acutal close date, since everything is from stacked_funnel, which is 
  # filtered by the planned_closed date happened in the curretn fiscal year. thus we need to make sure every won deal has a plannned _closed
  # date, if it doesn't, we use its actual closed date to replace. 
  incorrect2 <- subset(each, is.na(Planned_Close) & Stage == "06 - Won")
  each[rownames(incorrect2), 'Planned_Close'] <- each[rownames(incorrect2), "Actual_Close"]
  
  # prepare to create the FIRST metrics - deal board cycle - how long does it convert to the next deal board 
  cycle_each <- each  %>%  filter(!grepl("20", Stage)) %>%  #!grepl("11", Stage), !grepl("12", Stage), !grepl("13", Stage), !grepl("14", Stage), !grepl("15", Stage)) %>% 
    mutate(preq_db0 = ADB0 - Created, db0_db1 = ADB1 - ADB0, db1_db2 = ADB2 - ADB1, db2_db3 = ADB3 - ADB2, db3_db4 = ADB4 - ADB3, db0_db4 = ADB4 - ADB0) 
  cycle_each[!is.na(cycle_each$preq_db0) & cycle_each$preq_db0 < 0, "preq_db0"] <- NA # ignore the negative cycle time 
  # we create all_cycle_each for the report that covers the average cycle time by clculating all past cycles 
  cycle_each  <- cycle_each %>% select(Import_Date, Oppy, Complexity, District, Stage, Value, CM, BD, AE, ADB0, ADB4, preq_db0, db0_db1, db1_db2, db2_db3, db3_db4, db0_db4)
  # use the prepared one to generate a stcked_XXX, which will be used after for loop end 
  if(!exists("stacked_all_cycle")) {
    stacked_all_cycle <- cycle_each
  } else {
    stacked_all_cycle <- rbind(stacked_all_cycle, cycle_each)
  }
  
  # prepare to create the Second metrics - funnel health. The stacked_funnel within the for loop is a source for stacked_tcv.
  funnel_each <- each %>% filter(!grepl("20", Stage), !grepl("11", Stage), !grepl("12", Stage), !grepl("13", Stage), !grepl("14", Stage), !grepl("15", Stage), !grepl("01", Stage)) %>% 
    select(Import_Date, Oppy, Complexity, District, Stage, Value, Win_Perc, CM, BD, AE, Planned_Close, Actual_Close) %>% 
    filter(between(Planned_Close, as.Date("2019-01-01"), as.Date("2019-12-31")))
  
  # use the prepared one to generate a stacked_xxx, which will be used after for loop end for stacked_tcv
  if(!exists("stacked_funnel")) {
    stacked_funnel <- funnel_each
  } else {
    stacked_funnel <- rbind(stacked_funnel, funnel_each)
  }
  stacked_funnel[is.na(stacked_funnel$Value), "Value"] <- 0
  
  # prepare to create the Third metrics - idn penetration. Since the other hald of data is not from control tower list, so I need to 
  # get something done from control tower, and then mannully link them together outside. 
  pene_each <- each %>% mutate(LSP_Penetrated = NA)
  pene_each1 <- subset(pene_each, Stage == "01 - Pre-Qual")
  pene_each2 <- subset(pene_each, Stage == "14 - Pre-Qual Eliminated")
  pene_each3 <- subset(pene_each, Stage == "20 - Duplicate")
  pene_each4 <- subset(pene_each, Stage != "14 - Pre-Qual Eliminated" & Stage != "01 - Pre-Qual" & Stage != "20 - Duplicate")
  pene_each[rownames(pene_each1), "LSP_Penetrated"] <- 0
  pene_each[rownames(pene_each2), "LSP_Penetrated"] <- 0
  pene_each[rownames(pene_each3), "LSP_Penetrated"] <- 0
  pene_each[rownames(pene_each4), "LSP_Penetrated"] <- 1
  pene_each <- pene_each %>% select(Import_Date, Oppy, LSP_Penetrated, Stage) 
  if(!exists("stacked_idn_pene")) {
    stacked_idn_pene <- pene_each
  } else {
    stacked_idn_pene <- rbind(stacked_idn_pene, pene_each)
  }
}
stacked_idn_pene <- stacked_idn_pene %>% filter(Import_Date == last_report_date)
# use stacked_funnel to generate:
# TCV
# TCV by district 
# Funnel Health
# Funnel Health by district 

# TCV
# set up the base, the most recent report 
stacked_tcv <- stacked_funnel %>% filter(Import_Date == last_report_date, Stage == "06 - Won") %>% select(-Planned_Close) 
stacked_tcv$Actual_Close <- as.yearqtr(stacked_tcv$Actual_Close, format = "%Y-%m-%d")
# set up the right target: target - govt - canada (if there's zero govt earning, the target does not change)
district_won <- stacked_tcv %>% group_by(District, Actual_Close) %>% summarise(Value = sum(Value))
gov_won <- district_won %>% filter(District == "Govt") # cycle for govt deals 
can_won <- district_won %>% filter(District == "Canada")
national_won <- district_won %>% filter(District == "National")

# do three cycles to roll out of the won by Govt, Canada, and National 
net_target <- function(deal_won, district_name) {
  temp <- left_join(target_curr, deal_won, by = "Actual_Close")
  temp$District <- district_name
  temp[is.na(temp$Value), "Value"] <- 0
  temp$Target_TCV <- temp$Target_TCV - temp$Value
  temp <- temp %>% select(-District, -Value)
}
adj_target_curr <- net_target(gov_won, "Govt")
adj_target_curr <- net_target(can_won, "Canada")
adj_target_curr <- net_target(national_won, "National")

adj_target_funnel <- adj_target_curr %>% mutate(Planned_Close = Actual_Close, Target_Funnel = win_rate*Target_TCV) %>% select(-Actual_Close, -Target_TCV)


# now we have adjusted target. let's work on generate overall TCV first.
stacked_tcv1 <- stacked_tcv %>% group_by(Import_Date, Actual_Close) %>% summarise(Actual_TCV = sum(Value)) %>% as.data.frame() 
stacked_tcv1 <- left_join(target_curr, stacked_tcv1, by = "Actual_Close") %>% select(Import_Date, Actual_Close, Target_TCV, Actual_TCV)
stacked_tcv1$Import_Date <- last_report_date
stacked_tcv1[is.na(stacked_tcv1$Actual_TCV), "Actual_TCV"] <- 0
stacked_tcv1 <- stacked_tcv1 %>% mutate(Diff_TCV = Actual_TCV - Target_TCV)


# let's work on generate overall funnel then.filter(Import_Date == last_report_date)
stacked_funnel1 <- stacked_funnel %>% filter(Import_Date == last_report_date, Stage != "06 - Won") %>% select(-Actual_Close)
stacked_funnel1$Planned_Close <- as.yearqtr(stacked_funnel1$Planned_Close, format = "%Y%m%d")
stacked_funnel1[is.na(stacked_funnel1$Value), "Value"] <- 0
stacked_funnel1 <- stacked_funnel1 %>% group_by(Import_Date, Planned_Close) %>% summarise(Planned_TCV = sum(Value)) %>% as.data.frame() 
stacked_funnel1 <- left_join(target_funnel, stacked_funnel1, by = "Planned_Close")
stacked_funnel1$Import_Date <- last_report_date
stacked_funnel1[is.na(stacked_funnel1$Planned_TCV), "Planned_TCV"] <- 0
stacked_funnel1 <- stacked_funnel1 %>% select(Import_Date, Planned_Close, Target_Funnel, Planned_TCV)
# do somthing, because if we have won some deals, then our total funnel should go down 
temp_stacked_tcv1 <- stacked_tcv1
colnames(temp_stacked_tcv1)[2] <- "Planned_Close"
stacked_funnel1 <- left_join(stacked_funnel1,temp_stacked_tcv1, by ="Planned_Close") %>% select(-Import_Date.y, -Target_TCV, -Diff_TCV)
colnames(stacked_funnel1)[1] <- "Import_Date"
stacked_funnel1$Target_Funnel <- stacked_funnel1$Target_Funnel - stacked_funnel1$Actual_TCV*win_rate
stacked_funnel1 <- stacked_funnel1 %>% select(-Actual_TCV) %>% mutate(Diff_TCV = Planned_TCV - Target_Funnel)

# let's work on district tcv 
stacked_tcv2 <- stacked_tcv %>% filter(District != "Govt", District != "Canada", District != "National") %>% group_by(Actual_Close, District) %>% summarise(Value = sum(Value))
districts <- c("South", "MidWest", "West", "East")
qtrs <- unique(adj_target_curr$Actual_Close)
for (i in 1:length(qtrs)) {
  for (j in 1:length(districts)) {
    if (nrow(stacked_tcv2[stacked_tcv2$District == districts[j] & stacked_tcv2$Actual_Close == qtrs[i],]) == 0) {
      temp <- data.frame(Actual_Close = qtrs[i], District = districts[j], Value = 0)  
      stacked_tcv2 <- rbind(data.frame(stacked_tcv2), temp)
    }
  }
}
district_adj_target_curr <- adj_target_curr
district_adj_target_curr$Target_TCV <- district_adj_target_curr$Target_TCV / 4
stacked_tcv2 <- left_join(stacked_tcv2, district_adj_target_curr, by = "Actual_Close")
colnames(stacked_tcv2)[3] <- "Actual_TCV"
stacked_tcv2 <- stacked_tcv2 %>% mutate(Diff_TCV = Actual_TCV - Target_TCV)

# let's work on district funnel health. Question: should we exclude the won deal or not? - Yes
stacked_funnel2 <- stacked_funnel %>% filter(Import_Date == last_report_date, Stage != "06 - Won", District != "National", District != "Govt", District != "Canada") 
stacked_funnel2$Planned_Close <- as.yearqtr(stacked_funnel2$Planned_Close, format = "%Y%m%d")
stacked_funnel2 <- stacked_funnel2%>% group_by(Planned_Close, District) %>% summarise(Value = sum(Value))
for (i in 1:length(qtrs)) {
  for (j in 1:length(districts)) {
    if (nrow(stacked_funnel2[stacked_funnel2$District == districts[j] & stacked_funnel2$Planned_Close == qtrs[i],]) == 0) {
      temp <- data.frame(Planned_Close = qtrs[i], District = districts[j], Value = 0)  
      stacked_funnel2 <- rbind(data.frame(stacked_funnel2), temp)
    }
  }
}
district_adj_target_funnel <- adj_target_funnel # we need to edite sth like if MIDWEST won 20, then their funnel target would be 106/4=26.5-won 20 = 6.5 * win_rate 
district_adj_target_funnel$Target_Funnel <- district_adj_target_funnel$Target_Funnel / 4
stacked_funnel2 <- left_join(stacked_funnel2, district_adj_target_funnel, by = "Planned_Close")
colnames(stacked_funnel2)[3] <- "Planned_TCV"
# do something
temp_stacked_tcv2 <- stacked_tcv2 # we don't want to change the original stacked_tcv so, let's just create a new temp one
colnames(temp_stacked_tcv2)[1] <- "Planned_Close"
stacked_funnel2 <- left_join(stacked_funnel2, temp_stacked_tcv2, by = c("Planned_Close", "District")) %>% select(-Target_TCV, -Diff_TCV)
stacked_funnel2$Target_Funnel <- stacked_funnel2$Target_Funnel - stacked_funnel2$Actual_TCV*win_rate
stacked_funnel2 <- stacked_funnel2 %>% select(-Actual_TCV)
stacked_funnel2[stacked_funnel2$Target_Funnel < 0, "Target_Funnel"] <- 0
stacked_funnel2 <- stacked_funnel2 %>% mutate(Diff_TCV = Planned_TCV - Target_Funnel)

# get the lastest control tower report for detailed deals sheets that Preston requested
lastest_control_tower <- read.csv(filenames[nrow(filenames),1], stringsAsFactors = FALSE, na.strings=c("", NA))
clear_money <- function(price) {
  price <- gsub('[$]', '', price)
  price <- as.numeric(gsub(',', '', price))
}
lastest_control_tower$Value.in.M <- clear_money(lastest_control_tower$Value.in.M)


setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Phil/KIFCT/20190101KIFCT")
write.csv(stacked_all_cycle, "Stacked_All_Cycle.csv", row.names = FALSE) # all separate cycle time listed 
write.csv(stacked_funnel1, "Stacked_Funnel_Overall.csv",row.names = FALSE)
write.csv(stacked_funnel2, "Stacked_Funnel_District.csv", row.names = FALSE)
write.csv(stacked_tcv1, "Stacked_TCV_Overall.csv", row.names = FALSE)
write.csv(stacked_tcv2, "Stacked_TCV_District.csv", row.names = FALSE)
write.csv(stacked_idn_pene, "Stacked_IDN_Pene.csv", row.names = FALSE)
write.csv(lastest_control_tower, "Latest_ControlTower.csv", row.names = FALSE)








