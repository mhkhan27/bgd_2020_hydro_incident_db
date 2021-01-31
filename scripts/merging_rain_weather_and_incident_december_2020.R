# Hydro-Meteorological Database
# Data Merge Script
# Natural Hazards TWG 
# December 2020

# This script takes hydro-meteorological data and combines it with incident reports from the Bangladesh refugee camps to produce a single dataset.


rm (list = ls())

library(rlang)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)
library(lubridate)
library(tidyquant)
library(ggplot2)
source("scripts/path.R") #read all the path

weather_data <- c(T,F)[1] # To define either we have weather dataset(MSF8w and SMEP-Hub) or not
BMD_data <- c(T,F)[1]  # To define either we have BMD dataset or not
other_dataset <- c(T,F)[1] # To define either we have other dataset (CHIRPS and CFRS ) or not


rename_at_1 <- c("GSB.Cox.s.Bazaar", "GSB.Teknaf", "UN.Camp.16", "UN.Kuturc", 
                 "UN.Chakmarkul") #to rename the columns
rename_to_1 <- c("GSB Cox's Bazaar-1227", 
                 "GSB Teknaf-1226", "UN Camp 16-1280","UN Kuturc-1279" ,"UN Chakmarkul-1278")#to rename the columns

cols_needed_for_indicent <-   c( "Date.of.incident","Incident",
                                "Type.of.incident", 
                                "Number.of.incidents", "Affected.HHs", 
                                "Affected.individuals", "Casualties..ind.", "Missing.ind", "Injured.ind", 
                                "Displaced.HH", "Displaced.ind",
                                "Partially.Damaged.shelters", "Totally.Damaged.shelters",
                                "Damaged.waterpoints","Damaged.latrines", "Damaged.health.facilities" , 
                                "Damaged.food.distribution.site"  ) # to subset the necessary column from incident dataset

# File location 
precipitation_jan_to_may <- list.files(path = precipitation_folder_path,pattern = "*reporting",full.names = T)
precipitation_jun_to_end <- list.files(path = precipitation_folder_path,pattern = "*GSB",full.names = T)


# CHIRPS and CFRS dataset -----------------------------------------------------------

# CHIRPS and CFRS dataset is downloaded from google earth enginee. you can find the script here -
#https://code.earthengine.google.com/?accept_repo=users%2Fzackarno%2Fdefault&scriptPath=users%2Fzackarno%2Fdefault%3ABangladesh%2Fget_historical_precip
#you will need a google earth enginee account to run the code

if(other_dataset ==T){
  
chirps_dataset <- read.csv(chirps_dataset_path,na.strings = c("NA",""," "),stringsAsFactors = F) %>% mutate(
  date = anytime::anydate(system.time_start),
  rain.CHIRPS = round(precipitation,2)) %>% select(date,rain.CHIRPS)

cfrs_dataset <- read.csv(cfsr_dataset_path,na.strings = c("NA",""," "),stringsAsFactors = F)


#CFRS data are exported in ERA-Interim so it need to be converted into mm (for water 1l=.997kg (at 25 Degree temperature) because of water density)
# tday and with_tz(Aisha/Dhaka) lines are only for understanding how we have the cfsr data. you can ignore these lines.  
# And the it calculate per day precipitation in mm 
cfrs_dataset<- cfrs_dataset %>%
  mutate(date= anytime::anydate(system.time_start),
         mm= Precipitation_rate_surface_6_Hour_Average*(1/0.997)* 60*60*6,
         tday= rep_len(c("00:00:00","06:00:00","12:00:00","18:00:00"),length.out = nrow(.)),
         datetime_char= paste(date,tday),
         datetime= ymd_hms(datetime_char)
  ) %>%
  with_tz("Asia/Dhaka") %>% dplyr::group_by(date) %>% dplyr::summarise(
    rain.CFRS = round(sum(mm,na.rm=T),2)
  )
}
# precipitation -----------------------------------------------------------

#read_precipitation data from january to may
precipitation_data_jan_to_may <- list()
for (i in precipitation_jan_to_may) {
  precipitation_data_jan_to_may[[i]] <- read.csv(i,na.strings = c("NA",""," "))
}

#combind precipitation data from jan to may into one dataset
precipitation_data_jan_to_may_combind <- do.call("bind_rows",precipitation_data_jan_to_may) 


#read_precipitation data from Jun to end date
precipitation_data_jun_to_end<- list()
for (z in precipitation_jun_to_end) {
  data <- read.csv(z,na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))
  
  data <- setnames(data, old = rename_at_1, new = rename_to_1)
  
  precipitation_data_jun_to_end[[z]] <- data %>% pivot_longer(!Date,names_to = "Device.name",values_to = "Value")
}

#combind precipitation data from jun to last one into one dataset
precipitation_data_jun_to_end_combind <- do.call("bind_rows",precipitation_data_jun_to_end) 


#combind jan to may precipitation data with jun to end date (currently nov) precipitation data 
# And remove duplicates (using distinct() for this) observations
precipitation_combined_data <- bind_rows(precipitation_data_jan_to_may_combind,precipitation_data_jun_to_end_combind) %>% 
  distinct() 

# fix data_time formate, in dataset they are reported in 2 ways 
precipitation_combined_data_with_time_date <- precipitation_combined_data %>% mutate(
  date_time = if_else(is.na(Date),ymd_hms(Time),dmy_hm(Date))
) %>% dplyr::select(-Time,-Date) 

######################## to add raw data ###############################################

precipitation_combined_data_with_time_date_raw <- precipitation_combined_data_with_time_date %>% mutate(
  time_ts_round= floor_date(date_time, "15 mins")
) %>% select(Device.name,time_ts_round,Value) %>%  dplyr::group_by(Device.name,time_ts_round) %>% 
  dplyr::summarise(
    Value = mean(Value,na.rm = T)
  ) 

#make Nan to Na.I had to do this because when all values are NA for a specific time than mean value comes NaN
precipitation_combined_data_with_time_date_raw$Value <- if_else(is.nan(precipitation_combined_data_with_time_date_raw$Value),NA_real_,
                                                                precipitation_combined_data_with_time_date_raw$Value)

######################################################################################

# Identify problematic days. some data for these days are wrong
problem_dates<-ymd(c("2020-04-29", "2020-04-30", "2020-05-03")) 



precipitation_combined_data_with_time_date<- precipitation_combined_data_with_time_date %>%
  mutate(
    only_date = as.Date(date_time),
    # make wrong value to NA. 
    # we are focusing only problem dates as for other case it is possible to have more than 10000 value (in future) and less than 10 (as sometime UNDP reset the device). 
    # Currently in problems dates, more than 10000 and and less then 10 is very unrealistics. 
    # We needed to check the data manually before start working. so next year we should fist check the data for each device  
    quality = if_else((only_date  %in% problem_dates)& (Value>100000 | Value < 10),"Value suspected to be anomalous; removed",NA_character_), 
    Value=if_else((only_date  %in% problem_dates)& (Value>100000| Value < 10),NA_real_,Value), 
    # Round all times down to the nearest 15 minutes:
    time_ts_round= floor_date(date_time, "15 mins")
  ) 

############### separate quality column to add hourly_summary ###################
precipitation_combined_data_with_time_date_quality <- precipitation_combined_data_with_time_date %>% 
  select(Device.name,quality,time_ts_round) %>% 
  filter(!is.na(quality))
##################################################################################

precipitation_combined_data_with_time_date<- precipitation_combined_data_with_time_date %>% select(Device.name,time_ts_round,Value) %>% 
  dplyr::group_by(Device.name,time_ts_round) %>% 
  dplyr::summarise(
    Value = mean(Value,na.rm = T)
  ) 

#precipitation_combined_data_with_time_date$Value <- na_if(x =  precipitation_combined_data_with_time_date$Value,y=0)

#make Nan to Na.I had to do this because when all values are NA for a specific time than mean value comes NaN
precipitation_combined_data_with_time_date$Value <- if_else(is.nan(precipitation_combined_data_with_time_date$Value),NA_real_,
                                                            precipitation_combined_data_with_time_date$Value)

# Re-format the data into 15 minute intervals (non cumulative)
precipitation_full_15_min_interval <- precipitation_combined_data_with_time_date %>% 
  dplyr::group_by(Device.name) %>% 
  arrange(time_ts_round)  %>% dplyr::mutate(
  Interval = Value - lag(Value)
)%>% ungroup() %>% select(c( "Device.name","time_ts_round","Interval"))

#remove negetive value
precipitation_full_15_min_interval$Interval <- if_else(precipitation_full_15_min_interval$Interval < 0 , 0, precipitation_full_15_min_interval$Interval, NULL)

############################### Quality for negetive interval ##################################

quality_negetive_interval <- precipitation_full_15_min_interval %>% filter(Interval <0) %>% select(Device.name,time_ts_round) %>% 
  mutate(quality = "Value anomalous; removed.")


quality_combind  <- bind_rows(precipitation_combined_data_with_time_date_quality,quality_negetive_interval)
##################################################################################################

# pivot wider 15 min  ------------------------------------------------------------- 


pre_15_min_interval <- precipitation_full_15_min_interval %>% pivot_wider(names_from = Device.name ,
                                                                                names_prefix="interval.",
                                                                                values_from = Interval,
                                                                                 values_fn = sum)


# Ensure that all 15 minute timesteps are filled:
datetime_sequence<-data.frame(
  time_date =seq(min(pre_15_min_interval$time_ts_round) ,
                 max(pre_15_min_interval$time_ts_round), 
                 by="15 min")
)

# 3 hr max_15_min_interval ----------------------------------------------------------------

# Join precipitation interval with time sequence to ensure all timesteps are exist in dataset 
precipitation_with_all_date_time <- datetime_sequence %>% left_join(pre_15_min_interval,by=c("time_date"="time_ts_round"))

precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time %>% 
  pivot_longer(!time_date,names_to="Device.name" , values_to = "Interval" )

#remove last date, so each day has full 24 hr data
precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time_pivot_longer %>% filter(
  time_date < max(as.Date(time_date))
)

# calculate rolling sum for each 3 hour
precip_roll_sum<- precipitation_with_all_date_time_pivot_longer %>% 
 mutate(
    date = as.Date(time_date)) %>% 
  group_by(Device.name,date) %>% 
  tq_mutate(
    # tq_mutate args
    select     = Interval,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 12,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = T,
    # tq_mutate args
    col_rename = "max_3_hour"
  ) %>% dplyr::select(-time_date..1)


# daily_summary -----------------------------------------------------------

# calculate daily precipitation in mm and Maximum Sum of 3 hr precipitation in mm
daily_summary_precipitation <- precip_roll_sum  %>% dplyr::group_by(date,Device.name) %>% dplyr::summarise(
  interval = if_else(all(is.na(Interval)),NA_real_,sum(Interval,na.rm = T)),
  max_3_hr_interval = if_else(all(is.na(max_3_hour)),NA_real_,max(max_3_hour,na.rm=T))
)

daily_summary_precipitation$max_3_hr_interval <- if_else(is.na(daily_summary_precipitation$interval),
                                                         NA_real_, daily_summary_precipitation$max_3_hr_interval)
# make rolling 3 hr sum NA from 0 (initially in the daily summary 3hr rolling sum is 0 when all the values are NA for a certain day(as we used na.rm ==T during rolling sum calculation))
preci_data_full_summary <- daily_summary_precipitation %>% 
  pivot_wider(names_from = Device.name,names_sep = ".",values_from = c("interval","max_3_hr_interval"))

# Replace Inf with NAs:
preci_data_full_summary[sapply(preci_data_full_summary, is.infinite)] <- NA

# incident ----------------------------------------------------------------
year_included <- c("2020","Both")[1]


# list all hazard that need to added in hydro-incidednt dataset
include_list <- c( "Flood", "Slope-failure","Wind-Storm")

##################### read incident report from 2019 to june 2020 ############################


if (year_included == "Both") {
  
   incident_report_raw_2019_to_062020 <- read.csv(incident_report_file_path_04_2019_to_062020,
                                                  na.strings = c("NA",""," "),stringsAsFactors = F)

   incident_report_raw_2019_to_062020$Date.of.assessment <- incident_report_raw_2019_to_062020$Date.of.assessment %>% dmy() %>% as.Date()

   incident_report_raw_2019_to_062020$Date.of.incident <- incident_report_raw_2019_to_062020$Date.of.incident %>% dmy() %>% as.Date()
  
   incident_report_raw_2019_to_062020$Date.of.incident <- if_else(is.na(incident_report_raw_2019_to_062020$Date.of.incident),
                                                                    incident_report_raw_2019_to_062020$Date.of.assessment,
                                                                    incident_report_raw_2019_to_062020$Date.of.incident)


  }



#read incident report from Jan 2020 to Nov 2020
incident_report_raw_jan_to_nov_2020 <- read.csv(incident_report_file_path_jan_2020_to_nov_2020,na.strings = c("NA",""," "))

########Fix date formate
incident_report_raw_jan_to_nov_2020$Date.of.assessment <- incident_report_raw_jan_to_nov_2020$Date.of.assessment  %>% dmy() %>% as.Date()

incident_report_raw_jan_to_nov_2020$Date.of.incident <- incident_report_raw_jan_to_nov_2020$Date.of.incident %>% dmy()%>% as.Date()

# Make sure all date of incidents are recorded.( if date of incident report is missing than we assumed date of incident and date of assessment)

incident_report_raw_jan_to_nov_2020$Date.of.incident <- if_else(is.na(incident_report_raw_jan_to_nov_2020$Date.of.incident),
                                                                  incident_report_raw_jan_to_nov_2020$Date.of.assessment,
                                                                  incident_report_raw_jan_to_nov_2020$Date.of.incident)


if(year_included == "Both") {
# remove 2020s data from 2019 data
incident_report_raw_2019 <- incident_report_raw_2019_to_062020%>% 
   filter(Date.of.incident<as.Date("2020/01/01"))

#just for checking the dates 
 incident_report_raw_2019$Date.of.incident %>% max()
 incident_report_raw_jan_to_nov_2020$Date.of.incident %>% min()

# Merge 2019 and 2020 data 
incident_report_raw_to_add_in_dataset <- bind_rows(incident_report_raw_2019,incident_report_raw_jan_to_nov_2020) 
}

if(year_included != "Both") {
incident_report_raw_to_add_in_dataset <- incident_report_raw_jan_to_nov_2020 
  }


incident_report_raw_to_add_in_dataset_rm_date_of_assessment <- incident_report_raw_to_add_in_dataset %>%  select(-Date.of.assessment)%>%
  filter(Incident == "Yes") 


############################################### clean for add separate tab in dataset ################################################
final_for_raw_incident<-incident_report_raw_to_add_in_dataset_rm_date_of_assessment  %>% select(-Incident)

final_for_report_raw_incident <- final_for_raw_incident %>%
  filter(Type.of.incident %in% c(include_list,"Fire","Infrastructure","Lightning ")) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% select(-Incident.picture,-UID,-Time.of.submission)

###############################################################################################################################

# filter only include incident 
incident_report_raw <- incident_report_raw_to_add_in_dataset_rm_date_of_assessment %>% filter(Type.of.incident %in% include_list)  %>% 
  dplyr::select(cols_needed_for_indicent) %>% select(-Incident)


# Formate and summarise the incidents by each day 

wider_col<- c("Number.of.incidents", 
  "Affected.HHs", "Affected.individuals", "Casualties..ind.", "Missing.ind", 
  "Injured.ind", "Displaced.HH", "Displaced.ind", "Partially.Damaged.shelters", 
  "Totally.Damaged.shelters", "Damaged.waterpoints", "Damaged.latrines", 
  "Damaged.health.facilities", "Damaged.food.distribution.site"
)

incident_pivot_wider <- incident_report_raw %>% 
  pivot_wider(names_from = Type.of.incident ,values_from = wider_col, 
              names_sep = "_",values_fn = sum)  %>% 
  filter(!is.na(Date.of.incident))

incident_report<- incident_pivot_wider %>% group_by(Date.of.incident) %>%summarise_each(funs(sum(., na.rm = TRUE)))
incident_report$Date <- incident_report$Date.of.incident
incident_report <- incident_report %>% select(-Date.of.incident)



# precipitation and incident  ---------------------------------------------

#JOin incident dataset with precipitation dataset 

full_data <- preci_data_full_summary %>% full_join( incident_report,by = c("date"="Date")) %>% ungroup()


#Calculate total coloumns in the dataset
affected_cols_hh <- full_data %>% dplyr::select(starts_with("Affected.HHs")) %>% colnames()
affected_cols_indv <- full_data %>% dplyr::select(starts_with("Affected.individuals")) %>% colnames()

Displaced_cols_hh <- full_data %>% dplyr::select(starts_with("Displaced.HH")) %>% colnames()
Displaced_cols_indv<- full_data %>% dplyr::select(starts_with("Displaced.ind")) %>% colnames()

Partially.Damaged_cols <- full_data %>% dplyr::select(starts_with("Partially.Damaged.")) %>% colnames()
Totally.Damaged_cols <- full_data %>% dplyr::select(starts_with("Totally.Damaged.")) %>% colnames()

Damaged_waterpoint_cols <- full_data %>% dplyr::select(starts_with("Damaged.waterpoints")) %>% colnames()
Damaged_latrines_cols <- full_data %>% dplyr::select(starts_with("Damaged.latrines")) %>% colnames()
Damaged_health_facilities_cols <- full_data %>% dplyr::select(starts_with("Damaged.health.facilities")) %>% colnames()
Damaged_food_distribution_cols <- full_data %>% dplyr::select(starts_with("Damaged.food.distribution.")) %>% colnames()


Number.of.incidents_cols <- full_data %>% dplyr::select(starts_with("Number.of.incidents")) %>% colnames()

full_date <- data.frame(
  date= seq.Date(from = as.Date("2020-01-01"),to = as.Date("2020-11-30"),by = "day")
)

full_data <- full_date %>% left_join(full_data) 

full_data2 <- full_data %>% mutate(
  total_affected_hh = if_else(rowSums(is.na(full_data[affected_cols_hh]))==3,NA_real_,
                                       rowSums(full_data[affected_cols_hh],na.rm = T)),
  
  total_affected_indv = if_else(rowSums(is.na(full_data[affected_cols_indv]))==3,NA_real_,
                                rowSums(full_data[affected_cols_indv],na.rm = T)),
  ##########
  total_displaced_hh = if_else(rowSums(is.na(full_data[Displaced_cols_hh]))==3,NA_real_,
                               rowSums(full_data[Displaced_cols_hh],na.rm = T)),
  
  total_displaced_indv = if_else(rowSums(is.na(full_data[Displaced_cols_indv]))==3,NA_real_,
                                 rowSums(full_data[Displaced_cols_indv],na.rm = T)),
  
  total_partically_damaged = if_else(rowSums(is.na(full_data[Partially.Damaged_cols]))==3,NA_real_,
                                     rowSums(full_data[Partially.Damaged_cols],na.rm = T)),
                                     
  total_fully_damaged = if_else(rowSums(is.na(full_data[Totally.Damaged_cols]))==3,NA_real_,
                                rowSums(full_data[Totally.Damaged_cols],na.rm = T)),
  
  Damaged_waterpoint.Total = if_else(rowSums(is.na(full_data[Damaged_waterpoint_cols]))==3,NA_real_,
                                     rowSums(full_data[Damaged_waterpoint_cols],na.rm = T)),
                                     
  Damaged_latrines.Total =if_else(rowSums(is.na(full_data[Damaged_latrines_cols]))==3,NA_real_,
                                  rowSums(full_data[Damaged_latrines_cols],na.rm = T)),
                                  
  Damaged_health_facilities.Total=if_else(rowSums(is.na(full_data[Damaged_health_facilities_cols]))==3,NA_real_,
                                          rowSums(full_data[Damaged_health_facilities_cols],na.rm = T)),
                                          
  Damaged_food_distribution.Total =if_else(rowSums(is.na(full_data[Damaged_food_distribution_cols]))==3,NA_real_,
                                           rowSums(full_data[Damaged_food_distribution_cols],na.rm = T)),
  
  total_number_of_incident = if_else(rowSums(is.na(full_data[Number.of.incidents_cols]))==3,NA_real_,
                                     rowSums(full_data[Number.of.incidents_cols],na.rm = T)),
) 


  
names(full_data2) <- names(full_data2) %>% str_replace_all("interval.interval.","interval.")


if(BMD_data == T){
  
# bmd_data_cox ----------------------------------------------------------------

# Read BMD data for Cox's Bazar
precipitation_BMD_cox <- read.csv("inputs/BMD_data/BMD Rainfall_Timeseries_cox_May_to_nov.csv",na.strings = c("NA",""," ")) %>%
  dplyr::select(-starts_with("X"))

# Fix formating
precipitation_BMD_cox <- precipitation_BMD_cox %>% filter(! Date %in% c("Date"),
                                                          !is.na(Date),!is.na(Rainfall..mm.)) %>% select(Date,Rainfall..mm.)

# Fix date formating in the dataset 
precipitation_BMD_cox_data <- precipitation_BMD_cox %>%  mutate(
  first_two_character = substr(Date,1,2) %>% as.numeric(),
  only_date = if_else(first_two_character>12,substr(Date,1,2),substr(Date,4,5)),
  only_month2 = if_else(first_two_character>12,substr(Date,4,5),substr(Date,1,2)),
  only_month = str_replace_all(only_month2,"/","") %>% as.numeric(),
  date_new = paste0(only_date,"/",only_month,"/","2020") %>% dmy()
) %>% dplyr::select(-c("only_date","only_month2","only_month","Date","first_two_character")) %>% 
  setnames(old = c("date_new","Rainfall..mm."),new = c("Date","rain.BMD_COX"))  %>% 
  mutate(
    quality.BMD_COX = case_when(rain.BMD_COX == "T"~"Trace",
                                T~ NA_character_),
    rain.BMD_COX = if_else(rain.BMD_COX == "T", .5, as.numeric(rain.BMD_COX))
  )



# BMD_Data_teknaf ---------------------------------------------------------

#Read data for Teknaf
precipitation_BMD_TEK <- read.csv("inputs/BMD_data/BMD Rainfall_Timeseries_tek_May_to_nov.csv",na.strings = c("NA",""," ")) %>% 
  dplyr::select(-starts_with("X"))

# Fix formating
precipitation_BMD_TEK <- precipitation_BMD_TEK %>% filter(! Date %in% c("Date"),!is.na(Date),!is.na(Rainfall..mm.)) %>% 
  select(Date,Rainfall..mm.)

# Fix date formating in the dataset 
precipitation_BMD_TEK_data <- precipitation_BMD_TEK %>%  mutate(
  first_two_character = substr(Date,1,2) %>% as.numeric(),
  only_date = if_else(first_two_character>12,substr(Date,1,2),substr(Date,4,5)),
  only_month2 = if_else(first_two_character>12,substr(Date,4,5),substr(Date,1,2)),
  only_month = str_replace_all(only_month2,"/","") %>% as.numeric(),
  date_new = paste0(only_date,"/",only_month,"/","2020") %>% dmy()
) %>% dplyr::select(-c("only_date","only_month2","only_month","Date","first_two_character")) %>% 
  setnames(old = c("date_new","Rainfall..mm."),new = c("Date","rain.BMD_TEK"))  %>% 
  mutate(
    quality.BMD_TEK = if_else(rain.BMD_TEK == "T","Trace",NULL,NULL),
    rain.BMD_TEK = if_else(rain.BMD_TEK == "T", .5, as.numeric(rain.BMD_TEK))
  )

}

if(weather_data ==T){

# read_weather_data -------------------------------------------------------

#######################################msf_8W##############################

  # Read MSF8W weather station data 
msf_8w <-read.delim("inputs/weather_dataset/2009_MSF8W_June_to_September.txt",skip = 1)


# Fix Column name 
header_1_msf_8w <- read.delim("inputs/weather_dataset/2009_MSF8W_June_to_September.txt", skip = 0) %>% names()
header_2_msf_8w <- read.delim("inputs/weather_dataset/2009_MSF8W_June_to_September.txt", skip = 1) %>% names()
names(msf_8w) <- paste0(header_1_msf_8w, ".",header_2_msf_8w)

# Remove Duplicate data
msf_8w <- msf_8w %>% distinct()

# Fix date formate
msf_8w$time_date <-  paste0(msf_8w$X.Date," ",toupper(msf_8w$X.1.Time),"M")

#convert to posixct and add 12 (as data is wrong)
msf_8w$time_date <- as.POSIXct(msf_8w$time_date, format="%m/%d/%y %I:%M %p")+43200
msf_8w <- msf_8w %>% select(time_date,Temp.Out,Hi.Temp,Low.Temp.1,Wind.Speed,Wind.1.Dir,Hi.1.Speed.1,
                            Hi.2.Dir.1,X.2.Bar,X.3.Rain,Rain.Rate,Hi.Solar.Rad..1,UV.Index.3)

#Rename column name
msf_8w <- msf_8w %>% setnames(
  old = c("Temp.Out","Hi.Temp","Low.Temp.1","Wind.Speed","Wind.1.Dir","Hi.1.Speed.1",
          "Hi.2.Dir.1","X.2.Bar","X.3.Rain","Rain.Rate","Hi.Solar.Rad..1","UV.Index.3"),
  new = c("MSF_8_W.Temp_Out","MSF_8_W.Hi_Temp","MSF_8_W.Low_Temp","MSF_8_W.Wind_Speed","MSF_8_W.Wind_Dir","MSF_8_W.Hi_Speed",
          "MSF_8_W.Hi_Dir","MSF_8_W.Bar","MSF_8_W.Rain","MSF_8_W.Rain_Rate","MSF_8_W.Hi_Solar_Rad","MSF_8_W.UV_Index")
)

# Filter out old data
msf_8w<- msf_8w %>% filter(time_date <= max(final_for_report_raw_incident$Date.of.incident,na.rm = T))


msf_8w<- msf_8w %>% mutate_at(names(msf_8w %>% select(-time_date)),list(~na_if(.,"---")))


########################################smep_hub############################
# Read and fix column name of SMEP-Hub weather station data 
smep_hub <- read.delim("inputs/weather_dataset/2009_SMEPHub_June_to_September.txt",skip = 1) 
header_1_smep_hub <- read.delim("inputs/weather_dataset/2009_SMEPHub_June_to_September.txt", skip = 0) %>% names()
header_2_smep_hub <- read.delim("inputs/weather_dataset/2009_SMEPHub_June_to_September.txt", skip = 1) %>% names()
names(smep_hub) <- paste0(header_1_smep_hub, ".",header_2_smep_hub)

# Remove duplicate data
smep_hub <- smep_hub %>% distinct()

# Fix date formatting
smep_hub$time_date <-  paste0(smep_hub$X.Date," ",toupper(smep_hub$X.1.Time),"M")
smep_hub$time_date <- as.POSIXct(smep_hub$time_date, format="%m/%d/%y %I:%M %p")
smep_hub <- smep_hub %>% select(time_date,Temp.Out,Hi.Temp,Low.Temp.1,Wind.Speed,Wind.1.Dir,Hi.1.Speed.1,
                                Hi.2.Dir.1,X.2.Bar,X.3.Rain,Rain.Rate)

# Rename column name
smep_hub <- smep_hub %>% setnames(
  old = c("Temp.Out","Hi.Temp","Low.Temp.1","Wind.Speed","Wind.1.Dir","Hi.1.Speed.1",
          "Hi.2.Dir.1","X.2.Bar","X.3.Rain","Rain.Rate"),
  new = c("SMEP_Hub_Temp.Out","SMEP_Hub.Hi_Temp","SMEP_Hub.Low_Temp","SMEP_Hub.Wind_Speed","SMEP_Hub.Wind_Dir","SMEP_Hub.Hi_Speed",
          "SMEP_Hub.Hi_Dir","SMEP_Hub.Bar","SMEP_Hub.Rain","SMEP_Hub.Rain_Rate")
)

# Filter out old data
smep_hub<- smep_hub %>% filter(time_date <= max(final_for_report_raw_incident$Date.of.incident,na.rm = T))
}

# smep_hub <-  smep_hub %>% mutate(
#   SMEP_Hub.Bar = if_else(SMEP_Hub.Bar == "------",NA_character_,as.character(SMEP_Hub.Bar))
#   
# )

smep_hub<- smep_hub %>% mutate_at(names(smep_hub %>% select(-time_date)),list(~na_if(.,"------")))
smep_hub<- smep_hub %>% mutate_at(names(smep_hub %>% select(-time_date)),list(~na_if(.,"---")))

# hourly summary ----------------------------------------------------------

# Calculate hourly summary of precipitation data
df_hourly_cal <- precipitation_with_all_date_time_pivot_longer
df_hourly_cal$datehour <- cut(df_hourly_cal$time_date, breaks="1 hour") %>% as.POSIXct()

df_hourly <- df_hourly_cal %>% dplyr::group_by(Device.name,datehour) %>% summarise(
  percipitation = if_else(all(is.na(Interval)),NA_real_,sum(Interval,na.rm = T))
)

df_hourly_pivot_wider <- df_hourly %>%  pivot_wider(names_from = "Device.name",values_from = "percipitation")


# join hourly summary with weather data ----------------------------

if(weather_data == T){
hourly_summary <- df_hourly_pivot_wider %>% left_join(smep_hub,by = c("datehour"="time_date"))

hourly_summary <- hourly_summary %>% left_join(msf_8w,by = c("datehour"="time_date"))

######################################### add quality columns to weather_dataset ################################################

hourly_summary <- hourly_summary %>% mutate(
  quality.MSF_8_w_Hub = case_when(datehour < as.Date("2020-06-09") & is.na(MSF_8_W.Hi_Temp)~"-",
                               datehour > as.Date("2020-09-22") & is.na(MSF_8_W.Hi_Temp)~"Data were not collected due to COVID 19",
                               is.na(MSF_8_W.Hi_Temp)~ "missing",
                               T~ NA_character_),
  
  quality.SMEP_Hub =case_when(datehour < as.Date("2020-06-09") & is.na(SMEP_Hub.Low_Temp)~"-",
                                         datehour > as.Date("2020-09-22") & is.na(SMEP_Hub.Low_Temp)~"Data were not collected due to COVID 19",
                                         is.na(SMEP_Hub.Low_Temp)~ "missing",
                                         T~ NA_character_),
) %>% select(datehour,
             starts_with("inter"),
             quality.SMEP_Hub,
             starts_with("SMEP_Hub."),
             SMEP_Hub_Temp.Out,
             quality.MSF_8_w_Hub,
             starts_with("MSF_8_W."),
             everything())

}

if(weather_data == F){
  hourly_summary <- df_hourly_pivot_wider 
  }

# Rename column names
hourly_summary <- hourly_summary %>% setnames(old = c("interval.GSB Cox's Bazaar-1227", 
                                    "interval.GSB Teknaf-1226", 
                                    "interval.UN Camp 16-1280", 
                                    "interval.UN Chakmarkul-1278", 
                                    "interval.UN Kuturc-1279"),
                            new = c("precip.GSB Cox's Bazaar-1227", 
                                    "precip.GSB Teknaf-1226", 
                                    "precip.UN Camp 16-1280", 
                                    "precip.UN Chakmarkul-1278", 
                                    "precip.UN Kuturc-1279"))

# join_precipitation_data with BMD, CHIRPS and CFSR data ------------------------------------------------------
if(BMD_data == T){
full_data2<- full_data2 %>% left_join(precipitation_BMD_cox_data,by =c ("date"="Date")) #join BMD data
full_data2<- full_data2 %>% left_join(precipitation_BMD_TEK_data,by =c ("date"="Date")) #join BMD data



full_data2 <- full_data2 %>%  mutate(
 quality.BMD_COX = case_when(quality.BMD_COX == "Trace"~"Trace",
                             date < as.Date("2020-05-01") ~ "-",
                             is.na(rain.BMD_COX)~ "Missing data",
                             T~ NA_character_),
 quality.BMD_TEK = case_when(quality.BMD_TEK == "Trace"~"Trace",
                             date < as.Date("2020-05-01") ~ "-",
                             is.na(rain.BMD_TEK)~ "Missing data",
                             T~ NA_character_)
 )
}

if(other_dataset ==T){
  
full_data2<- full_data2 %>% left_join(chirps_dataset) #join CHIRPS DATA
full_data2<- full_data2 %>% left_join(cfrs_dataset) #JOIN CFSR data

}

names(full_data2) <-names(full_data2) %>% str_replace_all("max_3_hr_interval.","rain_max_3_hr_interval.")
# Reported Columns 
all_col_name_for_full_dataset <- c("date", "interval.GSB Cox's Bazaar-1227", "interval.GSB Teknaf-1226", 
                                   "interval.UN Camp 16-1280", "interval.UN Chakmarkul-1278", "interval.UN Kuturc-1279", 
                                   "rain_max_3_hr_interval.GSB Cox's Bazaar-1227", "rain_max_3_hr_interval.GSB Teknaf-1226", 
                                   "rain_max_3_hr_interval.UN Camp 16-1280", "rain_max_3_hr_interval.UN Chakmarkul-1278", 
                                   "rain_max_3_hr_interval.UN Kuturc-1279","rain.BMD_COX","quality.BMD_COX","rain.BMD_TEK","quality.BMD_TEK",
                                   "rain.CHIRPS",
                                   "rain.CFRS","total_number_of_incident", "Number.of.incidents_Wind-Storm", 
                                   "Number.of.incidents_Slope-failure", "Number.of.incidents_Flood", 
                                   "total_affected_hh", "Affected.HHs_Wind-Storm", 
                                   "Affected.HHs_Slope-failure", "Affected.HHs_Flood", 
                                   "total_affected_indv",affected_cols_indv,"total_displaced_hh",
                                   "Displaced.HH_Wind-Storm", "Displaced.HH_Slope-failure", "Displaced.HH_Flood", 
                                   "total_displaced_indv",Displaced_cols_indv,
                                   "total_partically_damaged","Partially.Damaged.shelters_Wind-Storm", 
                                   "Partially.Damaged.shelters_Slope-failure", "Partially.Damaged.shelters_Flood", 
                                   "total_fully_damaged", "Totally.Damaged.shelters_Wind-Storm", 
                                   "Totally.Damaged.shelters_Slope-failure", "Totally.Damaged.shelters_Flood",
                                   "Damaged_waterpoint.Total",Damaged_waterpoint_cols,
                                   "Damaged_latrines.Total",Damaged_latrines_cols,
                                   "Damaged_health_facilities.Total",Damaged_health_facilities_cols,
                                   "Damaged_food_distribution.Total",Damaged_food_distribution_cols
)


# Reported cols will be change if we are missing one of data sets (weather data/BDM data etc....). 
# The following line will make sure all the reported columns are exist in dataset
col_names_available_for_final_dataset <- all_col_name_for_full_dataset[all_col_name_for_full_dataset %in% names(full_data2)]

# Selet only necessary column 
full_data2 <- full_data2 %>% dplyr::select(col_names_available_for_final_dataset)

# Rename columns
full_data2 <- full_data2 %>% 
  setnames(old = c("interval.GSB Cox's Bazaar-1227", 
                   "interval.GSB Teknaf-1226", 
                   "interval.UN Camp 16-1280", 
                   "interval.UN Chakmarkul-1278", 
                   "interval.UN Kuturc-1279"),
           new = c("precip.GSB Cox's Bazaar-1227", 
                   "precip.GSB Teknaf-1226", 
                   "precip.UN Camp 16-1280", 
                   "precip.UN Chakmarkul-1278", 
                   "precip.UN Kuturc-1279"))


# Adding rendered value (accumulated value) to the dataset (raw) ----------------------------------------------------

rendered_value_pivot_wider <-precipitation_combined_data_with_time_date_raw %>% pivot_wider(names_from = "Device.name",
                                                                                        names_prefix = "accumulated.",
                                                                                        names_sep = ".",values_from = Value)
rendered_value_pivot_wider_with_all_time_date <- datetime_sequence %>% left_join(rendered_value_pivot_wider,by=c("time_date"="time_ts_round"))

precipitation_interval_and_rendered <- precipitation_with_all_date_time %>% 
  left_join(rendered_value_pivot_wider_with_all_time_date)

precipitation_interval_and_rendered <- precipitation_interval_and_rendered %>% 
  setnames(old = c("interval.GSB Cox's Bazaar-1227", 
                   "interval.GSB Teknaf-1226", 
                   "interval.UN Camp 16-1280", 
                   "interval.UN Chakmarkul-1278", 
                   "interval.UN Kuturc-1279"),
           new = c("precip.GSB Cox's Bazaar-1227", 
                   "precip.GSB Teknaf-1226", 
                   "precip.UN Camp 16-1280", 
                   "precip.UN Chakmarkul-1278", 
                   "precip.UN Kuturc-1279"))


####### Add quality column to field measures ################
quality_combind_pivot_wider <- quality_combind %>% pivot_wider(names_from = "Device.name" , 
                                                               values_from = "quality",
                                                               names_prefix = "quality.",)


precipitation_interval_and_rendered <- precipitation_interval_and_rendered %>% left_join(quality_combind_pivot_wider, 
                                                                                         by = c("time_date"="time_ts_round"))

##### Add NA quality column where there is not quality issues #############

quality_cols <- c("quality.GSB Cox's Bazaar-1227", 
                  "quality.GSB Teknaf-1226", 
                  "quality.UN Camp 16-1280", 
                  "quality.UN Chakmarkul-1278", 
                  "quality.UN Kuturc-1279")


non_existing_cols <- quality_cols[!quality_cols %in% names(precipitation_interval_and_rendered)]

for (i in non_existing_cols) {
print(i)  
precipitation_interval_and_rendered[i] <-NA_character_
}

#### add a warning where there is data accumulated across a gap #################### 

source("scripts/add_warning.r")


####################################################### rename columns #############################################################

cols_name_input_df <- read.csv("inputs/renaming/reported_column_name.csv",na.strings = c(""," "),
                               stringsAsFactors = F) %>% filter(!is.na(new_name)) %>% filter(!is.na(old_name))


# Hydro_incident_dataset --------------------------------------------------
cols_name_input_hydro_incident <- cols_name_input_df %>% filter(dataset == "Hydro-incident dataset" ) %>% 
  select(old_name,new_name) %>% mutate(
    tf= old_name %in% names(full_data2)
  )

names(full_data2)[match(cols_name_input_hydro_incident[,"old_name"], 
                                   names(full_data2))] = cols_name_input_hydro_incident[,"new_name"]


# hourly summary ---------------------------------------
cols_name_input_hourly_summary <- cols_name_input_df %>% filter(dataset == "Hourly Summary" ) %>% select(old_name,new_name)

names(hourly_summary)[match(cols_name_input_hourly_summary[,"old_name"], 
                        names(hourly_summary))] = cols_name_input_hourly_summary[,"new_name"]


# field measurement --------------------------------------------------------

cols_name_input_field_measurement<- cols_name_input_df %>% filter(dataset == "Field Measurements" ) %>% select(old_name,new_name)

names(precipitation_interval_and_rendered)[match(cols_name_input_field_measurement[,"old_name"], 
                            names(precipitation_interval_and_rendered))] = cols_name_input_field_measurement[,"new_name"]


######################################################################################################################################


# add missing in quality column -------------------------------------------

precipitation_interval_and_rendered <- precipitation_interval_and_rendered %>% mutate(
  `quality.UN Kuturc-1279` = if_else(is.na(`quality.UN Kuturc-1279` ) & is.na(`cumulative_rain.UN Kuturc-1279`),"Missing",`quality.UN Kuturc-1279` ),
  `quality.UN Chakmarkul-1278`=if_else(is.na(`quality.UN Chakmarkul-1278`) & is.na(`cumulative_rain.UN Chakmarkul-1278`),"Missing",`quality.UN Chakmarkul-1278`),
  `quality.UN Camp 16-1280`=if_else(is.na(`quality.UN Camp 16-1280`) & is.na(`cumulative_rain.UN Camp 16-1280`),"Missing",`quality.UN Camp 16-1280`),
  `quality.GSB Cox's Bazaar-1227`=if_else(is.na(`quality.GSB Cox's Bazaar-1227`) & is.na(`cumulative_rain.GSB Cox's Bazaar-1227`),"Missing",`quality.GSB Cox's Bazaar-1227`),
  `quality.GSB Teknaf-1226`=if_else(is.na( `quality.GSB Teknaf-1226`) & is.na(`cumulative_rain.GSB Teknaf-1226`),"Missing", `quality.GSB Teknaf-1226`)

) %>% select( date_time,
  starts_with("cum"),
  `quality.GSB Cox's Bazaar-1227`,
  `quality.GSB Teknaf-1226`,
  `quality.UN Camp 16-1280`,
  `quality.UN Chakmarkul-1278`,
  `quality.UN Kuturc-1279`,
  `rain.GSB Cox's Bazaar-1227`,
  `rain.GSB Teknaf-1226`,
  `rain.UN Camp 16-1280`,
  `rain.UN Chakmarkul-1278`,
  `rain.UN Kuturc-1279`,
  everything())


full_data2_cols_arrange <- full_data2 %>% names()
################################################### daily weather_data #############################################################
source("scripts/daily_weather.R")

full_data2 <- full_data2 %>% left_join(weather_dataset_daily_data)

full_data2 <- full_data2 %>% mutate(
  MSF_8W.Quality = case_when(date < as.Date("2020-06-09") & is.na(MSF_8_W.Hi_Temp)~"-",
                                  date > as.Date("2020-09-22") & is.na(MSF_8_W.Hi_Temp)~"Data were not collected due to COVID 19",
                                  is.na(MSF_8_W.Hi_Temp)~ "missing",
                                  T~ NA_character_),

  SMEP.Quality =case_when(date < as.Date("2020-06-09") & is.na(SMEP_Hub.Low_Temp)~"-",
                              date > as.Date("2020-09-22") & is.na(SMEP_Hub.Low_Temp)~"Data were not collected due to COVID 19",
                              is.na(SMEP_Hub.Low_Temp)~ "missing",
                              T~ NA_character_),
) %>% select(full_data2_cols_arrange,
             SMEP.Quality,
             starts_with("SMEP_Hub."),
             SMEP_Hub_Temp.Out,
             MSF_8W.Quality,
             starts_with("MSF_8_W."),
             everything()) %>% 
  select(-SMEP_Hub.Rain_Rate,-MSF_8_W.Rain_Rate) %>% rename("SMEP_Hub.Temp_Out"="SMEP_Hub_Temp.Out")

full_data2_for_percentage <- full_data2 # to calculate the percentage

full_data2 <-full_data2 %>%  select(-SMEP.Quality,-MSF_8W.Quality)

#####################################################################################################################################
#arrange cols
# hourly_summary <- hourly_summary %>% select(
#   date_time,
#   starts_with("rain."),
#   SMEP.Quality,
#   starts_with("SMEP"),
#   MSF_8W.Quality,
#   starts_with("MSF"),
#   everything()
# )

final_for_report_raw_incident <- final_for_report_raw_incident %>% select(Date.of.incident,everything())

final_for_report_raw_incident <- final_for_report_raw_incident %>% arrange(Date.of.incident)
final_for_report_raw_incident %>% class()


names(final_for_report_raw_incident) <- names(final_for_report_raw_incident) %>% stringr::str_replace_all("\\.","_") %>% 
  str_replace_all("__","_") %>% str_replace_all("_other_","_other") %>% 
  str_replace_all("_ind_","_ind") %>% str_replace_all("_Other_","_other") %>% 
  str_replace_all("Problems_require_intervention_","Problems_require_intervention") %>% 
  str_replace_all("Protection_concern_","Protection_concern") %>% 
  str_replace_all("Can_needs_be_managed_in_the_site_","Can_needs_be_managed_in_the_site")

final_for_report_raw_incident <- final_for_report_raw_incident %>% select(-starts_with("GPS"),-Organization)
names(final_for_report_raw_incident)<- names(final_for_report_raw_incident) %>% trimws()

final_for_report_raw_incident <- final_for_report_raw_incident %>% 
  select(-starts_with("Required_response_"),
         -starts_with("Response_delivered"),
         -starts_with("Displacement_refuge"),
         -starts_with("Traffic"),
         -Type_of_incident_other,
         -Location_access,
         -Problems_require_intervention,
         -Protection_concern,
         -Can_needs_be_managed_in_the_site,
         -Additional_comments)

######################################################################################################################################


source("scripts/write_work_book.R") #to write the datasets with specific format

source("scripts/graphs_for_dataset.R") #to export the graphs 

# device percentage raw data -------------------------------------------------------

daily_count <-dat.summary %>% group_by(Device.name,date) %>% summarise(
  count = sum(count,na.rm = T)
) 

# we should have 96 ( 24*60/15) data each day for each device, so to calculate available data we need to multipy number of days with 96  
daily_count_overall <- daily_count %>% group_by(Device.name) %>% summarise(
  total_data= n()*96,
  available_data = sum(count,na.rm = T),
  percent = available_data/total_data*100
)


# Available data (daily) --------------------------------------------------

df_daily <- full_data2_for_percentage %>% select(starts_with("rain."),
                                  contains("CHIRPS"),
                                  contains("CFRS"),
                                  SMEP.Quality,
                                  MSF_8W.Quality)


devices_percentage <- data.table(
  GSB_Coxs_Bazaar_1227 = sum(!is.na(df_daily$`rain.GSB Cox's Bazaar-1227`)==T)/nrow(df_daily)*100,
  GSB_Teknaf_1226 = sum(!is.na(df_daily$`rain.GSB Teknaf-1226`)==T)/nrow(df_daily)*100,
  UN_Camp_16_1280 = sum(!is.na(df_daily$`rain.UN Camp 16-1280`)==T)/nrow(df_daily)*100,
  UN_Chakmarkul_1278 = sum(!is.na(df_daily$`rain.UN Chakmarkul-1278`)==T)/nrow(df_daily)*100,
  UN_Kuturc_1279 = sum(!is.na(df_daily$`rain.UN Kuturc-1279`)==T)/nrow(df_daily)*100,
  
  BMD_cox = sum(!is.na(df_daily$`rain.BMD Cox's Bazaar`)==T)/nrow(df_daily)*100,
  BMD_Teknaf = sum(!is.na(df_daily$`rain.BMD Teknaf`)==T)/nrow(df_daily)*100,
  
  cHIRPS = sum(!is.na(df_daily$rain.CHIRPS)==T)/nrow(df_daily)*100,
  CFRS = sum(!is.na(df_daily$rain.CFRS)==T)/nrow(df_daily)*100,
  
  SMEP = sum(is.na(df_daily$SMEP.Quality)==T)/nrow(df_daily)*100,
  MSF_8W = sum(is.na(df_daily$MSF_8W.Quality)==T)/nrow(df_daily)*100
) %>% mutate_all(.,function(x){x<-round(x)})


# data_key_name -----------------------------------------------------------
# 
# 
# hydro_data_col <- data.table(`Column Name`= names(full_data2),
#                          Dataset = "Hydro-incident dataset")
# 
# hourly_summary_col <- data.table(`Column Name` = names(hourly_summary),
#                          Dataset = "Weather Gauges (hourly)")
# 
# precipitation_interval_and_rendered_col <- data.table(`Column Name` = names(precipitation_interval_and_rendered),
#                                  Dataset = "Rain Gauges (sub-hourly)")
# 
# 
# incident_report_col <- data.table(`Column Name` = names(final_for_report_raw_incident),
#                                                       Dataset = "Raw Incident Records")
# 
# final_data_key_cols <- bind_rows(hydro_data_col,hourly_summary_col,precipitation_interval_and_rendered_col,incident_report_col) %>% mutate(
#   Description = NA_character_
# ) %>% select(`Column Name`,Description,Dataset)
#  write.csv(final_data_key_cols,"outputs/formatting/Data Key.csv",na = "")

#####################################################################################################################################
