rm (list = ls())
library(rlang)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)
library(lubridate)
library(tidyquant)
source("scripts/path.R")

# clean_round_date<-function(df, date_column){
#   df$clean_floored_date<-df[[date_column]] %>% trimws() %>% dmy_hm() %>% floor_date("15 mins")
# }

rename_at_1 <- c("GSB.Cox.s.Bazaar", "GSB.Teknaf", "UN.Camp.16", "UN.Kuturc", 
                 "UN.Chakmarkul") 
rename_to_1 <- c("GSB Cox's Bazaar-1227", 
                 "GSB Teknaf-1226", "UN Camp 16-1280","UN Kuturc-1279" ,"UN Chakmarkul-1278")
cols_needed_for_indicent <-   c("Date.of.assessment", "Date.of.incident",
                                "Type.of.incident", 
                                "Number.of.incidents", "Affected.HHs", 
                                "Affected.individuals", "Casualties..ind.", "Missing.ind", "Injured.ind", 
                                "Displaced.HH", "Displaced.ind",
                                "Partially.Damaged.shelters", "Totally.Damaged.shelters")


precipitation_jan_to_may <- list.files(path = precipitation_folder_path,pattern = "*reporting",full.names = T)
precipitation_jun_to_aug <- list.files(path = precipitation_folder_path,pattern = "*GSB",full.names = T)

# other dataset -----------------------------------------------------------

chirps_dataset <- read.csv(chirps_dataset_path,na.strings = c("NA",""," "),stringsAsFactors = F)
cfrs_dataset <- read.csv(cfsr_dataset_path,na.strings = c("NA",""," "),stringsAsFactors = F)

cfrs_dataset<- cfrs_dataset %>%
  mutate(date= dmy(system.time_start),
         mm= Precipitation_rate_surface_6_Hour_Average*(1/0.997)* 60*60*6,
         tday= rep_len(c("00:00:00","06:00:00","12:00:00","18:00:00"),length.out = nrow(.)),
         datetime_char= paste(date,tday),
         datetime= ymd_hms(datetime_char)
  ) %>%
  with_tz("Asia/Dhaka") %>% dplyr::group_by(date) %>% dplyr::summarise(
    cfrs_precipitation = sum(mm,na.rm=T)
  )

# precipitation -----------------------------------------------------------

precipitation_data_jan_to_may <- list()
for (i in precipitation_jan_to_may) {
  precipitation_data_jan_to_may[[i]] <- read.csv(i,na.strings = c("NA",""," "))
}
precipitation_data_jan_to_may_combind <- do.call("bind_rows",precipitation_data_jan_to_may)


precipitation_data_jun_to_aug <- list()
for (z in precipitation_jun_to_aug) {
  data <- read.csv(z,na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))
  
  data <- setnames(data, old = rename_at_1, new = rename_to_1)
  
  precipitation_data_jun_to_aug[[z]] <- data %>% pivot_longer(!Date,names_to = "Device.name",values_to = "Value")
}
precipitation_data_jun_to_aug_combind <- do.call("bind_rows",precipitation_data_jun_to_aug)

precipitation_combined_data <- bind_rows(precipitation_data_jan_to_may_combind,precipitation_data_jun_to_aug_combind) %>% 
  distinct()


precipitation_combined_data_with_time_date <- precipitation_combined_data %>% mutate(
  date_time = if_else(is.na(Date),ymd_hms(Time),dmy_hm(Date))
) %>% dplyr::select(-Time,Date)

problem_dates<-ymd(c("2020-04-29", "2020-04-30", "2020-05-03"))

precipitation_combined_data_with_time_date<- precipitation_combined_data_with_time_date %>%
  mutate(
    only_date = as.Date(date_time),
    # Value=parse_number(Value),
    Value=ifelse((only_date  %in% problem_dates)& (Value<10|Value>100000),NA,Value),
    time_ts_round= floor_date(date_time, "15 mins")
  ) %>% select(Device.name,time_ts_round,Value) %>% dplyr::group_by(Device.name,time_ts_round) %>% 
  dplyr::summarise(
    Value = mean(Value,na.rm = T)
  )

precipitation_combined_data_with_time_date$Value <- na_if(x =  precipitation_combined_data_with_time_date$Value,y=0)
precipitation_combined_data_with_time_date$Value <- if_else(is.nan(precipitation_combined_data_with_time_date$Value),NA_real_,
                                                            precipitation_combined_data_with_time_date$Value)


precipitation_full_15_min_interval <- precipitation_combined_data_with_time_date %>% 
  dplyr::group_by(Device.name) %>% 
  arrange(time_ts_round)  %>% dplyr::mutate(
  Interval = Value - lag(Value)
)%>% ungroup() %>% select(c( "Device.name","time_ts_round","Interval"))

precipitation_full_15_min_interval$Interval <- if_else(precipitation_full_15_min_interval$Interval < 0 , 0, precipitation_full_15_min_interval$Interval, 0)

# precipitation_full_15_min_interval<-precipitation_data_with_interval %>% 
#   mutate(
#     # time_ts=precipitation_full3$Time  %>% dmy_hm(),
#     time_ts_round= floor_date(date_time, "15 mins")
#   ) %>% ungroup() %>% select(c( "Device.name","time_ts_round","Interval", ))


# pivot wider 15 min  ------------------------------------------------------------- 


pre_15_min_interval_final <- precipitation_full_15_min_interval %>% pivot_wider(names_from = Device.name ,
                                                                                names_prefix="interval.",
                                                                                values_from = Interval,
                                                                                 values_fn = sum)


# 3 hr max_15_min_interval ----------------------------------------------------------------

datetime_sequence<-data.frame(
  time_date =seq(min(pre_15_min_interval_final$time_ts_round) ,max(pre_15_min_interval_final$time_ts_round), by="15 min")
) #full data interval

precipitation_with_all_date_time <- datetime_sequence %>% left_join(pre_15_min_interval_final,by=c("time_date"="time_ts_round"))

precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time %>% 
  pivot_longer(!time_date,names_to="Device.name" , values_to = "Interval" )

precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time_pivot_longer %>% filter(
  time_date < max(time_date)
)
precip_roll_max<- precipitation_with_all_date_time_pivot_longer %>% 
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
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "max_3_hour"
  ) %>% dplyr::select(-time_date..1)


# daily_summary -----------------------------------------------------------

daily_summary_precipitation <- precip_roll_max  %>% dplyr::group_by(date,Device.name) %>% dplyr::summarise(
  interval = if_else(all(is.na(Interval)),NA_real_,sum(Interval,na.rm = T)),
  max_3_hr_interval = if_else(all(is.na(max_3_hour)),NA_real_,max(max_3_hour,na.rm=T))
)

daily_summary_precipitation$max_3_hr_interval <- if_else(is.na(daily_summary_precipitation$interval),
                                                         NA_real_,
                                                         daily_summary_precipitation$max_3_hr_interval)

preci_data_full_summary <- daily_summary_precipitation %>% 
  pivot_wider(names_from = Device.name,names_sep = ".",values_from = c("interval","max_3_hr_interval"))

preci_data_full_summary[sapply(preci_data_full_summary, is.infinite)] <- NA

# incident ----------------------------------------------------------------
include_list <- c( "Flood", "Lightning ", "Slope-failure","Wind-Storm")

incident_report_raw_2019_to_062020 <- read.csv(incident_report_file_path_04_2019_to_062020,na.strings = c("NA",""," ")) %>% 
  dplyr::select(cols_needed_for_indicent) %>% dplyr::select(-contains("ind")) %>% filter(Type.of.incident %in% include_list)

incident_report_raw_062020_to_092020 <- read.csv(incident_report_file_path_062020_to_092020,na.strings = c("NA",""," ")) %>% 
  dplyr::select(cols_needed_for_indicent) %>% dplyr::select(-contains("ind")) %>% filter(Type.of.incident %in% include_list)


incident_report_raw_2019_to_062020$Date.of.assessment <- incident_report_raw_2019_to_062020$Date.of.assessment %>% dmy() %>% as.Date()
incident_report_raw_062020_to_092020$Date.of.assessment <- incident_report_raw_062020_to_092020$Date.of.assessment %>% dmy() %>% as.Date()
incident_report_raw_2019_to_062020$Date.of.incident <- incident_report_raw_2019_to_062020$Date.of.incident %>% dmy() %>% as.Date()
incident_report_raw_062020_to_092020$Date.of.incident <- incident_report_raw_062020_to_092020$Date.of.incident %>% ymd() %>% as.Date()



incident_report_raw_2019_to_062020$Date.of.assessment <- if_else(is.na(incident_report_raw_2019_to_062020$Date.of.incident),
                                                                 incident_report_raw_2019_to_062020$Date.of.assessment,
                                                                 incident_report_raw_2019_to_062020$Date.of.incident)

incident_report_raw_062020_to_092020$Date.of.assessment <- if_else(is.na(incident_report_raw_062020_to_092020$Date.of.incident),
                                                                   incident_report_raw_062020_to_092020$Date.of.assessment,
                                                                   incident_report_raw_062020_to_092020$Date.of.incident)


incident_report_raw_062020_to_092020 <- incident_report_raw_062020_to_092020%>% 
  filter(Date.of.assessment>as.Date("2020/06/06"))

incident_report_raw_2019_to_062020$Date.of.assessment %>% max()
incident_report_raw_062020_to_092020$Date.of.assessment %>% min()


incident_report_raw <- bind_rows(incident_report_raw_2019_to_062020,incident_report_raw_062020_to_092020) %>% select(-Date.of.incident)


incident_pivot_wider <- incident_report_raw %>% 
  pivot_wider(names_from = Type.of.incident ,values_from = c("Number.of.incidents", 
                "Affected.HHs", "Displaced.HH", "Partially.Damaged.shelters", 
                 "Totally.Damaged.shelters"), names_sep = "_",values_fn = sum)  %>% filter(!is.na(Date.of.assessment))

incident_report<- incident_pivot_wider %>% group_by(Date.of.assessment) %>%summarise_each(funs(sum(., na.rm = TRUE)))
incident_report$Date <- incident_report$Date.of.assessment
incident_report <- incident_report %>% select(-Date.of.assessment)



# precipitation and incident  ---------------------------------------------

full_data <- preci_data_full_summary %>% left_join( incident_report,by = c("date"="Date")) %>% ungroup()

affected_cols <- full_data %>% dplyr::select(starts_with("affected.")) %>% colnames()
Displaced_cols <- full_data %>% dplyr::select(starts_with("Displaced.")) %>% colnames()
Partially.Damaged_cols <- full_data %>% dplyr::select(starts_with("Partially.Damaged.")) %>% colnames()
Totally.Damaged_cols <- full_data %>% dplyr::select(starts_with("Totally.Damaged.")) %>% colnames()
Number.of.incidents_cols <- full_data %>% dplyr::select(starts_with("Number.of.incidents")) %>% colnames()


full_data2 <- full_data %>% mutate(
  total_affected_hh = rowSums(full_data[affected_cols],na.rm = T),
  total_displaced_hh = rowSums(full_data[Displaced_cols],na.rm = T),
  total_partically_damaged_hh = rowSums(full_data[Partially.Damaged_cols],na.rm = T),
  total_fully_damaged_hh = rowSums(full_data[Totally.Damaged_cols],na.rm = T),
  total_number_of_incident_hh = rowSums(full_data[Number.of.incidents_cols],na.rm = T),
) 
  
names(full_data2) <- names(full_data2) %>% str_replace_all("interval.interval.","interval.")


# bmd_data ----------------------------------------------------------------

precipitation_BMD <- read.csv("inputs/Rainfall_Timeseries.csv",na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))

precipitation_BMD <- precipitation_BMD %>% filter(! Date %in% c("Date"),!is.na(Date),!is.na(Rainfall..mm.), Rainfall..mm.!= "T" )

precipitation_BMD_data <- precipitation_BMD %>%  mutate(
  first_two_character = substr(Date,1,2) %>% as.numeric(),
  only_date = if_else(first_two_character>12,substr(Date,1,2),substr(Date,4,5)),
  only_month2 = if_else(first_two_character>12,substr(Date,4,5),substr(Date,1,2)),
  only_month = str_replace_all(only_month2,"/","") %>% as.numeric(),
  date_new = paste0(only_date,"/",only_month,"/","2020") %>% dmy()
) %>% dplyr::select(-c("only_date","only_month2","only_month","Date","first_two_character")) %>% 
  setnames(old = c("date_new","Rainfall..mm."),new = c("Date","BMD_Precipitation_value"))

# join_precipitation ------------------------------------------------------

full_data2<- full_data2 %>% left_join(precipitation_BMD_data,by =c ("date"="Date"))


full_data2 <- full_data2 %>% dplyr::select(c("date", "interval.GSB Cox's Bazaar-1227", "interval.GSB Teknaf-1226", 
  "interval.UN Camp 16-1280", "interval.UN Chakmarkul-1278", "interval.UN Kuturc-1279", 
  "max_3_hr_interval.GSB Cox's Bazaar-1227", "max_3_hr_interval.GSB Teknaf-1226", 
  "max_3_hr_interval.UN Camp 16-1280", "max_3_hr_interval.UN Chakmarkul-1278", 
  "max_3_hr_interval.UN Kuturc-1279","BMD_Precipitation_value","total_number_of_incident_hh", "Number.of.incidents_Wind-Storm", 
  "Number.of.incidents_Slope-failure", "Number.of.incidents_Flood", 
  "Number.of.incidents_Lightning ","total_affected_hh", "Affected.HHs_Wind-Storm", 
  "Affected.HHs_Slope-failure", "Affected.HHs_Flood", "Affected.HHs_Lightning ", "total_displaced_hh",
  "Displaced.HH_Wind-Storm", "Displaced.HH_Slope-failure", "Displaced.HH_Flood", 
  "Displaced.HH_Lightning ", "total_partically_damaged_hh","Partially.Damaged.shelters_Wind-Storm", 
  "Partially.Damaged.shelters_Slope-failure", "Partially.Damaged.shelters_Flood", 
  "Partially.Damaged.shelters_Lightning ","total_fully_damaged_hh", "Totally.Damaged.shelters_Wind-Storm", 
  "Totally.Damaged.shelters_Slope-failure", "Totally.Damaged.shelters_Flood", 
  "Totally.Damaged.shelters_Lightning "))

# write.csv(full_data2,"outputs/compile_dataset.csv",na = "")



# rendered value added ----------------------------------------------------

rendered_value_pivot_wider <-precipitation_combined_data_with_time_date %>% pivot_wider(names_from = "Device.name",names_prefix = "accumulated.",
                                                                                        names_sep = ".",values_from = Value)
rendered_value_pivot_wider_with_all_time_date <- datetime_sequence %>% left_join(rendered_value_pivot_wider,by=c("time_date"="time_ts_round"))

precipitation_interval_and_rendered <- precipitation_with_all_date_time %>% left_join(rendered_value_pivot_wider_with_all_time_date)


# write -------------------------------------------------------------------

list_of_datasets <- list("Daily_Summary" = full_data2, "precepitation_dataset" = precipitation_interval_and_rendered)
write.xlsx(list_of_datasets, file = paste0("outputs/compile_dataset/",str_replace_all(Sys.Date(),"-",""),"_","hydromatrological_dataset",".xlsx"))

names(full_data2) %>% as.data.frame() %>% write.csv("names.csv") 

