# Hydro-Meteorological Database
# Data Merge Script
# Natural Hazards TWG 
# December 2020

# This script takes hydro-meteorological data and combines it with incident reports from the Bangladesh refugee camps to produce a single dataset.

# Preamble ----------------------------------------------------------------

library(rlang)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)
library(lubridate)
library(tidyquant)
library(ggplot2)

# Load in the paths to the input data sources:
source("scripts/path.R")

# Set up function for formatting dates:
clean_round_date<-function(df, date_column){
  df$clean_floored_date<-df[[date_column]] %>% trimws() %>% dmy_hm() %>% floor_date("15 mins")
}

cols_needed_for_indicent <-   c("Date.of.assessment", "Date.of.incident",
                                "Type.of.incident", 
                                "Number.of.incidents", "Affected.HHs", 
                                "Affected.individuals", "Casualties..ind.", "Missing.ind", "Injured.ind", 
                                "Displaced.HH", "Displaced.ind",
                                "Partially.Damaged.shelters", "Totally.Damaged.shelters")

# Precipitation file structure changed in June. List the two groups of files: 
precipitation_jan_to_may <- list.files(path = precipitation_folder_path,pattern = "*reporting",full.names = T)
precipitation_jun_to_nov <- list.files(path = precipitation_folder_path,pattern = "*GSB",full.names = T)

# External Precipitation Datasets -----------------------------------------

# CHIRPS dataset:
chirps_dataset <- read.csv(chirps_dataset_path, na.strings = c("NA",""," "), stringsAsFactors = F)

# start_time <- Sys.time()
# Sys.time()-start_time
  # Convert dates to year-month-day format and rename precipitation:
  chirps_dataset <- data.frame(date = dmy(chirps_dataset$system.time_start),
                               precipitataion_chirps = chirps_dataset$precipitation)

# CFRS dataset:
cfrs_dataset <- read.csv(cfsr_dataset_path, na.strings = c("NA",""," "), stringsAsFactors = F)

# What is this doing? Can we split it to be simpler?
cfrs_dataset<- cfrs_dataset %>%
  # Create a new date column with desired format:
  mutate(date = dmy(system.time_start),
         # Why is there 1/0.997?
         mm = Precipitation_rate_surface_6_Hour_Average*(1/0.997)* 60*60*6,
         # What is the purpose of these columns? Are they used again?
         timeofday = rep_len(c("00:00:00","06:00:00","12:00:00","18:00:00"), length.out = nrow(.)),
         datetime = ymd_hms(paste(date,timeofday))
  ) %>%
  # Assign time zone:
  with_tz("Asia/Dhaka") %>%
  # What does this do?
  dplyr::group_by(date) %>% dplyr::summarise(precipitation_cfrs = sum(mm, na.rm=T))


# Reformat the UNDP Rainfall Data -----------------------------------------

# Collate the first half of 2020:
precipitation_data_jan_to_may <- list()
for (i in precipitation_jan_to_may) {
  precipitation_data_jan_to_may[[i]] <- read.csv(i,na.strings = c("NA",""," "))
}
precipitation_data_jan_to_may_combind <- do.call("bind_rows", precipitation_data_jan_to_may)

# Collate the second half of 2020:
precipitation_data_jun_to_nov <- list()
rename_at_1 <- c("GSB.Cox.s.Bazaar", "GSB.Teknaf", "UN.Camp.16", "UN.Kuturc", "UN.Chakmarkul") 
rename_to_1 <- c("GSB Cox's Bazaar-1227", "GSB Teknaf-1226", "UN Camp 16-1280","UN Kuturc-1279" ,"UN Chakmarkul-1278")

for (z in precipitation_jun_to_nov) {
  # Filter out dud columns
  data <- read.csv(z, na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))
  
  # Rename columns:
  data <- setnames(data, old = rename_at_1, new = rename_to_1)
  
  # Assign to list as a collapsed tibble:
  precipitation_data_jun_to_nov[[z]] <- data %>% pivot_longer(!Date, names_to = "Device.name", values_to = "Value")
}

precipitation_data_jun_to_nov_combind <- do.call("bind_rows", precipitation_data_jun_to_nov)

# Join the two halves of the year together and:
precipitation_combined_data <- bind_rows(precipitation_data_jan_to_may_combind, precipitation_data_jun_to_nov_combind) %>% 
  # Why are we having to use distinct?
  distinct()

# Add date to the dataframe: (would perhaps be simpler to overwrite precipitation_combined_data to reduce the number of created variables)
precipitation_combined_data_with_time_date <- precipitation_combined_data %>% mutate(
  date_time = if_else(is.na(Date), ymd_hms(Time), dmy_hm(Date))
) %>% dplyr::select(-Time, Date)

# plot_rainfall_origional = plot_ly(x = precipitation_combined_data_with_time_date$date_time, y= precipitation_combined_data_with_time_date$Value, mode = "lines", type = "scatter", color = precipitation_combined_data_with_time_date$Device.name)

# Why are there problem dates? How have you found these? If we run this again for 2021 then it is useful to know the reasoning behind this. Do we know who these are happening?
problem_dates <- ymd(c("2020-04-29", "2020-04-30", "2020-05-03"))

# Change anomalous values to NA: 
precipitation_combined_data_with_time_date <- precipitation_combined_data_with_time_date %>%
  mutate(
    # Value=parse_number(Value),
    Value = ifelse((as.Date(date_time) %in% problem_dates) & (Value<10|Value>100000),NA,Value), # Why are we focussing on only 'problem dates', do not all values in this range want changing to NA?
    # Round all times down to the nearest 15 minutes:
    time_ts_round = floor_date(date_time, "15 mins")
    # If there are multiple recordings within a 15 minute period, take the mean of these:
  ) %>% select(Device.name, time_ts_round, Value) %>% dplyr::group_by(Device.name, time_ts_round) %>% 
  dplyr::summarise(
    Value = mean(Value,na.rm = T) # should we actually be taking the 1st of any duplicates, as this is the most appropriate value for the time?
  )

# plot_rainfall_NAed = plot_ly(x = precipitation_combined_data_with_time_date$time_ts_round, y= precipitation_combined_data_with_time_date$Value, mode = "lines", type = "scatter", color = precipitation_combined_data_with_time_date$Device.name)

# subplot(plot_rainfall_origional, plot_rainfall_NAed, nrows = 1)

precipitation_combined_data_with_time_date$Value <- na_if(x = precipitation_combined_data_with_time_date$Value, y = 0)
precipitation_combined_data_with_time_date$Value <- if_else(condition = is.nan(x = precipitation_combined_data_with_time_date$Value),
                                                            true = NA_real_,
                                                            false =  precipitation_combined_data_with_time_date$Value)

# Re-format the data into 15 minute intervals (non cumulative)
precipitation_full_15_min_interval <- precipitation_combined_data_with_time_date %>% 
  dplyr::group_by(Device.name) %>% 
  arrange(time_ts_round) %>% dplyr::mutate(
  Interval = Value - lag(Value)
) %>% ungroup() %>% select(c("Device.name","time_ts_round","Interval"))

# Remove negative values:
# precipitation_full_15_min_interval$Interval[precipitation_full_15_min_interval$Interval<0] = 0 # Potentially simpler
precipitation_full_15_min_interval$Interval <- if_else(precipitation_full_15_min_interval$Interval < 0, 0, precipitation_full_15_min_interval$Interval, NULL)

# precipitation_full_15_min_interval<-precipitation_data_with_interval %>% 
#   mutate(
#     # time_ts=precipitation_full3$Time  %>% dmy_hm(),
#     time_ts_round= floor_date(date_time, "15 mins")
#   ) %>% ungroup() %>% select(c( "Device.name","time_ts_round","Interval", ))



# Format UNDP rainfall dataset with columns for each gauge ---------------

pre_15_min_interval_final <- precipitation_full_15_min_interval %>% pivot_wider(names_from = Device.name ,
                                                                                names_prefix ="interval.",
                                                                                values_from = Interval,
                                                                                values_fn = sum) # This isn't very final, should we carry on overwriting things until the actual final below?

# Ensure that all 15 minute timesteps are filled:
datetime_sequence <- data.frame(
  time_date = seq(min(pre_15_min_interval_final$time_ts_round),
                  max(pre_15_min_interval_final$time_ts_round),
                  by="15 min"))

precipitation_with_all_date_time <- datetime_sequence %>% left_join(pre_15_min_interval_final, by=c("time_date"="time_ts_round"))


# 3 hr max_15_min_interval ----------------------------------------------------------------

precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time %>% 
  pivot_longer(!time_date,names_to="Device.name" , values_to = "Interval" )

precipitation_with_all_date_time_pivot_longer <- precipitation_with_all_date_time_pivot_longer %>% filter(
  time_date < max(time_date)
)#remove last date, so rolling max has full 24 hr for each day
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
include_list <- c( "Flood", "Slope-failure","Wind-Storm")

incident_report_raw_2019_to_062020 <- read.csv(incident_report_file_path_04_2019_to_062020,na.strings = c("NA",""," ")) %>% 
  dplyr::select(cols_needed_for_indicent) %>% dplyr::select(-contains("ind")) %>% filter(Type.of.incident %in% include_list)

incident_report_raw_jan_to_nov_2020 <- read.csv(incident_report_file_path_jan_2020_to_nov_2020,na.strings = c("NA",""," ")) %>% 
  dplyr::select(cols_needed_for_indicent) %>% dplyr::select(-contains("ind")) %>% filter(Type.of.incident %in% include_list)


incident_report_raw_2019_to_062020$Date.of.assessment <- incident_report_raw_2019_to_062020$Date.of.assessment %>% dmy() %>% as.Date()
incident_report_raw_jan_to_nov_2020$Date.of.assessment <- incident_report_raw_jan_to_nov_2020$Date.of.assessment  %>% dmy() %>% as.Date()

incident_report_raw_2019_to_062020$Date.of.incident <- incident_report_raw_2019_to_062020$Date.of.incident %>% dmy() %>% as.Date()
incident_report_raw_jan_to_nov_2020$Date.of.incident <- incident_report_raw_jan_to_nov_2020$Date.of.incident %>% dmy()%>% as.Date()


incident_report_raw_2019_to_062020$Date.of.assessment <- if_else(is.na(incident_report_raw_2019_to_062020$Date.of.incident),
                                                                 incident_report_raw_2019_to_062020$Date.of.assessment,
                                                                 incident_report_raw_2019_to_062020$Date.of.incident)



incident_report_raw_jan_to_nov_2020$Date.of.assessment <- if_else(is.na(incident_report_raw_jan_to_nov_2020$Date.of.incident),
                                                                  incident_report_raw_jan_to_nov_2020$Date.of.assessment,
                                                                  incident_report_raw_jan_to_nov_2020$Date.of.incident)


incident_report_raw_2019 <- incident_report_raw_2019_to_062020%>% 
  filter(Date.of.assessment<as.Date("2020/01/01"))

incident_report_raw_2019$Date.of.assessment %>% max()
incident_report_raw_jan_to_nov_2020$Date.of.assessment %>% min()


incident_report_raw <- bind_rows(incident_report_raw_2019,incident_report_raw_jan_to_nov_2020) %>% 
  select(-Date.of.incident)


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
  total_number_of_incident = rowSums(full_data[Number.of.incidents_cols],na.rm = T),
) 
  
names(full_data2) <- names(full_data2) %>% str_replace_all("interval.interval.","interval.")


# bmd_data_cox ----------------------------------------------------------------

precipitation_BMD_cox <- read.csv("inputs/BMD_Rainfall_Timeseries_COXs.csv",na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))

precipitation_BMD_cox <- precipitation_BMD_cox %>% filter(! Date %in% c("Date"),!is.na(Date),!is.na(Rainfall..mm.), Rainfall..mm.!= "T" )

precipitation_BMD_cox_data <- precipitation_BMD_cox %>%  mutate(
  first_two_character = substr(Date,1,2) %>% as.numeric(),
  only_date = if_else(first_two_character>12,substr(Date,1,2),substr(Date,4,5)),
  only_month2 = if_else(first_two_character>12,substr(Date,4,5),substr(Date,1,2)),
  only_month = str_replace_all(only_month2,"/","") %>% as.numeric(),
  date_new = paste0(only_date,"/",only_month,"/","2020") %>% dmy()
) %>% dplyr::select(-c("only_date","only_month2","only_month","Date","first_two_character")) %>% 
  setnames(old = c("date_new","Rainfall..mm."),new = c("Date","precip.BMD_COX"))



# BMD_Data_teknaf ---------------------------------------------------------
precipitation_BMD_TEK <- read.csv("inputs/BMD_Rainfall_Timeseries_TEK.csv",na.strings = c("NA",""," ")) %>% dplyr::select(-starts_with("X"))

precipitation_BMD_TEK <- precipitation_BMD_TEK %>% filter(! Date %in% c("Date"),!is.na(Date),!is.na(Rainfall..mm.))

precipitation_BMD_TEK_data <- precipitation_BMD_TEK %>%  mutate(
  first_two_character = substr(Date,1,2) %>% as.numeric(),
  only_date = if_else(first_two_character>12,substr(Date,1,2),substr(Date,4,5)),
  only_month2 = if_else(first_two_character>12,substr(Date,4,5),substr(Date,1,2)),
  only_month = str_replace_all(only_month2,"/","") %>% as.numeric(),
  date_new = paste0(only_date,"/",only_month,"/","2020") %>% dmy()
) %>% dplyr::select(-c("only_date","only_month2","only_month","Date","first_two_character")) %>% 
  setnames(old = c("date_new","Rainfall..mm."),new = c("Date","precip.BMD_TEK"))


# read_weather_data -------------------------------------------------------

#######################################msf_8W##############################
msf_8w <-read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_MSF8W_June_to_September.txt",skip = 1)
header_1_msf_8w <- read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_MSF8W_June_to_September.txt", skip = 0) %>% names()
header_2_msf_8w <- read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_MSF8W_June_to_September.txt", skip = 1) %>% names()
names(msf_8w) <- paste0(header_1_msf_8w, ".",header_2_msf_8w)

msf_8w <- msf_8w %>% distinct()
msf_8w$time_date <-  paste0(msf_8w$X.Date," ",toupper(msf_8w$X.1.Time),"M")
msf_8w$time_date <- as.POSIXct(msf_8w$time_date, format="%m/%d/%y %I:%M %p")
msf_8w <- msf_8w %>% select(time_date,Temp.Out,Hi.Temp,Low.Temp.1,Wind.Speed,Wind.1.Dir,Hi.1.Speed.1,
                            Hi.2.Dir.1,X.2.Bar,X.3.Rain,Rain.Rate,Hi.Solar.Rad..1,UV.Index.3)

msf_8w <- msf_8w %>% setnames(
  old = c("Temp.Out","Hi.Temp","Low.Temp.1","Wind.Speed","Wind.1.Dir","Hi.1.Speed.1",
          "Hi.2.Dir.1","X.2.Bar","X.3.Rain","Rain.Rate","Hi.Solar.Rad..1","UV.Index.3"),
  new = c("MSF_8_W.Temp_Out","MSF_8_W.Hi_Temp","MSF_8_W.Low_Temp","MSF_8_W.Wind_Speed","MSF_8_W.Wind_Dir","MSF_8_W.Hi_Speed",
          "MSF_8_W.Hi_Dir","MSF_8_W.Bar","MSF_8_W.Rain","MSF_8_W.Rain_Rate","MSF_8_W.Hi_Solar_Rad","MSF_8_W.UV_Index")
)

msf_8w<- msf_8w %>% filter(time_date <= max(precipitation_with_all_date_time_pivot_longer$time_date,na.rm = T))


########################################smep_hub############################
smep_hub <- read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_SMEPHub_June_to_September.txt",skip = 1) 
header_1_smep_hub <- read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_SMEPHub_June_to_September.txt", skip = 0) %>% names()
header_2_smep_hub <- read.delim("inputs/Hydrometeorological Data 2020/Weather Gagues Data _ June to September 2020/2009_SMEPHub_June_to_September.txt", skip = 1) %>% names()
names(smep_hub) <- paste0(header_1_smep_hub, ".",header_2_smep_hub)
smep_hub <- smep_hub %>% distinct()

smep_hub$time_date <-  paste0(smep_hub$X.Date," ",toupper(smep_hub$X.1.Time),"M")
smep_hub$time_date <- as.POSIXct(smep_hub$time_date, format="%m/%d/%y %I:%M %p")
smep_hub <- smep_hub %>% select(time_date,Temp.Out,Hi.Temp,Low.Temp.1,Wind.Speed,Wind.1.Dir,Hi.1.Speed.1,
                                Hi.2.Dir.1,X.2.Bar,X.3.Rain,Rain.Rate)

smep_hub <- smep_hub %>% setnames(
  old = c("Temp.Out","Hi.Temp","Low.Temp.1","Wind.Speed","Wind.1.Dir","Hi.1.Speed.1",
          "Hi.2.Dir.1","X.2.Bar","X.3.Rain","Rain.Rate"),
  new = c("SMEP_Hub_Temp.Out","SMEP_Hub.Hi_Temp","SMEP_Hub.Low_Temp","SMEP_Hub.Wind_Speed","SMEP_Hub.Wind_Dir","SMEP_Hub.Hi_Speed",
          "SMEP_Hub.Hi_Dir","SMEP_Hub.Bar","SMEP_Hub.Rain","SMEP_Hub.Rain_Rate")
)

smep_hub<- smep_hub %>% filter(time_date <= max(precipitation_with_all_date_time_pivot_longer$time_date,na.rm = T))

# hourly summary ----------------------------------------------------------

df_hourly_cal <- precipitation_with_all_date_time_pivot_longer
df_hourly_cal$datehour <- cut(df_hourly_cal$time_date, breaks="1 hour") %>% as.POSIXct()

df_hourly <- df_hourly_cal %>% dplyr::group_by(Device.name,datehour) %>% summarise(
  percipitation = if_else(all(is.na(Interval)),NA_real_,sum(Interval,na.rm = T))
)

df_hourly_pivot_wider <- df_hourly %>%  pivot_wider(names_from = "Device.name",values_from = "percipitation")

# join hourly summary with msf 8w and smep hub ----------------------------

hourly_summary <- df_hourly_pivot_wider %>% left_join(smep_hub,by = c("datehour"="time_date"))

hourly_summary <- hourly_summary %>% left_join(msf_8w,by = c("datehour"="time_date"))

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

# join_precipitation ------------------------------------------------------

full_data2<- full_data2 %>% left_join(precipitation_BMD_cox_data,by =c ("date"="Date")) #join BMD data
full_data2<- full_data2 %>% left_join(precipitation_BMD_TEK_data,by =c ("date"="Date")) #join BMD data

full_data2<- full_data2 %>% left_join(chirps_dataset) #join CHIRPS DATA
full_data2<- full_data2 %>% left_join(cfrs_dataset) #JOIN CFSR data

full_data2 <- full_data2 %>% dplyr::select(c("date", "interval.GSB Cox's Bazaar-1227", "interval.GSB Teknaf-1226", 
  "interval.UN Camp 16-1280", "interval.UN Chakmarkul-1278", "interval.UN Kuturc-1279", 
  "max_3_hr_interval.GSB Cox's Bazaar-1227", "max_3_hr_interval.GSB Teknaf-1226", 
  "max_3_hr_interval.UN Camp 16-1280", "max_3_hr_interval.UN Chakmarkul-1278", 
  "max_3_hr_interval.UN Kuturc-1279","precip.BMD_COX","precip.BMD_TEK","precipitataion_chirps",
  "precipitation_cfrs","total_number_of_incident", "Number.of.incidents_Wind-Storm", 
  "Number.of.incidents_Slope-failure", "Number.of.incidents_Flood", 
  "total_affected_hh", "Affected.HHs_Wind-Storm", 
  "Affected.HHs_Slope-failure", "Affected.HHs_Flood", "total_displaced_hh",
  "Displaced.HH_Wind-Storm", "Displaced.HH_Slope-failure", "Displaced.HH_Flood", 
   "total_partically_damaged_hh","Partially.Damaged.shelters_Wind-Storm", 
  "Partially.Damaged.shelters_Slope-failure", "Partially.Damaged.shelters_Flood", 
  "total_fully_damaged_hh", "Totally.Damaged.shelters_Wind-Storm", 
  "Totally.Damaged.shelters_Slope-failure", "Totally.Damaged.shelters_Flood" 
  ))


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
# write.csv(full_data2,"outputs/compile_dataset.csv",na = "")



# rendered value added ----------------------------------------------------

rendered_value_pivot_wider <-precipitation_combined_data_with_time_date %>% pivot_wider(names_from = "Device.name",names_prefix = "accumulated.",
                                                                                        names_sep = ".",values_from = Value)
rendered_value_pivot_wider_with_all_time_date <- datetime_sequence %>% left_join(rendered_value_pivot_wider,by=c("time_date"="time_ts_round"))

precipitation_interval_and_rendered <- precipitation_with_all_date_time %>% left_join(rendered_value_pivot_wider_with_all_time_date)

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


# write -------------------------------------------------------------------

# names(full_data2) <- names(full_data2) %>% snakecase::to_snake_case() %>% str_replace_all("_h_hs_","_hhs_")
# names(hourly_summary) <- names(hourly_summary) %>% snakecase::to_snake_case() 
# names(precipitation_interval_and_rendered) <- names(precipitation_interval_and_rendered) %>% snakecase::to_snake_case() 

list_of_datasets <- list("Hydro-Incident DB (daily)" = full_data2,
                         "Hourly Summary (weather-field)" = hourly_summary,
                         "Field Measurements (rain mm)" = precipitation_interval_and_rendered
                         )
write.xlsx(list_of_datasets, file = paste0("outputs/compile_dataset/",str_replace_all(Sys.Date(),"-",""),"_","hydromatrological_dataset",".xlsx"))

# names(full_data2) %>% as.data.frame() %>% write.csv("names.csv") 



# charts ------------------------------------------------------------------

precipitation_data_for_charts <- full_data2 %>% select(c("date", "precip.GSB Cox's Bazaar-1227", "precip.GSB Teknaf-1226", 
                                                         "precip.UN Camp 16-1280", "precip.UN Chakmarkul-1278", "precip.UN Kuturc-1279"
                                                        ))
precipitation_data_for_charts <- precipitation_data_for_charts %>% 
  setnames(old = c("precip.GSB Cox's Bazaar-1227", 
                   "precip.GSB Teknaf-1226", 
                   "precip.UN Camp 16-1280", 
                   "precip.UN Chakmarkul-1278", 
                   "precip.UN Kuturc-1279"),
           new = c("GSB Cox's Bazaar-1227", 
                   "GSB Teknaf-1226", 
                   "UN Camp 16-1280", 
                   "UN Chakmarkul-1278", 
                   "UN Kuturc-1279" ))


precipitation_data_for_charts_pivot_longer_with_NAs <- pivot_longer(precipitation_data_for_charts,cols = !date,names_to = "Device",values_to = "precipitation")
 

precipitation_data_for_charts_pivot_longer <- precipitation_data_for_charts_pivot_longer_with_NAs %>% filter(!is.na(precipitation))


precipitation_data_for_charts_pivot_longer$Month <- month(precipitation_data_for_charts_pivot_longer$date,label = T,abbr = T)

monthly_data <- precipitation_data_for_charts_pivot_longer %>% dplyr::group_by(Month, Device) %>% dplyr::summarise(
  Day_Count = n() 
)


ggplot(precipitation_data_for_charts_pivot_longer_with_NAs, aes(x=date, y=precipitation)) + 
  geom_line(method=lm,se=FALSE,linetype="solid",
              color="#ee5859",size=1.2,fullrange=TRUE,formula = y ~ x)+
  # facet_grid(~period) +
  theme(axis.title.x  = element_text(size = 24),
        axis.title.y  = element_text(size = 24),
        plot.title = element_text(lineheight = 0.9,size = 35,face = "bold"),
        plot.subtitle = element_text(lineheight = 0.7,size = 24),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 24,angle = 45,vjust = -0,hjust = -.2),
        axis.text.y = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.major.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        # panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
        # colour = "#c1c1c1"),
        
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        panel.grid.minor.x = element_line(size = 0.5, linetype = "dashed",
                             colour = "#c1c1c1"),
        panel.spacing = unit(.5,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 22,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .45,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        strip.text = element_text(size=24),
        legend.text.align = 0)+
  facet_wrap(.~Device,ncol= 3)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month",
               limits=c(as_date("2019-12-30"),as_date("2020-09-30")),expand = c(0,0))+
  ylab("Precipitation (mm)") +xlab("")+labs(title = "Daily aggregations of rain (mm) per gauge",
                                            subtitle = "Significant data gaps are acknowledged as a limitation")


ggsave(path = "outputs/charts/",filename ="precipitatiaon.jpg" ,width=40,height=20,units="cm",scale = 1.8,dpi = 100)



######################

df<-precipitation_with_all_date_time_pivot_longer
df$Device.name <- str_replace_all(df$Device.name ,
                                  "interval.","")

df <- df %>% mutate(
  time = format(time_date, format = "%H:%M:%S"),
  date = as.Date(time_date)
)

dat.summary = df %>% group_by(by8_hr=cut(time_date, "480 min"),date,Device.name) %>%
  summarise(count=sum(!is.na(Interval))) %>% ungroup() %>% mutate(
    available = if_else(count>0,"yes","no",NULL)
  )


dat.summary_2<- dat.summary %>% group_by(date,Device.name) %>%  summarise(
  useable_data = if_else(sum(available == "yes",na.rm=T)>1,1,0,NULL)) %>% ungroup() %>% mutate(
    Month = month(date,label = T,abbr = T),
    month_as_int = month(date)
  ) %>% group_by(Device.name,Month) %>% summarise(
    count= sum(useable_data)
  )

number_of_days <- data.frame(
Month = dat.summary_2$Month %>% unique(),
number_of_days = c(31,29,31,30,31,30,31,31,30))

dat.summary_2 <- dat.summary_2 %>% left_join(number_of_days) %>% mutate(
  percent = count/number_of_days*100
)
palette <- c ("#ee5859","#d1d3d4","#58585a", "#d2cbb8","#0067a9")
ggplot(dat.summary_2, aes(fill=Device.name, y=percent, x=Month)) + 
  geom_bar(position=position_dodge(.8),width = .8, stat="identity")+
  theme(axis.title.x = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(lineheight = 0.9,size = 13,face = "bold"),
        plot.subtitle = element_text(lineheight = 0.7,size = 8),
        # axis.line.x.top = element_line(),
        # axis.line.y.right = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill=NA, size=.8),
        panel.background = element_blank(),
        panel.grid.minor.x= element_line(size = 0.5, linetype = "dashed",
                                         colour = "#c1c1c1"),
        # panel.grid.major.x = element_line(size = 0.5, linetype = "dashed",
        #                                   colour = "#c1c1c1"),
        # panel.grid.minor.y= element_blank(),
        panel.grid.minor.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        # panel.border = element_rect(colour = "#58585a", fill=NA, size=1),
        panel.spacing = unit(0,"cm"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "bottom",
        legend.justification = .25,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(.9, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        plot.margin = unit(c(.1, .1, 0, 0), "cm"),
        legend.text.align = 0)+ ylab("Available days (%)")+
  scale_fill_manual(values = palette)+labs(title = "% days with sufficient measurements for daily aggregation by instrument",
                                            subtitle = "For daily aggregation, only days with measurements spanning at least 8 hours are considered sufficient")

# scale_linetype_manual(values = line_typ)

ggsave(path = "outputs/charts/",filename ="monthly_count.jpg" ,width=13,height=7,units="cm",scale = 1.8,dpi = 400)


# device percentage -------------------------------------------------------

daily_count <-dat.summary %>% group_by(Device.name,date) %>% summarise(
  count = sum(count,na.rm = T)
) 

daily_count_overall <- daily_count %>% group_by(Device.name) %>% summarise(
  total_data= n()*96,
  available_data = sum(count,na.rm = T),
  percent = available_data/total_data*100
)
