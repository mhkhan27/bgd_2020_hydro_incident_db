rm (list = ls())
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)
library(lubridate)
source("scripts/path.R")


# cols --------------------------------------------------------------------

cols_needed_for_indicent <-   c("Date.of.assessment", 
                                "Type.of.incident", 
                                "Number.of.incidents", "Affected.HHs", 
                                "Affected.individuals", "Casualties..ind.", "Missing.ind", "Injured.ind", 
                                "Displaced.HH", "Displaced.ind",
                                "Partially.Damaged.shelters", "Totally.Damaged.shelters", "Damaged.waterpoints", 
                                "Damaged.latrines", "Damaged.health.facilities", "Damaged.food.distribution.site")

rename_at_1 <- c("GSB.Cox.s.Bazaar", "GSB.Teknaf", "UN.Camp.16", "UN.Kuturc", 
               "UN.Chakmarkul") 
rename_to_1 <- c("GSB Cox's Bazaar-1227", 
                 "GSB Teknaf-1226", "UN Camp 16-1280","UN Kuturc-1279" ,"UN Chakmarkul-1278")


full_data_order <- c("Date", "GSB Cox's Bazaar-1227", "UN Camp 16-1280", "UN Chakmarkul-1278", 
                     "GSB Teknaf-1226", "UN Kuturc-1279","BMD_Precipitation_value", "Wind-Storm", "Drowning", "Slope-failure", 
                     "Fire", "Traffic","Infrastructure", "Flood", "Lightning ", "Affected.HHs", 
                     "Affected.individuals", "Casualties..ind.", "Missing.ind", "Injured.ind", 
                     "Displaced.HH", "Displaced.ind", "Partially.Damaged.shelters", 
                     "Totally.Damaged.shelters", "Damaged.waterpoints", "Damaged.latrines", 
                     "Damaged.health.facilities", "Damaged.food.distribution.site"
                     
                     )
# read_data ---------------------------------------------------------------

incident_report_raw <- read.csv(incident_report_file_path,na.strings = c("NA",""," ")) %>% 
  dplyr::select(cols_needed_for_indicent) %>% mutate()

precipitation_jan_to_may <- list.files(path = precipitation_folder_path,pattern = "*reporting",full.names = T)
precipitation_jun_to_aug <- list.files(path = precipitation_folder_path,pattern = "*GSB",full.names = T)

# incident_report ---------------------------------------------------------

incident_pivot_wider <- incident_report_raw %>% pivot_wider(names_from = Type.of.incident ,values_from = Number.of.incidents,
                                                             values_fn = sum) %>% dplyr::select(-"NA") %>% filter(!is.na(Date.of.assessment))

incident_report<- incident_pivot_wider %>% group_by(Date.of.assessment) %>%summarise_each(funs(sum(., na.rm = TRUE)))
incident_report$Date <- incident_report$Date.of.assessment %>% dmy() %>% as.Date()
incident_report <- incident_report %>% select(-Date.of.assessment)

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


# daily_average -----------------------------------------------------------

precipitation_data_jan_to_may_combind$Date <-  as.Date(precipitation_data_jan_to_may_combind$Time)

precipitation_data_jan_to_may_daily_mean <- precipitation_data_jan_to_may_combind %>% group_by(Date,Device.name) %>% 
  summarise(
    Value = mean(Value,na.rm = T)
  )


precipitation_data_jun_to_aug_combind$date_time <- dmy_hm(precipitation_data_jun_to_aug_combind$Date)
precipitation_data_jun_to_aug_combind$Date <- as.Date(precipitation_data_jun_to_aug_combind$date_time)

precipitation_data_jun_to_aug_daily_mean <- precipitation_data_jun_to_aug_combind %>% group_by(Date,Device.name) %>% 
  summarise(
    Value = mean(Value,na.rm = T)
  )


# combind_two_precipitation_dataset ------------------------------------------------------
precipitation_data_jan_to_may_full <- precipitation_data_jan_to_may_daily_mean %>% 
  pivot_wider(names_from = Device.name,values_from = Value)

precipitation_data_jun_to_aug_full <- precipitation_data_jun_to_aug_daily_mean %>% 
  pivot_wider(names_from = Device.name,values_from = Value)

precipitation_data_jun_to_aug_full[sapply(precipitation_data_jun_to_aug_full, is.nan)] <- NA

precipitation_data <- bind_rows(precipitation_data_jan_to_may_full,precipitation_data_jun_to_aug_full)



# BMD_precipitation_data --------------------------------------------------

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


# precipitation value with BMD --------------------------------------------

precipitation_full_data <- precipitation_data %>% full_join(precipitation_BMD_data)



# data_with_incident_report -----------------------------------------------

full_data <- precipitation_full_data %>% full_join( incident_report)
full_data <- full_data[full_data_order] %>% arrange(Date)

write.csv(full_data,"outputs/precipitation_incident.csv",na = "")
