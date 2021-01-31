
weather_dataset <-smep_hub  %>% left_join(msf_8w) %>% mutate(
  time_date_2 = as.character.Date(time_date),
  #date2 =as_datetime(time_date),
  date =as.Date(time_date_2)
)

weather_dataset<- weather_dataset %>% select (-contains("Dir")) %>% 
  select(-time_date,-time_date_2) %>% 
  select(date,everything())


factor_cols <-c("MSF_8_W.Temp_Out", "MSF_8_W.Hi_Solar_Rad")

weather_dataset <- weather_dataset %>% mutate_at(factor_cols, function(x) {x<- as.numeric(as.character(x))})

cols_high <- weather_dataset %>% select(contains("Hi")) %>% names() %>% dput %>% as.character()
cols_low <- weather_dataset %>% select(contains("Low")) %>% names() %>% dput %>% as.character()
cols_mean <- weather_dataset %>% select(-cols_high,-cols_low,-date,-contains("Dir"),-contains("Rain")) %>% names %>% dput %>% as.character()
cols_sum <- weather_dataset %>% select(contains("Rain")) %>% names() %>% dput %>% as.character()
#weather_dataset[cols_mean] %>% View()


weather_dataset_daily_data_high_cols <- weather_dataset %>% group_by(date) %>% summarise_at(
  cols_high, function(x){x<- max(x,na.rm=T)})

weather_dataset_daily_data_low_cols<- weather_dataset %>% group_by(date) %>% summarise_at(
  cols_low, function(x){x<- min(x,na.rm=T)})

weather_dataset_daily_data_mean_cols <- weather_dataset %>% group_by(date) %>% summarise_at(
  cols_mean, function(x){x<- mean(x,na.rm=T)})

weather_dataset_daily_data_sum_cols <- weather_dataset %>% group_by(date) %>% summarise_at(
  cols_sum, function(x){x<- sum(x,na.rm=T)})  

weather_dataset_daily_data <- left_join(weather_dataset_daily_data_high_cols,
                                        weather_dataset_daily_data_low_cols
                                        )


weather_dataset_daily_data <- left_join(weather_dataset_daily_data,
                                        weather_dataset_daily_data_sum_cols
)

weather_dataset_daily_data<- left_join(weather_dataset_daily_data,
                                       weather_dataset_daily_data_mean_cols) %>% 
  select(date,starts_with("MSF"),starts_with("SMEP"),everything()) %>% select(-SMEP_Hub.Bar)


weather_dataset_daily_data <- weather_dataset_daily_data %>%
  mutate_at(names(weather_dataset_daily_data %>% select(-date)),~round(.,2))
