rm(list=ls())

library(sf)
library(dplyr)

# read_dfs ----------------------------------------------------------------

rename<- read.csv("inputs/renaming/incident_camp_name.csv")

camp_boundary <- st_read("E://03_Common_shape_files/01_camp_block_sub_block/200908_RRC_Outline_Camp_AL1/200908_RRC_Outline_Camp_AL1.shp")

incident_df <- read.csv("inputs/incident_report/SMSD Incident database 2020 Jan - Nov.csv",
                        na.strings = c(""," ","n/a","NA"),stringsAsFactors = F) %>% filter(Incident == "Yes")


# fix_camp name  ----------------------------------------------------------

incident_df <- incident_df %>% left_join(rename,by =c("Camp"="old_name"))

incident_df$new_name <- incident_df$new_name %>% as.character()

incident_df$camp_name_reported <- if_else(is.na(incident_df$new_name),incident_df$Camp,incident_df$new_name)

incident_df <- incident_df %>% select(-new_name,-Camp)


# remove records where GPS is NA ------------------------------------------

incident_df_with_no_coordinate <- incident_df %>% filter(is.na(GPS.Log))
incident_df_with_coordinate <- incident_df %>% filter(!is.na(GPS.Log)&!is.na(GPS.Lat))

#covert to SF object
incident_data_sf <- st_as_sf(incident_df_with_coordinate,coords = c("GPS.Log","GPS.Lat"),crs = 4326)

# identify the inside and outside points  
incident_data_location_with_boundary <- incident_data_sf %>% mutate(
  location_status = lengths(st_within(incident_data_sf, camp_boundary)),
  location_status = if_else(location_status == 0 ,"outside_camp_boundary","inside_camp_boundary")
  )


# check camp consistency
incident_sf_only_inside <- incident_data_location_with_boundary #%>% filter(location_status== "inside_camp_boundary")

incident_sf_only_inside_intersection <- incident_sf_only_inside %>% st_join(camp_boundary %>% select(Camp_Name))


incident_sf_only_inside_intersection <- incident_sf_only_inside_intersection %>% mutate(
  camp_consitency = camp_name_reported == Camp_Name
) %>% select(location_status,camp_name_reported,Camp_Name,camp_consitency,everything()) %>% rename("camp_name_gps"="Camp_Name")



# write CSV ---------------------------------------------------------------


write.csv(incident_sf_only_inside_intersection,"outputs/incident_report_gps_check/incident_with_gps_check.csv")




