# bgd_2020_hydro_incident_db

This repo contains the scripts that are used to merge the precipitation data with incident data and weather data. The input folder contains all different datasets that are collected from different sources. 

## inputs
Precipitation data is collected from 3 different sources.<br />
	- Bangladesh Meteological Department (inputs/BMD_data)<br />
	- UNDP rain guages data (inputs/precipitation_data/[YEAR])<br />
	- Satellite base precipitation data (inputs/cfrs_chirps_precipitation_datasets)<br />

Two weather datasets are collected from UNDP.<br />
	- MSF 8W (inputs/weather_dataset/2009_MSF8W_June_to_September)<br />
	- SMEP Hub (inputs/weather_dataset/2009_SMEPHub_June_to_September)<br />

Incident data are collected from IOM <br />
	- Incident report (inputs/incident_report/SMSD Incident database 2020 Jan - Nov)<br />

Note: No inputs are uploaded in here to ensure the data confidentiality. Contact with REACH Initiative, Bangladesh team if interested.

## outputs
The outputs folder contains the output that results from the script.

## scripts
The script folder contains all the scripts that are used to merge the dataset. However you should run the "merging_rain_weather_and_incident_december_2020.R" only. All the other scripts are sourced to this script. 