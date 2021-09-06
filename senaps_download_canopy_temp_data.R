#read in downloaded data file with  sensor data from senaps and tidy
#connect to metadata, this version uses metadata for the Ferrero Rocher project.
#simple visual plots to carry out sensor activity check
#combine the weather data into the canopy temperature data frame.

library(tidyverse)
library(readxl)
library(lubridate)
library(forcats)
library(padr)
library(shiny)
library(directlabels)

timezone <-  "Australia/Sydney"
date_first <- ymd("2020/11/15", tz = timezone)
date_last <-  ymd("2021/04/07", tz = timezone)

#Read in metadata file. 
F_metadata <- read_csv("data/Hazelnut_metadata_compiled.csv") %>%
  mutate(location_ID = as.factor(location_ID),
         sensor_no = as.factor(sensor_no),
         stream = as.factor(stream)) %>%
  mutate(site_id = as.factor(site_id)) %>%
  mutate(tree_age_years = as.factor(tree_age_years)) %>% 
  mutate(start_date = dmy(start_date)) %>% 
  mutate(end_date = dmy(end_date)) %>% 
  arrange(desc(stream))

# read in and tidy canopy temperature data

F_ct_DATA <- read.csv("data/AgriAust_CanopyTemp_raw.csv", skip = 48)

F_CanopyTemps <- pivot_longer(F_ct_DATA, c("goannaag.12001.CSIRO_AgriAust_Block_414_S1.temp":"goannaag.17659.CSIRO_AgriAust_Block_173_S2.temp"), names_to = "stream_type", values_to = "canopy_temp") %>% 
  mutate(stream = str_remove(stream_type, ".temp")) %>%
  mutate(stream = as.factor(stream)) %>% 
  arrange(desc(stream)) %>% 
  mutate(date_time = as_datetime(timestamp, tz = "Australia/Sydney")) %>% 
  select(date_time,timestamp,stream,stream_type,canopy_temp)


#join the metadata and the canopy temps. 
sensors <- full_join(F_CanopyTemps, F_metadata) %>% 
  select(location_ID,
         site_id,
         name,
         stream,
         sensor_no,
         date_time,
         canopy_temp,
         location,
         experiment,
         tree_age_years,
         AGRI_Map_Unit,
         start_date,
         end_date,
         ct_data_quality,
         ASC_Soil_Classification,
         Soil_Hydraulic_Characteristics)

#Data Hygiene test - checks that no blank rows have been inserted.look in console, should be a tibble with no null rows. 
sensors %>% filter(is.na(site_id))

##filter out sensors or dates we do not want to include in the "active" data set.
#filter applied to filter out "bad" or "poor" and "patchy" quality data. Poor and patchy data can potentially be patched. 
active <- sensors %>% 
  drop_na(canopy_temp) %>% 
  filter(date_time >= date_first & date_time <= date_last) %>% 
  filter(ct_data_quality != "bad") %>% 
  filter(ct_data_quality != "poor") 

#Data Hygiene test - checks that no blank rows have been inserted.
active %>% filter(is.na(site_id))


############################################################################
##This forms the main data frame 
#apply pad and filter dates for canopy temp visualisation.
active_f <- active%>%
  mutate(tree_age_years = as_factor(tree_age_years)) %>%
  mutate(site_id = as_factor(site_id),
         sensor_no = as_factor(sensor_no),
         AGRI_Map_Unit = as.factor(AGRI_Map_Unit)) %>%
  filter(between(canopy_temp, 5, 50)) %>% 
  pad(by = "date_time") %>% 
  select(location_ID,site_id,sensor_no,stream,canopy_temp,tree_age_years, AGRI_Map_Unit,everything())
#Data Hygiene test - checks that no blank rows have been inserted.
active_f %>% filter(is.na(site_id)) 

##calculate count per day per sensor
sensor_activity <- active %>%
  mutate(day = date(date_time)) %>%
  group_by(day,stream,location_ID, site_id) %>%
  arrange(desc(stream)) %>% 
  summarise(sensor_activity=n())

# visualise count per day for each sensor
sensor_activity %>%  
  ggplot+
  aes(x = day, y = sensor_activity)+
  geom_point(size = 1, colour = "red")+
  geom_hline(yintercept = 96, colour = "green")+
  geom_hline(yintercept = 48, colour = "green")+
  labs(x = "Date", y = "observation count", title = "Sensor observations per day")+
  facet_wrap(~location_ID)+
  scale_x_date(date_labels = "%d -%b", date_breaks = "15 days")+
  ylim (0,110)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


###############################################################################  
#visualise plot canopy temperature reads

active_f %>% 
  ggplot()+
  aes(x = date_time, y = canopy_temp, colour = AGRI_Map_Unit, group = site_id)+
  geom_line()+ #(alpha = 0.3, size = 0.5)+
  geom_point (data = filter(active_f, !between(canopy_temp, 5, 50)),
              aes(x=date_time, y=canopy_temp), colour = "red", size = 1)+
  labs(y = "canopy temperature C", title = "Daily Canopy temperature")+
  facet_wrap(site_id + .~location_ID)+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "20 day")+
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#histogram of cT by soil type 
active_f %>%
 filter(date_time >= "2020-11-15 10:00:00" & date_time <= "2021-04-06 14:00:00") %>%
 ggplot() +
 aes(x = canopy_temp, colour = AGRI_Map_Unit) +
 geom_histogram(bins = 30L)+
 theme(legend.title = element_blank())+
 scale_color_manual(values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33"))+
 labs(title = "Histogram temperature counts", y = "count of temperature", x = "canopy temperature C")+
 theme_minimal()+
 facet_grid(vars(tree_age_years, labeller = "year trees"), vars(AGRI_Map_Unit))

##Calculate Stress Hours
#####################################################################################################

#calculate stress hours - function. 
stress_times_ausbiotic <- function(timestamp,
                                   canopy_temp,
                                   start_of_day = 7,
                                   end_of_day = 19,
                                   threshold_canopy_temp = 25,
                                   threshold_solar_radiation = 150) {
  
  
# Calulate difference between timestamp and previous timestamp
  hour      <- hour(timestamp)
  diff_hour <- difftime(timestamp[-1], timestamp[-length(timestamp)], units = 'hour')
  diff_hour <- c(diff_hour[1], diff_hour)
  #---
  
  ST <- case_when((canopy_temp > threshold_canopy_temp) & (hour >= start_of_day) & (hour < end_of_day) ~ diff_hour,
                  TRUE ~ 0)
  ST
}

sensorsdata_stress <- active_f %>%
  distinct() %>% 
  group_by(location_ID) %>% 
  mutate(ST = stress_times_ausbiotic(timestamp = date_time,
                                     canopy_temp = canopy_temp,
                                     start_of_day = 7,
                                     end_of_day = 19,
                                     threshold_canopy_temp = 25,
                                     threshold_solar_radiation = 150)) %>% 
  mutate(ST = ifelse(ST<0, 0, ST)) %>% 
  mutate(DH = cumsum(as.numeric(ST))) %>%
  ungroup()

#create file stress hours all sensors. 
write_csv(sensorsdata_stress, "data/Canopy_Temperature_sensor_stress_hours.csv")
write_rds(sensorsdata_stress, "data/Canopy_Temperature_sensor_stress_hours.rds")

#metadata of soil and tree age per site
site_metadata <- sensorsdata_stress %>% 
  distinct(site_id,tree_age_years,AGRI_Map_Unit,ASC_Soil_Classification,Soil_Hydraulic_Characteristics)
write_csv(site_metadata, "data/site_id_metadata.csv")
write_rds(site_metadata, "data/site_id_metadata.rds")

#visualise stress accumulation per season, by location_ID, for each site.

sensorsdata_stress %>% 
  ggplot()+
  aes(x = date_time)+
  geom_line(aes(y = DH, colour = location_ID))+
  labs(title = "cumulative stress hours for each site", y = "cumulative stress hours" )+
  xlab("")+
  facet_wrap(~site_id)+
  theme_minimal()+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  guides(colour = guide_legend("sensor no"))


##graphed with separate location and site id labelled. 
sensorsdata_stress %>% 
  ggplot()+
  aes(x = date_time)+
  geom_line(aes(y = DH, colour = location_ID))+
  labs(title = "cumulative stress hours for each site", y = "cumulative stress hours" )+
  xlab("")+
  facet_wrap(location_ID + .~site_id)+
  theme_minimal()+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(legend.position = "none")

#####Visualise  Cumulative stress for 5 year old trees, by location id. 

#labelled location_ID faceted by soil type
sensorsdata_stress %>%
  filter(tree_age_years %in% "5") %>% 
  ggplot() +
  aes(x = date_time, y = DH, colour = AGRI_Map_Unit, group = location_ID) +
  geom_line(size = 1L) +
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("blue", "#FFBD33","#FF5733","#75FF33")) +
  labs(y = "cumulative stress hours", title = "Cumulative Stress Hours of 5 year old trees") +
  xlab("")+
  facet_wrap(~AGRI_Map_Unit)+
  theme_minimal()+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  expand_limits(x = ymd_hm("2021-11-15 0:00", "2021-04-20 0:00"))+
  geom_dl(aes(label=location_ID), method="last.points") 

## cummultative stress hours per location id by soil type
sensorsdata_stress %>%
  filter(tree_age_years %in% "5") %>% #filter(AGRI_Map_Unit == 'Sands') %>% 
  ggplot() +
  aes(x = date_time, y = DH, colour = AGRI_Map_Unit, group = location_ID) +
  geom_line(size = 1L) +
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  labs(y = "cumulative stress hours", title = "Cumulative Stress Hours of 5 year old trees") +
  xlab("")+
  theme_minimal()+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  expand_limits(x = ymd_hm("2020-11-15 0:00", "2021-04-20 0:00"))

#calculate average cummulative stress hours for season, by site. 
average_cum_stress_site <- sensorsdata_stress %>% mutate(day = date(date_time)) %>% 
  group_by(site_id, date_time) %>% 
  summarise(ST_site = mean(as.numeric(ST), na.rm = TRUE)) %>% 
  mutate(DH_site = cumsum(ST_site))

average_cum_stress_soil <- sensorsdata_stress %>% mutate(day = date(date_time)) %>% 
  group_by(AGRI_Map_Unit,date_time) %>% 
  summarise(ST_soiltype = mean(as.numeric(ST), na.rm = TRUE)) %>% 
  mutate(DH_soiltype = cumsum(ST_soiltype))

#join average cumulative stress hours to site metadata. 
stress_summary_site <- full_join(average_cum_stress_site,site_metadata, by = "site_id")
write_csv(stress_summary_site, "data/average_stress_site.csv")
write_rds(stress_summary_site, "data/average_stress_site.rds")

#cummulative stress hours for each site - average of stress hours per site. 
stress_summary_site %>% 
  ggplot()+
  aes(x = date_time)+
  geom_line(aes(y = DH_site, group = site_id, colour = AGRI_Map_Unit ))+
  labs(title = "average cumulative stress hours by site", y = "cumulative stress hours")+
  xlab("")+
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  expand_limits(x = ymd_hm("2020-11-15 0:00", "2021-04-20 0:00"))

#stress by soil type
#cummulative stress hours for each site - average of stress hours per soiltype. 
average_cum_stress_soil %>% 
  ggplot()+
  aes(x = date_time)+
  geom_line(aes(y = DH_soiltype, group = AGRI_Map_Unit, colour = AGRI_Map_Unit ))+
  labs(title = "soil type", subtitle = "average cumulative stress hours", y = "cumulative stress hours")+
  xlab("")+
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("blue", "#FFBD33","#FF5733","#75FF33")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  expand_limits(x = ymd_hm("2020-11-15 0:00", "2021-04-20 0:00"))

###funky sandy clay loams soil numbers....
sensorsdata_stress %>%
  filter(location_ID %in% c("17068", "17018", "17016")) %>%
  
  filter(date_time >= "2020-11-15 10:00:00" & date_time <= "2021-04-06 14:00:00") %>%
  ggplot() +
  aes(x = date_time, y = DH, colour = location_ID, group = location_ID) +
  geom_line(size = 1L) +
  scale_color_hue()+
  labs(title = "funky sandy clay loams individual sensors", y = "cumulative SH")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  theme_minimal()

# ##Average by soil Cummulative stress for 5 year old trees .
stress_summary_site %>% filter(tree_age_years %in% "5") %>%
  ggplot()+
  aes(x = date_time)+
  geom_line(aes(y = DH_site, group = site_id, colour = AGRI_Map_Unit ))+
  labs(title = "5 year old trees", subtitle = "average cumulative stress hours per site", y = "cumulative stress hours")+
  xlab("")+
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "1 month")+
  expand_limits(x = ymd_hm("2020-11-15 0:00", "2021-04-20 0:00"))


#Daily accumulated Stress Hours##############
ST_acc_daily <- sensorsdata_stress %>% mutate(day = date(date_time)) %>% 
  group_by(location_ID, day) %>% 
  arrange(day) %>% 
  group_by(location_ID,day) %>% 
  summarise(daySH = sum(ST))

#Join metadata to daily stress and output csv
#select columns for data frame
location_list <- sensorsdata_stress %>% 
  select("location_ID","site_id","sensor_no", "stream","tree_age_years", "AGRI_Map_Unit","ct_data_quality","ASC_Soil_Classification","Soil_Hydraulic_Characteristics")
#create unique list by location_ID for metadata
location_metadata <- unique(location_list)
#join dataframes  
daily_ST <- full_join(location_metadata, ST_acc_daily) %>% 
  select("location_ID","site_id","tree_age_years", "AGRI_Map_Unit","day","daySH", "ASC_Soil_Classification","Soil_Hydraulic_Characteristics","stream" ,"ct_data_quality","sensor_no")
#output daily stress hours csv
write_csv(daily_ST, "data/daily_stress_location.csv")
write_rds(daily_ST, "data/daily_stress_location.rds")

##daily stress hours graph - bar version

ggplot(daily_ST) +
  aes(x = day, y = daySH, fill = AGRI_Map_Unit, group = site_id) +
  geom_col()+
  scale_fill_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  labs(y = "daily accumulated stress hours", title = "Daily accumulated stress", subtitle = "site and sensor") +
  theme_minimal() +
  facet_wrap(site_id + .~location_ID)+
  theme(legend.position = "bottom")+
  theme(strip.text = element_text(size=8))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(date_labels = "%d -%b", date_breaks = "1 month")+
  expand_limits(x = ymd("2020-11-15", "2021-04-20"))

#as a line graph
ggplot(daily_ST) +
  aes(x = day, y = daySH, colour = AGRI_Map_Unit) +
  geom_line(size = 0.8) +
  scale_color_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  labs(y = "daily accumulated stress hours", title = "Daily accumulated stress", subtitle = "sensor") +
  xlab("")+
  theme_minimal() +
  facet_wrap(site_id + .~location_ID)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(date_labels = "%d -%b", date_breaks = "1 month")+
  expand_limits(x = ymd("2020-11-15", "2021-04-20"))+
  labs(title = "All sensors", subtitle = "daily stress time site", y = "daily stress hours")
 

#histogram stress hours per day
ggplot(daily_ST) +
  aes(x = daySH, colour = AGRI_Map_Unit) +
  geom_histogram(bins = 30L,binwidth=1/3) +
  theme_minimal() +
  facet_wrap(vars(AGRI_Map_Unit), scales = "free")+
  scale_fill_manual(values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) +
  scale_color_manual(values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33")) 
  #scale_fill_manual(name = "soil type",breaks = c("Sands","Sandy Clay Loams", "Clays", "High Density"),labels = c("Sands","Sandy Clay Loams", "Clays", "High Density planting"),values = c("#FAFF33", "#FFBD33","#FF5733","#75FF33"))

#view single sensor
singleplot <- active_f %>% filter(sensor_no == 1754) 
ggplot(singleplot,aes(x = date_time, y = canopy_temp))+
  geom_point(alpha = 1, size = 0.8)+
  geom_point (data = filter(singleplot, !between(canopy_temp, 5, 50)),
              aes(x=date_time, y=canopy_temp), colour = "red", size = 1)+
  scale_x_datetime(date_labels = "%d -%b", date_breaks = "10 day")+
  labs(x = "Date time", y = "canopy temperature C", title = (singleplot$sensor_no), subtitle = (singleplot$experiment))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#read in and tidy weather data sets.  
F_weather_A_DATA <- read.csv("data/Weather_AgriAust_Arrambee_20201001-20210526.csv", skip = 5) %>% 
  rename(date_time_char = Date...Time,
         air_temp_C = Temp....C,
         rainfall_mm = Rain...mm,
         low_temp_C = Low.Temp....C,
         high_temp_C = High.Temp....C) %>% 
  select(date_time_char,air_temp_C,rainfall_mm,low_temp_C,high_temp_C)

#read in and tidy weather data sets.  
F_weather_D_DATA <- read.csv("data/Weather_AgriAust_Dellapool_20201001-20210526.csv", skip = 5) %>% 
  rename(date_time_char = Date...Time,
         air_temp_C = Temp....C,
         rainfall_mm = Rain...mm,
         low_temp_C = Low.Temp....C,
         high_temp_C = High.Temp....C) %>% 
  select(date_time_char,air_temp_C,rainfall_mm,low_temp_C,high_temp_C)

#Apply date and number transformations to weather data.
F_weather_Arrambee <- F_weather_A_DATA %>%
  mutate(air_temp_C = na_if(air_temp_C,"--"),
         low_temp_C = na_if(low_temp_C, "--"),
         high_temp_C = na_if(high_temp_C, "--")) %>% 
  mutate(date_time = mdy_hm(date_time_char, tz = "Australia/Sydney")) %>% 
  mutate(air_temp_C = as.numeric(air_temp_C),
         low_temp_C = as.numeric(low_temp_C),
         high_temp_C = as.numeric(high_temp_C)) %>% 
filter(date_time >= "2020-11-15 21:00:00" & date_time <= "2021-04-06 14:00:00")


F_weather_Dellapool <- F_weather_D_DATA %>% 
  mutate(air_temp_C = na_if(air_temp_C,"--"),
         low_temp_C = na_if(low_temp_C, "--"),
         high_temp_C = na_if(high_temp_C, "--")) %>% 
  mutate(date_time = mdy_hm(date_time_char, tz = "Australia/Sydney")) %>% 
  mutate(air_temp_C = as.numeric(air_temp_C),
         low_temp_C = as.numeric(low_temp_C),
         high_temp_C = as.numeric(high_temp_C))

#visualise weather data to check the tz conversion
ggplot(F_weather_Arrambee,aes(x = hour(date_time), y = air_temp_C))+
  geom_smooth()
#visualise weather data
ggplot(F_weather_Arrambee, aes(x = date_time, y = air_temp_C)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  scale_x_datetime(date_labels = "%d-%b", date_breaks = "15 days")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


weather_data_join <- full_join(sensorsdata_stress, F_weather_Arrambee, by.x = "date_time", by.y = "date_time") %>% 
  ungroup()


write_csv(weather_data_join,"data/Canopy_Weather.csv")

#check data quality
weather_data_join %>%filter(is.na(site_id))
ggplot(weather_data_join,aes(x = hour(date_time), y = air_temp_C))+
  geom_smooth()

#Weather data and air temp. too busy, too big. better to subset and run for loop
weather_data_join %>%
  ggplot(aes(x = date_time))+
  geom_line(aes(y = canopy_temp)) +
  geom_point(aes(x = date_time,y = air_temp_C), shape = 1, size = 0.5,colour = "blue", alpha = 0.5)+
  facet_wrap(~location_ID)+
  scale_x_datetime(date_labels = "%d-%b", date_breaks = "20 days")+
  labs(x = "date", y = "Temperature C", title = "Canopy and Air Temperature") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


  
  ####################################extract single file############

#view each sensor in a "for" loop for a closer look at each individual sensor.

sensors <- unique(active_f$location_ID)

gplot_sensor <- sensors[i]

#
#loops all sensors to create a separate plot for all sensors in loop
for (i in 1:length(sensors)){

  gplot_sensor <- sensors[i]

  g1 <- ggplot(filter(active, location_ID == gplot_sensor), aes(x = date_time, y = canopy_temp))+
    geom_line()+
    labs(
      x = "Time",              # x axis title
      y = "CT",   # y axis title
      title = gplot_sensor)      # main title of figure
  # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

  print(g1)

}


SensorExtract <- weather_data_join

Sensors <- SensorExtract %>% distinct(location_ID) 

#####################needs fixing
# use a for loop to go filter the data by each sensor ID 
for (i in 1:length(Sensors)) {
  
  SensorID <- Sensors$location_ID[i] %>% 
    
    BySensor <- SensorExtract %>% 
      
      SaveFileName <- paste0("data/file","/s",location_ID,".csv",sep = "") %>% 
        
        try(write_csv(SaveFileName))
}







