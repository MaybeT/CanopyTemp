#Quality Assurance checks for the sensors from the Goanna site. 
#the reference for the sensor activity is the 2021_sensor_allocation active sensors excel file. 
#What sensors are working, what is the quality of the data
#Tracey May 5/11/2020

library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)


#set timezone

timezone <-  "Australia/Sydney"
date_first <- "2020-12-30" 
date_last <-  "2020-12-15"

#set date to filter by
filter_date <- "2020-10-20"

#set file get and save paths
save_path <- "//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys_Current/Canopy Temperatures"
get_path <- "//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys_Current/Canopy Temperatures"

#read in 2021 sensor allocations excel file and the Goanna ag download
sensor_allocation <- read_excel("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys_Current/2020-2021/2021_canopy_sensor_allocation.xlsx", "2021_sensor_allocations")
colnames(sensor_allocation)
#read in Goanna_ag data and prepare for joining
Goanna_ag <- readRDS(file.path(get_path, "whole-season-canopy-temps.RDS")) %>% 
  select("location_ID","date_time","temp", "air_temp", "ambient_temp") %>% 
  mutate(location_ID = as.numeric(location_ID))
colnames(Goanna_ag) 
#check the locations for goanna_ag downloads
#Goanna_locations_id <- select(Goanna_ag,c(location,location_ID)) %>% 
#  unique()

# prepare the files for joining. 



#Join the sensor allocations spreadsheet information to the Goanna ag download, filter by active sensors

sensors <-full_join(Goanna_ag_DL, sensor_allocation, by = c("location_ID" = "location_ID")) %>% 
  select(location_ID,
         name,
         location,
         sensor_no,
         date_time,
         temp,
         air_temp,
         ambient_temp,
         experiment,
         plot,
         treatment,
         active)

#filter by active sensors  
active <-  filter(sensors, active == "request") %>% 
  drop_na(name) %>% 
  drop_na(location_ID)#this should not be necessary once the names are fixed. 

check_list  <- select(active,c(location_ID, name, location)) %>% 
  unique()

#calculate number of reads per day per sensor
sensor_activity <- active %>% 
  mutate(day =  format.Date(date_time, "%Y-%m-%d")) %>%
  mutate(day = as.Date(day)) %>% 
  group_by(sensor_serial,day) %>% 
  summarise(Go_activity=n())

# visualise count per day by sensor number 
ggplot(sensor_activity, aes(x = day, y = Go_activity))+
  geom_point()+
  facet_wrap(~sensor_serial)+
  ylab("observations")+
  xlab("Date")+       
  scale_x_date(date_breaks = "10 day")+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  
  
#visualise plot canopy temperature reads per day per sensor

ggplot(sensors, aes(x=date_time, y=canopy_temp, colour = UID))+
  geom_point(alpha=0.3, size= 0.5, colour = "black")+
  geom_point(data = filter(CT_sensors_cleaned, !between(canopy_temp, 5, 50)),
             aes(x=date_time, y=canopy_temp), colour = "red")+
  facet_wrap(~UID, scales = "free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(active,aes(x = sensor_serial, y = CanopyTemp, colour = Location))+
  geom_point()+
  facet_wrap(~Location, scales = "free")

#last date sensors were read by sensor  
summary_tab <- sensors %>%
  group_by(ID)
summarise(max_date = max(Last), reads = n()) %>% 
  mutate(max_date = as.Date(max_date))

#Summarise with UID from SD card file to see the date range present for the SD card

start_end_date <- group_by(sensor_SD_file, UID) %>% 
  summarise(min_date = min(Timestamp, na.rm = TRUE),
            max_date = max(Timestamp, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE),
            count = n())        

#view each sensor in a "for" loop for a closer look at each individual sensor. 

sensors <- unique(SensorExtract$UID)

plotUID <- sensors[i]

i = 2

#loops all sensors to create a plot for all sensors in loop
for (i in 1:length(sensors)){
  
  plotUID <- sensors[i]
  
  g1 <- ggplot(filter(SensorExtract, UID == plotUID), aes(x = Timestamp, y = Value))+
    geom_line()+
    labs(
      x = "Time",              # x axis title
      y = "CT",   # y axis title
      title = plotUID)      # main title of figure
  # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  print(g1)
  
}
ggplot(filter(SensorExtract, UID == plotUID), aes(x = Timestamp, y = Value))+
  geom_line()+
  labs(
    x = "Time",              # x axis title
    y = "CT",   # y axis title
    title = plotUID)      # main title of figure
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


