library(tidyverse)
library(readxl)
library(lubridate)

#mySummary <- read_excel("//mvpc60-mv/C$/Users/wea070/Documents/dat.xlsx") %>%  
mySummary <- read_excel("//nexus.csiro.au/csiro/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys/AusBIOTIC database/Definative_cT_Raw/2015/ACRI/ACRI_2015_16_HourlyWeather.xlsx") %>%
  rename(Rain_mm = "Rain (mm)") %>% 
  rename(Max_Temp = "Max Air Temp (°C)") %>% 
  rename(Min_Temp = "Min Air Temp (°C)")

#Add a date column
  
 %>% 
  group_by(Day) %>% 
  summarise(min = min(Temp))
Summary_max <- mySummary %>% 
  mutate(Day = format.Date(Local_ts, "%Y-%m-%d")) %>% 
  group_by(Day) %>% 
  summarise(max = max(Temp))
#Join 
Summary_join <- full_join(Summary_min, Summary_max)
#sort by day and make convert date field to "date" 
Sum_daily_temp <-  Summary_join %>% mutate(Day = as.Date(Day)) %>% 
arrange(Summary_join, by = Day)

#put the summary file into tidy format to be able to visualise data

view_daily <- pivot_longer(Sum_daily_temp,c(min,max), names_to = "var_name", values_to = "temperature")

#This is a "geom_point - by point plot"
ggplot(view_daily, aes(x = Day, y = temperature, colour = var_name))+
        geom_point()+
        ylab("temperature C")+
        xlab("Date")+       
        scale_x_date(date_breaks = "5 day")+
        theme(legend.title = element_blank())+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 
#same plot as a "geom_line" - line plot       
ggplot(view_daily, aes(x = Day, y = temperature, colour = var_name))+
  geom_line()+
  ylab("temperature C")+
  xlab("Date")+       
  scale_x_date(date_breaks = "5 day")+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



write_csv(Summary_max, "//mvpc60-mv/C$/Users/wea070/Documents/Max.csv" )

write_csv(Sum_daily_temp, "//mvpc60-mv/C$/Users/wea070/Documents/MinMax.csv" )

write_csv(Summary_min, "//mvpc60-mv/C$/Users/wea070/Documents/Min.csv" )       