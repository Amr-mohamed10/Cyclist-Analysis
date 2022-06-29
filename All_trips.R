library(tidyverse)
library(lubridate) 
library(ggplot2) 
library(readr)

#know Your current working director 
getwd()
#Set a new director 
setwd("C:/Users/Amr 10/Downloads/Trip analysis 2019-2020")

#Fix date format

Sys.setlocale("LC_TIME","C")

#import CSV Files
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#Check colnames
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

# Rename columns  to make them consistent with q1_2020 
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character 
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames 
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#Remove lat, long, birthyear, and gender fields
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

#Count 
nrow(all_trips)

dim(all_trips) 

#Statistical summary of data.
summary(all_trips)

#check member_casual char
table(all_trips$member_casual)

#Change 4 labels to 2 labels
all_trips <- all_trips %>%
  mutate(member_casual = case_when(
         member_casual %in% "Subscriber" ~ "member",
         member_casual %in% "Customer" ~ "casual"))

#Aggergate Date 
all_trips$date <- as.Date(all_trips$started_at)#Export year,Month,day 
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")


#Get trip duration in seconds
 all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#Drop dirty values and negative results
 all_tripsV2<-all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
 
#convert ride_length to numeric 
 all_tripsV2$ride_length <- as.numeric(as.character(all_tripsV2$ride_length))
 
#Summary descriptive Analytics
summary(all_tripsV2$ride_length)

#Aggregate ride_length based on member_casual
member_casual_mean <- aggregate(all_tripsV2$ride_length ~ all_tripsV2$member_casual, FUN = mean)
member_casual_median <-aggregate(all_tripsV2$ride_length ~ all_tripsV2$member_casual, FUN = median)
member_casual_max <-aggregate(all_tripsV2$ride_length ~ all_tripsV2$member_casual, FUN = max)
member_casual_min <-aggregate(all_tripsV2$ride_length ~ all_tripsV2$member_casual, FUN = min)

#Notice that the days of the week are out of order
all_tripsV2$day_of_week <- ordered(all_tripsV2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#See the average ride time by each day for members vs casual users
Counts <- aggregate(all_tripsV2$ride_length ~ all_tripsV2$member_casual + all_tripsV2$day_of_week, FUN = mean)
#Visualize average duration per weekdays
all_tripsV2 %>%  
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(weekday,average_duration,fill = member_casual)) + 
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title ="Average trip duration by customer type Vs. Day of the week")

#Visualize demand per 24 Hours
all_tripsV2 %>% 
  mutate(time = hour(as.POSIXct(all_tripsV2$started_at))) %>%
  group_by(member_casual, time) %>%
  arrange(member_casual,time)%>%
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = member_casual, group = member_casual)) +
  geom_line()+
  scale_x_continuous(limits = c(0, 23),breaks = seq(0,23,1))+
  scale_y_continuous(n.breaks =10 )+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(title = "Number of trips per Hour", x = "Hour",y = "Number of Rides")

#visualize demands per month
  all_tripsV2 %>% 
    group_by(member_casual,month) %>%
    arrange(member_casual,month) %>%  
    summarise(number_of_trips = n()) %>%
    ggplot(aes(x= month, y= number_of_trips,fill = member_casual,group = member_casual))+ 
    geom_col(position = "dodge")+
    scale_y_continuous(n.breaks =10 )+
    labs(title = "Number of trips per Month", x = "Month",y = "Number of Rides")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
#Visualize demands per years 
  all_tripsV2 %>% 
    group_by(member_casual,year) %>%
    arrange(member_casual,year) %>%  
    summarise(number_of_trips = n()) %>%
    ggplot(aes(x= year, y= number_of_trips,fill = member_casual,group = member_casual))+ 
    geom_col(position = "dodge")+
    scale_y_continuous(n.breaks =10 )+
    labs(title = "Number of trips per Year", x = "Year",y = "Number of Rides")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  write.csv(counts,file = 'C:/Users/Amr 10/Downloads/divvy-tripdata Excel/csv data/avg_ride_length.csv')