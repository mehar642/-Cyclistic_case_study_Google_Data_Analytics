library(tidyverse)
library(lubridate)
library(ggplot2)

getwd()

setwd("C:/Users/user/Desktop/Dtaset")

jan_2023 <-read.csv("202301-divvy-tripdata.csv")
feb_2023 <-read.csv("202302-divvy-tripdata.csv")
mar_2023 <-read.csv("202303-divvy-tripdata.csv")
apl_2023 <-read.csv("202304-divvy-tripdata.csv")
may_2023 <-read.csv("202305-divvy-tripdata.csv")
jun_2023 <-read.csv("202306-divvy-tripdata.csv")
jul_2023 <-read.csv("202307-divvy-tripdata.csv")
aug_2023 <-read.csv("202308-divvy-tripdata.csv")
sep_2023 <-read.csv("202309-divvy-tripdata.csv")
oct_2023 <-read.csv("202310-divvy-tripdata.csv")
nov_2023 <-read.csv("202311-divvy-tripdata.csv")

all_trips <-bind_rows(jan_2023,feb_2023,mar_2023,apl_2023,may_2023,jun_2023,jul_2023,aug_2023,sep_2023,oct_2023,nov_2023)

all_trips <-  all_trips %>% 
   mutate(member_casual = recode(member_casual
                   ,"Subscriber" = "member"
                    ,"Customer" = "casual"))

table(all_trips$member_casual)

all_trips$date <-as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)



str(all_trips)

is.factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(cnt = n()) %>%
  mutate(percentage = cnt/sum(cnt)) %>% 
  ggplot(aes(x = "", y = cnt, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start=0) +
  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribution of Annual and Casual Riders(in percentage)')+
  theme_void()

all_trips_v2 %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(cnt2 = n()) %>% 
  ggplot(aes(x = rideable_type, y = cnt2, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7) +
  labs(title = "Total Number of Rides by Bike Types and Member Types", x = 'Type of Bikes', y = 'Number of Rides')

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7) +
  labs(title = "Total Number Of Rides by Member Type and Week of Day", x = "Weekday", y = "Number of rides")


all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7)+
  labs(title ='Average Duration Of Rides During The Week By Member Type', x = 'Weekday', y = 'Average Duration')