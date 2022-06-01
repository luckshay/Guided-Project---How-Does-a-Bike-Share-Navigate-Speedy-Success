library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

setwd("E:/Capstone Project/Case Study 1 (Guided)") #sets your working directory to simplify calls to data ...
getwd()

#import data

may <- read.csv("may_21.csv")
jun <- read.csv("jun_21.csv")
jul <- read.csv("jul_21.csv")
aug <- read.csv("aug_21.csv")
sep <- read.csv("sep_21.csv")
oct <- read.csv("oct_21.csv")
nov <- read.csv("nov_21.csv")
dec <- read.csv("dec_21.csv")
jan <- read.csv("jan_22.csv")
feb <- read.csv("feb_22.csv")
mar <- read.csv("mar_22.csv")
apr <- read.csv("apr_22.csv")

#colnames(jan)


#Initial Processing and Preparation before merging datasets


(may <- rename(may,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(jun <- rename(jun,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(jul <- rename(jul,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(aug <- rename(aug,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(sep <- rename(sep,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(oct <- rename(oct,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(nov <- rename(nov,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(dec <- rename(dec,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

jan <- rename(jan,
              trip_id = ride_id
              ,bike_id = rideable_type
              ,start_time = started_at
              ,end_time = ended_at
              ,user_type = member_casual)

feb <- rename(feb,
              trip_id = ride_id
              ,bike_id = rideable_type
              ,start_time = started_at
              ,end_time = ended_at
              ,user_type = member_casual)

(mar <- rename(mar,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))

(apr <- rename(apr,
               trip_id = ride_id
               ,bike_id = rideable_type
               ,start_time = started_at
               ,end_time = ended_at
               ,user_type = member_casual))


#store trip_id and bike_id as character data type

may <-  mutate(may, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

jun <-  mutate(jun, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

jul <-  mutate(jul, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

aug <-  mutate(aug, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

sep <-  mutate(sep, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

oct <-  mutate(oct, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id))

nov <-  mutate(nov, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id))

dec <-  mutate(dec, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id))

jan <-  mutate(jan, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

feb <-  mutate(feb, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

mar <-  mutate(mar, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

apr <-  mutate(apr, trip_id = as.character(trip_id)
               ,bike_id = as.character(bike_id)) 

#merge all tables into a singular dataset

trips_df <- bind_rows(jan,feb,mar,apr,jun,may,jul,aug,sep,oct)
trips_df <- trips_df %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))


no_of_users <- trips_df %>% 
  count(user_type)      #counting number of distinct user types and checking if there are any null or irregular values
View(no_of_users)

#merge all tables into a singular dataset

trips_df <- bind_rows(jan,feb,mar,apr,jun,may,jul,aug,sep,oct)
trips_df <- trips_df %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))


no_of_users <- trips_df %>% 
  count(user_type)      #counting number of distinct user types and checking if there are any null or irregular values
View(no_of_users)

#View(trips_df)
#str(trips_df)
#summary(trips_df)

#make a backup or v2 data frame

v2_trips_df<- as.data.frame(trips_df)
View(v2_trips_df)
str(v2_trips_df)
summary(v2_trips_df)

str(trips_df)
summary(trips_df)
