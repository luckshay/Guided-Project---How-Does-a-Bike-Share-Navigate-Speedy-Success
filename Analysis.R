library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

setwd("E:/Capstone Project/Case Study 1 (Guided)") #sets your working directory to simplify calls to data ...
getwd()
load("E:/Capstone Project/Case Study 1 (Guided)/Data.RData")

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

trips_df <- bind_rows(may,jun,jul,aug,sep,oct,nov,dec,jan,feb,mar,apr)
trips_df <- trips_df %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))


no_of_users <- trips_df %>% 
  count(user_type)      #counting number of distinct user types and checking if there are any null or irregular values
View(no_of_users)

##View(trips_df)
##str(trips_df)
##summary(trips_df)

#make a backup or v2 data frame

v2_trips_df<- as.data.frame(trips_df)
View(v2_trips_df)
str(v2_trips_df)
summary(v2_trips_df)

str(trips_df)
summary(trips_df)

#remove data frames of singular months
rm("may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar","apr")


#PROCESSING OF DATA

trips_df$day_of_week <- format(as.Date(trips_df$start_time), "%A") #get the week of day of the ride
trips_df$ride_length<- difftime(trips_df$end_time, trips_df$start_time, units="mins" ) #get the ride length
view(trips_df[(trips_df$ride_length<0 ),]) #check for bad data i.e. if duration is negative
all_trips <- trips_df[!(trips_df$ride_length<0),] #remove bad data found in previous step

view(all_trips[(all_trips$ride_length<0 ),]) #check for bad data i.e. if duration is negative
rm("trips_df")

#Analysis of DATA

#descriptive analysis
desc_mean <- mean(all_trips$ride_length) #straight average (total ride length / rides)
desc_median <- median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
desc_max <- max(all_trips$ride_length) #longest ride
desc_min <- min(all_trips$ride_length) #shortest ride

summary(all_trips$ride_length)

# Compare members and casual users
agg_mean <- aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = mean)
agg_median <- aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = median)
agg_max <- aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = max)
agg_min <- aggregate(all_trips$ride_length ~ all_trips$user_type, FUN = min)


# average ride time by each day for members vs casual users
agg_mean_byday <- aggregate(all_trips$ride_length ~ all_trips$user_type + all_trips$day_of_week, FUN = mean)

# Notice that days of the week are out of order. Let's fix that.
agg_mean_byday <- aggregate(all_trips$ride_length ~ all_trips$user_type + ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), FUN = mean)

# analyze ridership data by user type and weekday
ridership_data_byWeekDay <- all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(user_type, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()      #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%       # calculates the average duration
  arrange(user_type, weekday)                            # sorts

# analyze ridership data by user type, bike type and month
ridership_data_byMonth <- all_trips %>% 
  mutate(mths=month(start_time, label=TRUE)) %>% 
  group_by(user_type,mths,bike_id) %>% 
  summarise(number_of_rides=n(),
            avg_duration=mean(ride_length)) %>% 
  arrange(user_type, mths,bike_id)

# total ride length
ride_length_count<- all_trips %>% 
  count(ride_length,sort = TRUE)

# VISUALIZATIONS

week_viz<-ggplot(data=ridership_data_byWeekDay)+geom_point(mapping = aes(x=weekday,y=number_of_rides, color=user_type,size=1))

week_viz+
  labs(title="Active weekdays of users", subtitle ="The graph explains which days of the week are busiest for users ", caption = "Data Based on 'Sophisticated, Clear, and Polished': Divvy and Data Visualization" )+
  annotate("text", x="Wed",y=530000, label="Casuals prefer weekends, and members' preference is consistent over the week", color="black", fontface="italic", size=3.5)


bike_pref_viz<-ggplot(ridership_data_byMonth)+geom_col(mapping = aes(x=bike_id, y=number_of_rides,fill=user_type))+facet_wrap(~user_type)

bike_pref_viz+
  labs(title="What type of rental bikes do users prefer?", caption = "Data Based on 'Sophisticated, Clear, and Polished': Divvy and Data Visualization" )+
  annotate("text", x="docked_bike",y=1955000, label="              Both types of users prefer
                     classic bike the most", color="black", fontface="italic", size=3.5)


#avg_duration_viz <- ggplot(ridership_data_byMonth)+geom_col(mapping=aes(x=mths, y=avg_duration, fill=user_type, size=1)) + facet_wrap(~user_type)

#export these data frames for analysis and visualization in other visualization tools

monthly_group
weekday_group
no_of_users
ride_length_count
trips_df

write.csv(ridership_data_byMonth,file="monthly_group.csv")
write.csv(ridership_data_byWeekDay,file="weekday_group.csv")
write.csv(no_of_users,file="no_of_users.csv")
write.csv(ride_length_count,file="ride_length_count.csv")
write.csv(all_trips,file="final_trips_data.csv")