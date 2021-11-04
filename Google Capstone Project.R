#loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#setting up working directory
setwd("/Users/RONKE/Desktop/csvs/Cyclistic")


#COLLECTING DATA  (12-month trip data)

jul_2020 <- read_csv("202007-divvy-tripdata.csv")
aug_2020 <- read_csv("202008-divvy-tripdata.csv")
sep_2020 <- read_csv("202009-divvy-tripdata.csv")
oct_2020 <- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")

#comparing column names on each of the files to see if they are in the same order

colnames(jul_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)
colnames(jun_2021)
colnames(jul_2021)

# Converting start_station_id and end_station_id to character so that they can stack correctly
jul_2020 <-  mutate(jul_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

aug_2020 <-  mutate(aug_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

sep_2020 <-  mutate(sep_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

oct_2020 <-  mutate(oct_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

nov_2020 <-  mutate(nov_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

dec_2020 <-  mutate(dec_2020,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

jan_2021 <-  mutate(jan_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

feb_2021 <-  mutate(feb_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

mar_2021 <-  mutate(mar_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

apr_2021 <-  mutate(apr_2021,start_station_id = as.character(start_station_id),
                   end_station_id = as.character(end_station_id))

may_2021 <-  mutate(may_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

jun_2021 <-  mutate(jun_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

jul_2021 <-  mutate(jul_2021,start_station_id = as.character(start_station_id),
                    end_station_id = as.character(end_station_id))

#stacking individual dataframes into one big data frame

all_trips <- bind_rows(jul_2020,aug_2020,sep_2020,oct_2020,
                       nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,
                       apr_2021,may_2021,jun_2021,jul_2021)

#removing lat, lng and station names columns
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng,start_station_name,end_station_name))
            

#DATA CLEANING
# Inspecting the new table that has been created
colnames(all_trips)  #List of column names

nrow(all_trips)  #How many rows are in data frame?

dim(all_trips)  #Dimensions of the data frame

head(all_trips)  #See the first 6 rows of data frame

tail(all_trips)  #see the last 6 rows of the dataframe

str(all_trips)  #See list of columns and data types (numeric, character, etc)

summary(all_trips)  #Statistical summary of data

table(all_trips$member_casual) #checking the number of observation under each user-type

# Adding columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd

all_trips$month <- format(as.Date(all_trips$date), "%m")

all_trips$day <- format(as.Date(all_trips$date), "%d")

all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Adding a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspecting the structure of the columns
str(all_trips)

#converting ride_length column to numeric datatype
all_trips$ride_length <- as.numeric(as.difftime(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Removing rows where ride_length is negative and creating a new dataframe
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]

#checking the structure of the new dataframe
str(all_trips_v2)

#DESCRIPTIVE ANALYSIS

mean(all_trips_v2$ride_length) #straight average (total ride length / rides)

median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths

max(all_trips_v2$ride_length) #longest ride

min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# checking the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#ordering the days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# checking the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%           #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

#VISUALIZATIONS
#number of rides by rider_type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
ggsave('number of rides by each rider_type.png')

#average ride duration covered by rider_type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
ggsave('average ride length by riders by day of week.png')

#EXPORTING DATA FOR FURTHER ANALYSIS
# Create a csv file that we will visualize in Excel, Tableau
counts_trip <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts_trip, file = "C:/Users/RONKE/Desktop/csvs/aveg_ride_length.csv")
