# Installed Tidyverse & loaded tidyverse, lubridate, HMS and data.table libraries for my project, I did in Console

# Uploaded Original Data Files between August 2020 to July 2021
aug08_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202008-divvy-tripdata.csv")
sep09_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202009-divvy-tripdata.csv")
oct10_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202010-divvy-tripdata.csv")
nov11_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202011-divvy-tripdata.csv")
dec12_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202012-divvy-tripdata.csv")
jan01_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202101-divvy-tripdata.csv")
feb02_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202102-divvy-tripdata.csv")
mar03_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202103-divvy-tripdata.csv")
apr04_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202104-divvy-tripdata.csv")
may05_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202105-divvy-tripdata.csv")
jun06_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202106-divvy-tripdata.csv")
jul07_df <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Cyclistic_Project/202107-divvy-tripdata.csv")

# Merge the above file into a single file
cyclistic_df <- rbind(aug08_df,sep09_df,oct10_df,nov11_df,dec12_df,jan01_df,feb02_df,mar03_df,apr04_df,jun06_df,jul07_df,may05_df)

# Remove individual month data frames to clear up space in the environment
remove(aug08_df,sep09_df,oct10_df,nov11_df,dec12_df,jan01_df,feb02_df,mar03_df,apr04_df, may05_df,jun06_df,jul07_df)

# Create new data frame - it will contain all of the new columns
cyclistic_data <- cyclistic_df

# Create new columns for the new data frame "cylistic_data"
# Create columns for ride length, hour, day, time, time of day, day of week, month, year, season

# Create Ride Length column by subtracting endtime & startime using difftime function, convert them into minutes
# $ sign help us to step in the cyclistic_data
cyclistic_data$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

# Create date column
# as.date() works like =int() function in excel.
# default format is yyyy-mm-dd, use start data
cyclistic_data$date <- as.Date(cyclistic_data$started_at)

# create day of week column
# wday() works like =weekday() in excel, It only shows the numerical value
cyclistic_data$day_of_week <- wday(cyclistic_df$started_at)
# we can format that numerical value into actual day like we do in Excel.
# Here %d-%m-%y helps us to find the date & Change it as day of the week
cyclistic_data$day_of_week <- weekdays(as.Date(cyclistic_data$date, "%d-%m-%y"))


# Create a column for Month
# We're Formatting the dates of cyclistic_data as month
# Here month calculated as 1 to 12
cyclistic_data$month <- format(as.Date(cyclistic_data$date), "%m")

# Create a column for Day
# We're formatting the dates of cyclistic_data as day
# Here day calculate as 1 to 30 or 31
cyclistic_data$day <- format(as.Date(cyclistic_data$date), "%d")

# Create a column for year
# We're formatting the dates of Cyclistic_data as year
# Here we count year as 2020 or 2021 since we use 2020 to 2021 data's for our analysis
cyclistic_data$year <- format(as.Date(cyclistic_data$date), "")

# Create a column for time
# At first, we'll format the date of cylistic_Data  in HH:MM:SS and create new column for time.
# next we'll find the time in cyclistic_df's started_at column using as_hms function and we'll save them in a time column.
# When I run the below code, it shows me 00:00:00, because in the cyclistic_data's date column don't have time.
cyclistic_data$time <- format(as.Date(cyclistic_data$date),"%H:%M:%S")
cyclistic_data$time <- as_hms((cyclistic_df$started_at))

# Create a column for hour
# use hour function like Excel to return the "hh", we have to load "lubridate" to use "hour" functions
# call the time in the cyclistic_data, to sort out only "hh".
cyclistic_data$hour <- hour(cyclistic_data$time)

# Create a column for different seasons : Spring, Summer, Fall, Winter
# Spring month - 03, 04, 05
# Summer month - 06, 07 , 08
# Fall month - 9, 10, 11
# Winter month - 12, 01, 02
# Used %>% pipeline for output or input functions, before that we have to load "dplyr" library
# mutate function for creating "New Variables" or "New Columns"
cyclistic_data <- cyclistic_data %>% mutate(season = case_when(month == "03" ~ "Spring",
                                                               month == "04" ~ "Spring",
                                                               month == "05" ~ "Spring",
                                                               month == "06" ~ "Summer",
                                                               month == "07" ~ "Summer",
                                                               month == "08" ~ "Summer",
                                                               month == "09" ~ "Fall",
                                                               month == "10" ~ "Fall",
                                                               month == "11" ~ "Fall",
                                                               month == "12" ~ "Winter",
                                                               month == "01" ~ "Winter",
                                                               month == "02" ~ "Winter"))

# Create a column for different time of day: Night, Morning, Afternoon, Evening
# 0 to 5 means Night
# 6 to 11 means Morning
# 12 to 17 means Afternoon
# 18 to 23 meqns Evening
# Used %>% pipeline for output or input functions
# Mutate functions for creating "New Varibles" or "New Columns"
cyclistic_data <- cyclistic_data %>% mutate(time_of_day = 
                                              case_when(hour == "01" ~ "Night",
                                                                    hour == "02" ~ "Night",
                                                                    hour == "03" ~ "Night",
                                                                    hour == "04" ~ "Night",
                                                                    hour == "05" ~ "Night",
                                                                    hour == "06" ~ "Morning",
                                                                    hour == "07" ~ "Morning",
                                                                    hour == "08" ~ "Morning",
                                                                    hour == "09" ~ "Morning",
                                                                    hour == "10" ~ "Morning",
                                                                    hour == "11" ~ "Morning",
                                                                    hour == "12" ~ "Afternoon",
                                                                    hour == "13" ~ "Afternoon",
                                                                    hour == "14" ~ "Afternoon",
                                                                    hour == "15" ~ "Afternoon",
                                                                    hour == "16" ~ "Afternoon",
                                                                    hour == "17" ~ "Afternoon",
                                                                    hour == "18" ~ "Evening",
                                                                    hour == "19" ~ "Evening",
                                                                    hour == "20" ~ "Evening",
                                                                    hour == "21" ~ "Evening",
                                                                    hour == "22" ~ "Evening",
                                                                    hour == "23" ~ "Evening"))

#create a column for the month using the full month name
cyclistic_data <-cyclistic_data %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                             )
)


# Clean the Data

# Remove Rows with NA Values using na.omit() function
cyclistic_data <- na.omit(cyclistic_data)

# Remove duplicate rows using "distinct" function. 
# If it's shows error, check dplyr
cyclistic_data <- distinct(cyclistic_data)

# Remove where ride length is "0" or negtive values
# ! function helps us to remove the the values as per the requiremnets and also we used [] bracket index the data frame.
cyclistic_data <- cyclistic_data[!(cyclistic_data$ride_length <=0), ]

# Remove columns that we don't use
# Remove ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng using piples %>% & Select function
# c() indicates combine or concatenate, so -c do the opposite
cyclistic_data <- cyclistic_data %>%
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

# View the final data
View(cyclistic_data)

# Note: Above analysis in Excel took me like 17hours, but in R it's just 4.5hrs

#-----------------------------------------LET'S CALCULATE TOTAL RIDES--------------------------------------

# Total number of rides
# nrow() functions return the no of rows present in a given data frame
nrow(cyclistic_data)

# ----------------- Member Type -----------------

# group_by() function used to group member & casual
# count() function used to found total rides of member & casual separately
# if %>% gives error, check the library of "dplyr"
library(ggplot2)
cyclistic_data %>%
  group_by(member_casual) %>%
  count(member_casual)


# Create column chart for member type using ggplot2
# Without stat="identity" function, it shows error in geom_bar function
# The stat = "identity" argument tells ggplot2 to use the values in total_number_of_rides for the heights of the bars.
# Note: This is for my practice. I didn't used labels for member & Casual bars since I create data visualization in "Tableu".
ggplot(data = cyclistic_data) + 
  geom_bar(mapping = aes(x= member_casual, y= total_number_of_rides, color= member_casual), stat = "identity") + 
  labs(title = "Total Rides per Member Type")

# ----------------- Type Bike -----------------

# Total rides per Bike Type
cyclistic_data %>%
  group_by(rideable_type) %>%
  count(rideable_type)

# Create Bar chart for Total rides per Bike Type
ggplot(data = cyclistic_data) +
  geom_bar(mapping = aes(x= rideable_type, y= total_number_of_rides, color= rideable_type), stat = "identity") +
  labs(title = "Total Rides per Bike Type")

# Total rides by member type
cyclistic_data %>%
  group_by(member_casual, rideable_type) %>%
  count(rideable_type)

# Create Stacked Chart for Total Rides by Member Vs Casual
# At first, I created bar chart like above, but it's not shown the chart I wanted, So i searched and find the below websites.
# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html - It have multiple sources like bar, stacked chart and more.
# The fill aesthetic is used to colour the inside areas of geoms.
ggplot(cyclistic_data, aes(fill=member_casual, y=total_number_of_rides, x=rideable_type)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Total Rides by Member Vs Casual")

# ----------------- Hour -----------------

# Total Rides by Hour
# Counts functions count the 24 hour here
# when I used Count alone, it only shows limited hours, that's why we used print(n=24)
cyclistic_data %>%
  count(hour) %>%
  print(n=24)

# Create Bar Chart for total rides by hour
ggplot(data = cyclistic_data)+
  geom_bar(mapping = aes(x= hour, y= total_number_of_rides, colour = hour), stat = "identity") +
  labs(title = "Total Rides by Hour")

# Total Rides by Member Type
# For casual & member, i have to separate hours so i used print(n=48)
cyclistic_data %>%
  group_by(member_casual) %>%
  count(hour) %>%
  print(n=48)

# Create Stacked bar chart for total rides by member type
ggplot(cyclistic_data, aes(fill=member_casual, y=total_number_of_rides, x=hour)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Total Rides by Member Vs Casual")

# ----------------- Time of Day -----------------

# ----Morning----

# Total Rides by Member Type
# I used to filter function to check only specific things.
# I don't want to create visualization for every session, I'll create visualization for all time of day
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Morning") %>%
  count(time_of_day)

# Total Rides
cyclistic_data %>%
  filter(time_of_day == "Morning") %>%
  count(time_of_day)

# ----Afternoon----

# Total Rides by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Afternoon") %>%
  count(time_of_day)

# Total Rides
cyclistic_data %>%
  filter(time_of_day == "Afternoon") %>%
  count(time_of_day)

# ----Evening----

# Total Rides by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Evening") %>%
  count(time_of_day)

# Total Rides
cyclistic_data %>%
  filter(time_of_day == "Evening") %>%
  count(time_of_day)

# ----Night----

# Total Rides by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Night") %>%
  count(time_of_day)

# Total Rides
cyclistic_data %>%
  filter(time_of_day == "Night") %>%
  count(time_of_day)

# ----All Times of Day----

# Total Rides by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  count(time_of_day)

# Created a bar chart for Total rides by Member Type
ggplot(data = cyclistic_data)+
  geom_col(mapping = aes(fill = time_of_day, x= member_casual, y= total_number_of_rides, colour = time_of_day)) +
  labs(title = "Total rides by Member Type for All times of day")

# Total Rides
cyclistic_data %>%
  group_by(time_of_day) %>%
  count(time_of_day)

# Created a bar chart for Total rides using geom_col()
# I used geom_col(), instead of geom_bar() due to total rides
ggplot(data = cyclistic_data)+
  geom_col(mapping = aes(fill = time_of_day, x= time_of_day, y= total_number_of_rides, colour = time_of_day)) +
  labs(title = "Total rides by Member Type for All times of day")

# ----------------- Day of the Week -----------------

# Change the order of day_of_week
# Factors are particularly useful for categorical data and can be ordered or unordered.
cyclistic_data$day_of_week <- factor(cyclistic_data$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday"))

#Total Rides by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  count(day_of_week)

# Create a Stacked Chart for total rides by member type for the day of the week
ggplot(data = cyclistic_data)+
  geom_col(mapping = aes(fill = member_casual, x=day_of_week, y=total_number_of_rides, colour = member_casual))+
  labs(title = "Total Rides by Member Type for Day of Week")

# Total rides by day of week
cyclistic_data %>%
  count(day_of_week)

# Create a Bar chart
ggplot(data = cyclistic_data) + geom_col(mapping = aes(x= day_of_week,, y=total_number_of_rides, colour=day_of_week))+
  labs(title = "Total Rides - Day of Week")

# ----------------- Day of the Month -----------------

# Total Rides By member Type
# print(n =62) ensures that all 62 rows (2groups X 31 days) are printed to the console. It helps you to view entire tibble.
cyclistic_data %>%
  group_by(member_casual) %>%
  count(day) %>%
  print(n = 62)

# Create a Stacked Chart for total rides by member type for the day of the month
ggplot(data = cyclistic_data)+
  geom_col(mapping = aes(fill = member_casual, x=month, y=total_number_of_rides, colour = member_casual))+
  labs(title = "Total Rides by Member Type for Day of Month")

# Total Rides
# print(n =31) ensures that all 31 rows (1groups X 31 days) are printed to the console. It helps you to view entire tibble.
cyclistic_data %>%
  count(day) %>%
  print(n = 31)

# Create a bar Chart for total rides - Day of the Month
# Originally, I want to create line chart but I didn't know how to do it.
# I'll add line chart in Tableu ad will learn about R line Chart Later
ggplot(cyclistic_data, aes(x = day, y = total_number_of_rides, colour = member_casual)) +
  geom_col() +
  labs(title = "Total Rides by Day of Month")

# ----------------- Month -----------------

# Total Rides by Member Type
# print(n=24) used to check the entire rows of month
cyclistic_data %>%
  group_by(member_casual) %>%
  count(month) %>%
  print(n=24)

# Create a bar chart for total rides by member type - month
ggplot(data = cyclistic_data) +
  geom_col(mapping = aes(x=month, y=total_number_of_rides, fill = member_casual, colour= member_casual)) +
  labs(title = "Total Rides by Member Type - Month")

# Total Rides
cyclistic_data %>%
  count(month) %>%
  print(n=12)

# Create a bar chart for total rides by month
ggplot(data = cyclistic_data) +
  geom_col(mapping = aes(x=month, y=total_number_of_rides, colour = month ))+
  labs(title = "Total rides by Month")

# ----------------- Season -----------------

# We'll create data visualization for all seasons

# ---- Spring ----
# Total Rides by Member Type - Spring
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(season=="Spring") %>%
  count(season)

# Total Rides - Spring
cyclistic_data %>%
  filter(season == "Spring") %>%
  count(season)

# ---- Summer ----
# Total Rides by Member Type - Summer
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(season == "Summer") %>%
  count(season)

# Total Rides - Summer
cyclistic_data %>%
  filter(season == "Summer") %>%
  count(season)

# ---- Fall ----
# Total Rides by Member Type - Fall
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(season == "Fall") %>%
  count(season)

# Total Rides - Fall
cyclistic_data %>%
  filter(season == "Fall") %>%
  count(season)

# ---- Winter ----
# Total Rides by Member Type - Winter
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(season == "Winter") %>%
  count(season)

# Total Rides - Winter
cyclistic_data %>%
  filter(season == "Winter") %>%
  count(season)

# ---- All seasons ----
# Total Rides by Member Type - All Seasons
cyclistic_data %>%
  group_by(member_casual) %>%
  count(season)

# Created a bar chart for Total Rides by Member Type - All Seasons
ggplot(data = cyclistic_data)+
  geom_col(mapping = aes(x=season, y=total_number_of_rides, fill = member_casual))+
  labs(title = "Total Rides by Member Type - All Seasons")

# Total Rides - All Seasons
cyclistic_data %>%
  count(season)

# Created a bar chart for Total Rides - All Seasons
ggplot(data = cyclistic_data) +
  geom_col(mapping = aes(x=season, y=total_number_of_rides, colour = season)) +
  labs(title =  "Total Rides for All Seasons")
  
#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

# Average of Ride Length
# we're using mean() function like we did it Excel
cyclistic_avgRide <- mean(cyclistic_data$ride_length)
print(cyclistic_avgRide)

#------------------Member Type----------------------

# Calculating Average of Ride Length by member type
# summarise_at() - it allows you to apply summary functions to specific columns.
# vars() - specifies the column to summarise, in this case ride_length
# list() - Created a named list
cyclistic_data %>% 
  group_by(member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean))

#------------------ Type of Bike ----------------------

# Average rides Length by member Type - Type Bike 
cyclistic_data %>%
  group_by(member_casual, rideable_type) %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Type Bike
cyclistic_data %>%
  group_by(rideable_type) %>%
  summarise_at(vars(ride_length), list(time = mean))

#------------------ Hour ----------------------

# Average Rides Length by Member Type - Hours
cyclistic_data %>%
  group_by(hour, member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean)) %>%
  print(n=48)

# Average Rides by Hours
cyclistic_data %>%
  group_by(hour) %>%
  summarise_at(vars(ride_length), list(time = mean)) %>%
  print(n=24)

#------------------ Time of Day ----------------------

#------- Morning ------

# Average Ride Length by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Morning") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length in the Morning
cyclistic_data %>%
  filter(time_of_day == "Morning") %>%
  summarise_at(vars(ride_length), list(time =  mean))

#------- Afternoon ------

# Average Ride Length by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Afternoon") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length in the Afternoon
cyclistic_data %>%
  filter(time_of_day == "Afternoon") %>%
  summarise_at(vars(ride_length), list(time =  mean))

#------- Evening ------

# Average Ride Length by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Evening") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length in the Evening
cyclistic_data %>%
  filter(time_of_day == "Evening") %>%
  summarise_at(vars(ride_length), list(time =  mean))

#------- Night ------

# I don't why the "Night" value are not there in my sheet, I'll check later.

# Average Ride Length by Member Type
cyclistic_data %>%
  group_by(member_casual) %>%
  filter(time_of_day == "Night") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length in the Night
cyclistic_data %>%
  filter(time_of_day == "Night") %>%
  summarise_at(vars(ride_length), list(time =  mean))

#------- All times ------
# Average Ride Length by Member Type
cyclistic_data %>%
  group_by(time_of_day,member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length in the Average
cyclistic_data %>%
  group_by(time_of_day) %>%
  summarise_at(vars(ride_length), list(time =  mean))


#------------------ Day of the Week ----------------------

# Average Ride Length by Member type - Day of Week
cyclistic_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Day of Week
cyclistic_data %>%
  group_by(day_of_week) %>%
  summarise_at(vars(ride_length), list(time = mean))

#------------------ Day of the Month ----------------------

# Average Ride Length by Member Type - Day of the Month
cyclistic_data %>%
  group_by(day, member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean)) %>%
  print(n=62)

# Average Ride Length- Day of the Month
cyclistic_data %>%
  group_by(day) %>%
  summarise_at(vars(ride_length), list(time = mean)) %>%
  print(n=31)

#------------------ Month ----------------------

# Average Ride Length by Member Type - Month
cyclistic_data %>%
  group_by(month, member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean)) %>%
  print(n=24)

# Average Ride Length - Month
cyclistic_data %>%
  group_by(month) %>%
  summarise_at(vars(ride_length), list(time = mean))

#------------------ Season ----------------------

#------ Spring -------

# Average Ride Length by Member Type - Spring
cyclistic_data %>%
  group_by(season, member_casual) %>%
  filter(season == "Spring") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Spring
cyclistic_data %>%
  group_by(season) %>%
  filter(season == "Spring") %>%
  summarise_at(vars(ride_length), list(time = mean))

#------ Summer -------

# Average Ride Length by Member Type - Summer
cyclistic_data %>%
  group_by(season, member_casual) %>%
  filter(season == "Summer") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Summer
cyclistic_data %>%
  group_by(season) %>%
  filter(season == "Summer") %>%
  summarise_at(vars(ride_length), list(time = mean))

#------ Fall -------

# Average Ride Length by Member Type - Fall
cyclistic_data %>%
  group_by(season, member_casual) %>%
  filter(season == "Fall") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Fall
cyclistic_data %>%
  group_by(season) %>%
  filter(season == "Fall") %>%
  summarise_at(vars(ride_length), list(time = mean))

#------ Winter -------

# Average Ride Length by Member Type - Winter
cyclistic_data %>%
  group_by(season, member_casual) %>%
  filter(season == "Winter") %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length - Winter
cyclistic_data %>%
  group_by(season) %>%
  filter(season == "Winter") %>%
  summarise_at(vars(ride_length), list(time = mean))

#------ All seasons -------

# Average Ride Length by Member Type - All Seasons
cyclistic_data %>%
  group_by(season, member_casual) %>%
  summarise_at(vars(ride_length), list(time = mean))

# Average Ride Length -  All Seasons
cyclistic_data %>%
  group_by(season) %>%
  summarise_at(vars(ride_length), list(time = mean))

#------------------------------------Find out the Stations----------------------------------

#------Total Rides per station -------
# Calculate the total rides for start station
# Summarise - helps us to count no of rides at each stations
# Renamed the column for easy concatenation
start_station_rides <- cyclistic_data %>%
  group_by(start_station_name) %>%
  summarise(total_rides_start = n()) %>%
  rename(station_name = start_station_name)

# Calculate the total rides for end station
end_station_rides <- cyclistic_data %>%
  group_by(end_station_name) %>%
  summarise(total_rides_end = n()) %>%
  rename(station_name = end_station_name)

# Combine the start and end rides to get the total rides per station
# bind_rows() use to combine the start and end station rides
#  na.rm = TRUE - helps to ignore the NA Values in our data set
total_rides_per_station <- bind_rows(start_station_rides, end_station_rides) %>%
  group_by(station_name) %>%
  summarise(total_rides = sum(total_rides_start, total_rides_end, na.rm = TRUE))

#------Most Popular station -------
# Identify stations with most and least rides
most_ridden_station <- total_rides_per_station %>%
  filter(total_rides == max(total_rides, na.rm = TRUE))

least_ridden_station <- total_rides_per_station %>%
  filter(total_rides == min(total_rides, na.rm = TRUE))

# Calculate average rides per station
average_rides <- mean(total_rides_per_station$total_rides, na.rm = TRUE)

# Create a summary table
# tibble() - Create a summary table with the names and ride counts of the most and least ridden stations, along with the average rides.
station_stats <- tibble(
  most_ridden_station_name = most_ridden_station$station_name,
  most_rides = most_ridden_station$total_rides,
  least_ridden_station_name = least_ridden_station$station_name,
  least_rides = least_ridden_station$total_rides,
  average_rides = average_rides
)

print(total_rides_per_station)
print(station_stats)

