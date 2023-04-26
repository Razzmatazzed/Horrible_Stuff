library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)

#The whole loading process should be automated, a project for the future
# Uploading the data for cleaning, NA values included.
nApril_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202204-divvy-tripdata.csv")
nMay_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202205-divvy-tripdata.csv")
nJune_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202206-divvy-tripdata.csv")
nJuly_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202207-divvy-tripdata.csv")
nAugust_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202208-divvy-tripdata.csv")
nSeptember_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202209-divvy-publictripdata.csv")
nOctober_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202210-divvy-tripdata.csv")
nNovember_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202211-divvy-tripdata.csv")
nDecember_22 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202212-divvy-tripdata.csv")
nJanuary_23 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202301-divvy-tripdata.csv")
nFebruary_23 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202302-divvy-tripdata.csv")
nMarch_23 <- read_csv("C:/0work/0Data Analsis Course/Module 8/Capstone 1/Extract/202303-divvy-tripdata.csv")

#### PERSONAL NOTE: DIRECTORIES SHOULD NOT HAVE SPACES
#Combine the data into one workable data frame with NA values, as columns are consistent, full join!

nRaw_Data <- nApril_22 %>% 
  full_join(nMay_22) %>% 
  full_join(nJune_22) %>% 
  full_join(nJuly_22) %>% 
  full_join(nAugust_22) %>% 
  full_join(nSeptember_22) %>% 
  full_join(nOctober_22) %>% 
  full_join(nNovember_22) %>% 
  full_join(nDecember_22) %>% 
  full_join(nJanuary_23) %>% 
  full_join(nFebruary_23) %>%
  full_join(nMarch_23)

# Uploading the data for cleaning, removing cells with missing data. 
Raw_Data <- drop_na(nRaw_Data)

# Let's calculate the percentage of NA values
Percent_dif_of_na <- (count(Raw_Data) / count(nRaw_Data)*100)-100
print(Percent_dif_of_na) # The percentage being 22.7% is quite high. 

#Let's see which columns have missing data
View(colSums(is.na(nRaw_Data))) # The columns that have rows with value == NA do not pertain to this task!
rm(nApril_22, nMay_22, nJune_22, nJuly_22, nAugust_22, nSeptember_22, nOctober_22, nNovember_22,nDecember_22,nJanuary_23,nFebruary_23,nMarch_23)

#Converting to a tibble and making an immediate duplicate
CData <- as_tibble(nRaw_Data) %>% 
  rename("membership" = "member_casual") %>%  #Renaming Columns for cleaning and transformation
  rename("start_coord" = "start_lat") %>% 
  rename("end_coord" = "end_lat") %>% 
  unite(start_coord, start_lng, sep = " ") %>% #Coordinates will not be used, halving the coordinate columns
  unite(end_coord, end_lng, sep = " ") 
#After reviewing, I think this is where the coords become "character" and NA is interpreted as "NA". 
#As the columns unite, the missing values become interpreted as strings

#calculate duration of trips
trip_duration <- as_tibble(difftime(CData$ended_at, CData$started_at, tz))
CData <- add_column(CData, trip_duration, .before = "start_coord")
CData <- CData %>% 
  rename("trip_duration" = "value")
#count trips that last less than 30 seconds to remove outliers
count(trip_duration, trip_duration < 30)
trip_duration <- as.integer(trip_duration$value)
CData <- subset(CData, trip_duration > 30, drop = TRUE) # Dropped 85,183 rows

#Also let's switch trip_duration to base numeric
CData <- mutate(CData, trip_duration = as.numeric(CData$trip_duration))

#Determine the day of the week rides start at!
weekday <- weekdays.POSIXt(CData$started_at, abbreviate = FALSE)
CData <- add_column(CData, weekday, .before = "start_coord")

#Add more variables
trip_date <- as.Date(CData$started_at)
CData <- add_column(CData, trip_date, .before = "start_coord")
trip_month <- format(as.Date(CData$started_at), "%m")
CData <- add_column(CData, trip_month, .before = "start_coord")
trip_day <- format(as.Date(CData$started_at), "%d")
CData <- add_column(CData, trip_day, .before = "start_coord")

#Do a summary of the data
Summary <- CData %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(membership, weekday) %>% 
  summarise(ride_number = n(),
            average_duration = mean(trip_duration)) %>% 
  arrange(membership, weekday)

#Calculate number of rides per day
CData %>% 
  mutate(weekday = day(started_at)) %>% 
  group_by(membership, weekday) %>% 
  summarise(number_of_rides =n())

#Quick calculation
(85183/5803720*100) # We drop ~1.5% of data

#Decide to drop columns that do not pertain to further analysis.
CData <- subset(CData, select = c(-ride_id, -start_station_id, -end_station_id, -start_station_name, -end_station_name, -value))

# Checking for any inconsistencies - again, I don't see it here as NA is interpreted as "NA"
View(colSums(is.na(CData)))

# We know the company stocks Classic Bikes, Electric bikes, Reclining bikes, Hand Tricycles and Cargo bikes. Let's check:
ggplot(data = CData) +
  geom_bar(mapping = aes(x = rideable_type), stat ="count")
#can docked_bike mean *other*? Other than that, data is validated

#Cleaning and manipulation seems to be done!
#Further steps:
#average ride length
#maximum ride length
#weekday operations

avg_trip <- (mean(as.integer(CData$trip_duration)))/60 # 19.26mins
max_trip <- (max(as.integer(CData$trip_duration)))/60 # 41387.25mins
min_trip <- min(as.integer(CData$trip_duration)) # 31secs

#Most of the longest trips do not have trip end coordinates, possible that initial thought of leaving in NA data is incorrect.
#####Personal note - Fix the analysis with drop_na

#Let's check the mode of weekday
v <- CData$weekday

Gmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- Gmode(v)
print(result)

#Let's go visualizing
