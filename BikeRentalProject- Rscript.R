install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("anytime")
library(anytime)
install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)

library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("scales")
library(scales)


#CLEANING 12MONTHS DATA ONE BY ONE:

Apr2020 <- read_csv("202004-divvy-tripdata.csv")
May2020 <- read_csv("202005-divvy-tripdata.csv")
June2020 <- read_csv("202006-divvy-tripdata.csv")
July2020 <- read_csv("202007-divvy-tripdata.csv")
Aug2020 <- read_csv("202008-divvy-tripdata.csv")
Sep2020 <- read_csv("202009-divvy-tripdata.csv")
Oct2020 <- read_csv("202010-divvy-tripdata.csv")
Nov2020 <- read_csv("202011-divvy-tripdata.csv")
Dec2020 <- read_csv("202012-divvy-tripdata.csv")
Jan2021 <- read_csv("202101-divvy-tripdata.csv")
Feb2021 <- read_csv("202102-divvy-tripdata.csv")
Mar2021 <- read_csv("202103-divvy-tripdata.csv")


#I discovered the time data in Apr is in character format but for the rest of dataset they are datetime format.Now I will convert Apr
#time data from chr to datetime.

#CLEANING APRIL DATA

which(is.na(Apr2020$ride_id)) 
which(is.na(Apr2020$rideable_type))
which(is.na(Apr2020$started_at))
which(is.na(Apr2020$ended_at))
which(is.na(Apr2020$start_station_name)) 
which(is.na(Apr2020$start_station_id)) 
which(is.na(Apr2020$end_station_name)) #100 nulls around
which(is.na(Apr2020$end_station_id)) #100 nulls around
which(is.na(Apr2020$start_lat))
which(is.na(Apr2020$start_lng))
which(is.na(Apr2020$end_lat)) #100 around nulls
which(is.na(Apr2020$end_lng)) #100 around nulls
which(is.na(Apr2020$member_casual))
#I am checking if these have nulls and the location of nulls.

n_distinct(unique(Apr2020)) #checking if each row is unique or not, if not will have to cut the duplicates. The result number matched with rows mentioned in metadata so all cool.

n_distinct(Apr2020$ride_id)
n_distinct(Apr2020$rideable_type) #1
unique(Apr2020$rideable_type) #"docked_bike"
n_distinct((Apr2020$start_station_name),na.rm=TRUE) #602
n_distinct((Apr2020$start_station_id),na.rm=TRUE) #602
n_distinct((Apr2020$end_station_name),na.rm=TRUE) #600
n_distinct((Apr2020$end_station_id),na.rm=TRUE) #600
n_distinct(Apr2020$member_casual) #2

all(Apr2020$end_station_name %in% Apr2020$start_station_name, na.rm=TRUE) #FALSE, checked if former is a subset of latter

range(Apr2020$start_lat) #41.7366 42.0649
range(Apr2020$start_lng) #-87.7747 -87.5494
range(Apr2020$end_lat, na.rm=TRUE) #41.7366 42.0649
range(Apr2020$end_lng, na.rm=TRUE) #-87.7747 -87.5494 , did this to see outliers

nrow(filter(Apr2020, started_at>ended_at)) #36, THIS IS WRONG AS THIS COLUMN IS NOT DATETIME HERE

apr1 <- Apr2020 %>% 
  mutate(started_at = dmy_hm(started_at),
         ended_at = dmy_hm(ended_at))

nrow(filter(apr1, started_at>ended_at)) #14, THIS IS RIGHT AS THIS COLUMN IS NOW DATETIME. checked where start time is after the end time.

#CLEANING MAY DATA

which(is.na(May2020$ride_id)) 
which(is.na(May2020$rideable_type))
which(is.na(May2020$started_at))
which(is.na(May2020$ended_at))
which(is.na(May2020$start_station_name))
which(is.na(May2020$start_station_id))
which(is.na(May2020$end_station_name)) #many of these missing
which(is.na(May2020$end_station_id)) #same no.of things missing here
which(is.na(May2020$start_lat))
which(is.na(May2020$start_lng))
which(is.na(May2020$end_lat)) #same no.of things missing here
which(is.na(May2020$end_lng)) #same no.of things missing here
which(is.na(May2020$member_casual))
#I checked for NA with this function,4 columns have NA

n_distinct(unique(May2020))
#I checked how many unique values, found the number to match the figure in the metadata, so there is no duplicate rows
n_distinct(unique(May2020$ride_id))
#I checked how many unique values, found the number to match the figure in the metadata, so there is no duplicate entry
n_distinct(unique(May2020$rideable_type))
#Only 1 ride_able is found here
n_distinct(unique(May2020$start_station_name))
#608 unique start_station_name found
n_distinct(unique(May2020$start_station_id))
#608 unique start_station_id is found, so it is alright
n_distinct(May2020$end_station_name, na.rm=TRUE)
#611 unique end_station.This is another way to count. ra.nm=TRUE means NAs do not count.
n_distinct(May2020$end_station_id, na.rm=TRUE)
#611 is the result
n_distinct(May2020$start_lat)
#522
n_distinct(May2020$start_lng)
#489
n_distinct(May2020$end_lat, na.rm=TRUE)
#524
n_distinct(May2020$end_lng, na.rm=TRUE)
#490
n_distinct(May2020$member_casual)
#2 distinct values

all(May2020$start_station_name %in% May2020$end_station_name)
#I was checking if former is a subset of latter and the result is TRUE.
range(May2020$start_lat)
#(41.7366 to 42.0649)Just checking the range to see for any outliers
range(May2020$start_lng)
#(-87.7747 to -87.5494)Just checking the range to see outliers
range(May2020$end_lat, na.rm=TRUE)
#(41.7366 42.0649),same as start_lat
range(May2020$end_lng, na.rm=TRUE)
#(-87.7747 to -87.5494), same as end


nrow(filter(May2020, started_at>ended_at))
#172 rows where start time is before end time. This is definitely an error.

nrow((May2020$started_at>May2020$ended_at) & is.null(May2020$end_station_id))
#tried to find number of rows where the above condition stands. The result NULL probably means 0(zero).However later by calculating i find there are 2 overlaps.

May2020_cl <- May2020 %>% 
  na.omit(May2020) %>% 
  filter(started_at<ended_at | started_at==ended_at)

#CLEANING JUNE2020 DATA

which(is.na(June2020$ride_id)) 
which(is.na(June2020$rideable_type))
which(is.na(June2020$started_at))
which(is.na(June2020$ended_at))
which(is.na(June2020$start_station_name))
which(is.na(June2020$start_station_id))
which(is.na(June2020$end_station_name)) #many of these missing, 469
which(is.na(June2020$end_station_id)) #same no.of things missing here
which(is.na(June2020$start_lat))
which(is.na(June2020$start_lng))
which(is.na(June2020$end_lat)) #same no.of things missing here, 469
which(is.na(June2020$end_lng)) #same no.of things missing here
which(is.na(June2020$member_casual))

#I checked for NA with this function, 4 columns have NA

n_distinct(unique(June2020))
#I checked how many unique values, found the number to match the figure in the metadata, so there is no duplicate rows
n_distinct(unique(June2020$ride_id))
#Number of unique entries matches the fugure in the metadata
n_distinct(unique(June2020$rideable_type))
#only 1 rideable type
n_distinct(unique(June2020$start_station_name))
#616
n_distinct(unique(June2020$start_station_id))
#611 ids. This means there must be some ERROR either in this or in start_station_name column
n_distinct(June2020$start_station_id)
#611
n_distinct(unique(June2020$end_station_name, na.rm=TRUE))
#620, THIS IS WRONG RESULT, THE LAST ARGUMNT IS BEING AVOIDED HERE
n_distinct(June2020$end_station_name, na.rm=TRUE)
#619
n_distinct(unique(June2020$end_station_name), na.rm=TRUE)
#619
#THIS SHOWS USING n_distinct+unique ultimately ends up ignoring the na.rm condition and counting nulls
#if we put this argument inside brackets,hence when nulls are there we should just use n_distinct
n_distinct(June2020$end_station_id, na.rm=TRUE)
#612
June2020_1 <- June2020 
unique_station_ids_inJune2020 <- select(June2020, c('start_station_name','start_station_id'))
View(unique_station_ids_inJune2020)
for(x in 1:length(unique_station_ids_inJune2020$start_station_id))
{ifelse(unique(unique_station_ids_inJune2020$start_station_name)>1,
  print(unique_station_ids_inJune2020$start_station_id))}
    #Sayan to continue tomorrow

View(June2020_1)

n_distinct(June2020$start_lat)
#706
n_distinct(June2020$start_lng)
#702
n_distinct(June2020$end_lat, na.rm=TRUE)
#792
n_distinct(June2020$end_lng, na.rm=TRUE)
#790
#checking distinct vals for these variables is really not required
n_distinct(June2020$member_casual)
#2. all cool.

all(June2020$start_station_name %in% June2020$end_station_name)
#TRUE. So former is a subset of latter.
range(June2020$start_lat)
#41.73665 42.06485 , no discrepancy here
range(June2020$start_lng)
#-87.77470 -87.54939, no discrep
range(June2020$end_lat, na.rm=TRUE)
#41.73665 42.06485 ,no discrep
range(June2020$end_lng, na.rm=TRUE)
#-87.77470 -87.54939, no discrep

nrow(filter(June2020,started_at>ended_at)) #469 values

#CLEANING JULY2020 DATA

which(is.na(July2020$ride_id)) 
which(is.na(July2020$rideable_type))
which(is.na(July2020$started_at))
which(is.na(July2020$ended_at))
which(is.na(July2020$start_station_name)) #many nulls,150nulls
which(is.na(July2020$start_station_id)) #many nulls, not neccessarily same start station name objects,153nulls
which(is.na(July2020$end_station_name)) #many nulls,968, all different elements, no neccessary match with previous columns
which(is.na(July2020$end_station_id)) #many nulls,970
which(is.na(July2020$start_lat))
which(is.na(July2020$start_lng))
which(is.na(July2020$end_lat)) #around 770 nulls
which(is.na(July2020$end_lng)) #around 770 nulls
which(is.na(July2020$member_casual))

#I checked for nulls, found them in 6 columns, all different rows mising

n_distinct(unique(July2020))
#all unique
n_distinct(unique(July2020$ride_id))
n_distinct(unique(July2020$rideable_type)) #2types of rideable
n_distinct(July2020$start_station_name, na.rm=TRUE) #619
n_distinct(July2020$start_station_id, na.rm=TRUE) #618, this shows one single ID is shared by two different stations, check if this is an error
n_distinct(July2020$end_station_name, na.rm=TRUE) #622
n_distinct(July2020$end_station_id, na.rm=TRUE) #619, this show one single ID is shared by two or more stations, check if this is an error
n_distinct(July2020$member_casual) #2, this means it is okay

all(July2020$start_station_name %in% July2020$end_station_name, na.rm=TRUE)
#I checked if former is subset of latter and this is true

range(July2020$start_lat)
#41.73665 42.06485,no discrep
range(July2020$start_lng)
#-87.77470 -87.54939,no discrep
range(July2020$end_lat,na.rm=TRUE)
#41.72927 42.06485,no discrep
range(July2020$end_lng, na.rm=TRUE)
#-87.77470 -87.54939, no discrep

nrow(filter(July2020, started_at>ended_at)) #1745 values

#CLEANING AUGUST2020 DATA

which(is.na(Aug2020$ride_id)) 
which(is.na(Aug2020$rideable_type))
which(is.na(Aug2020$started_at))
which(is.na(Aug2020$ended_at))
which(is.na(Aug2020$start_station_name)) #many nulls, over 6600 nulls
which(is.na(Aug2020$start_station_id)) #many nulls, around 6800 nulls
which(is.na(Aug2020$end_station_name)) #many nulls,around 10035
which(is.na(Aug2020$end_station_id)) #many nulls,around 10110
which(is.na(Aug2020$start_lat))
which(is.na(Aug2020$start_lng))
which(is.na(Aug2020$end_lat)) #around 939 nulls
which(is.na(Aug2020$end_lng)) #around 939 nulls
which(is.na(Aug2020$member_casual))

n_distinct(unique(Aug2020)) #matched with metadata

n_distinct(Aug2020$ride_id)
n_distinct(Aug2020$rideable_type) #2
n_distinct((Aug2020$start_station_name),na.rm=TRUE) #636
n_distinct((Aug2020$start_station_id),na.rm=TRUE) #633
n_distinct((Aug2020$end_station_name),na.rm=TRUE) #636
n_distinct((Aug2020$end_station_id),na.rm=TRUE) #633
n_distinct(Aug2020$member_casual) #2

all(Aug2020$start_station_name %in% Aug2020$end_station_name, na.rm=TRUE) #TRUE

range(Aug2020$start_lat) #41.66 42.07
range(Aug2020$start_lng) #-87.87 -87.53
range(Aug2020$end_lat, na.rm=TRUE) #41.66 42.16
range(Aug2020$end_lng, na.rm=TRUE) #-87.89 -87.53

nrow(filter(Aug2020, started_at>ended_at)) #2769

#CLEANING SEPT DATA


which(is.na(Sep2020$ride_id)) 
which(is.na(Sep2020$rideable_type))
which(is.na(Sep2020$started_at))
which(is.na(Sep2020$ended_at))
which(is.na(Sep2020$start_station_name)) #sround 19691 nulls
which(is.na(Sep2020$start_station_id)) #around 19901 nulls
which(is.na(Sep2020$end_station_name)) #around 23373 nulls
which(is.na(Sep2020$end_station_id)) #around 23524 nulls
which(is.na(Sep2020$start_lat))
which(is.na(Sep2020$start_lng))
which(is.na(Sep2020$end_lat)) # around 800 nulls
which(is.na(Sep2020$end_lng)) # around 800 nulls
which(is.na(Sep2020$member_casual))

n_distinct(unique(Sep2020))

n_distinct(Sep2020$ride_id)
n_distinct(Sep2020$rideable_type) #2
n_distinct((Sep2020$start_station_name),na.rm=TRUE) #664
n_distinct((Sep2020$start_station_id),na.rm=TRUE) #656
n_distinct((Sep2020$end_station_name),na.rm=TRUE) #663
n_distinct((Sep2020$end_station_id),na.rm=TRUE) #655
n_distinct(Sep2020$member_casual)

all(Sep2020$end_station_name %in% Sep2020$start_station_name, na.rm=TRUE) #FALSE. No idea how to deal with this

range(Sep2020$start_lat) #41.65187 42.08000
range(Sep2020$start_lng) #-87.84 -87.52
range(Sep2020$end_lat, na.rm=TRUE) #41.64 42.12
range(Sep2020$end_lng, na.rm=TRUE) #-87.88 -87.52

nrow(filter(Sep2020, started_at>ended_at)) #2132

#CLEANING OCTOBER TRIPDATA

which(is.na(Oct2020$ride_id)) 
which(is.na(Oct2020$rideable_type))
which(is.na(Oct2020$started_at))
which(is.na(Oct2020$ended_at))
which(is.na(Oct2020$start_station_name)) #31198 nulls around
which(is.na(Oct2020$start_station_id)) #31405 nulls around
which(is.na(Oct2020$end_station_name)) #35631 nulls around
which(is.na(Oct2020$end_station_id)) #35787 nulls around
which(is.na(Oct2020$start_lat))
which(is.na(Oct2020$start_lng))
which(is.na(Oct2020$end_lat)) # around 500 nulls
which(is.na(Oct2020$end_lng)) # around 500 nulls
which(is.na(Oct2020$member_casual))

n_distinct(unique(Oct2020))

n_distinct(Oct2020$ride_id)
n_distinct(Oct2020$rideable_type) #2
n_distinct((Oct2020$start_station_name),na.rm=TRUE) #669
n_distinct((Oct2020$start_station_id),na.rm=TRUE) #664
n_distinct((Oct2020$end_station_name),na.rm=TRUE) #669
n_distinct((Oct2020$end_station_id),na.rm=TRUE) #664
n_distinct(Oct2020$member_casual) #2

all(Oct2020$end_station_name %in% Oct2020$start_station_name, na.rm=TRUE) #FALSE. This means not all start stations are contained in end stations and vice vera.

range(Oct2020$start_lat) #41.64 42.08
range(Oct2020$start_lng) #-87.80 -87.52
range(Oct2020$end_lat, na.rm=TRUE) #41.63 42.15
range(Oct2020$end_lng, na.rm=TRUE) #-87.84 -87.52

nrow(filter(Oct2020, started_at>ended_at)) #1911

#CLEANING NOV2020 DATA

which(is.na(Nov2020$ride_id)) 
which(is.na(Nov2020$rideable_type))
which(is.na(Nov2020$started_at))
which(is.na(Nov2020$ended_at))
which(is.na(Nov2020$start_station_name)) #24324 nulls around
which(is.na(Nov2020$start_station_id)) # 24434 around nulls
which(is.na(Nov2020$end_station_name)) # 26749 nulls around
which(is.na(Nov2020$end_station_id)) #26826 nulls around
which(is.na(Nov2020$start_lat))
which(is.na(Nov2020$start_lng))
which(is.na(Nov2020$end_lat)) #285 nulls around
which(is.na(Nov2020$end_lng)) #285 nulls around
which(is.na(Nov2020$member_casual))

n_distinct(unique(Nov2020))

n_distinct(Nov2020$ride_id)
n_distinct(Nov2020$rideable_type) #2
n_distinct((Nov2020$start_station_name),na.rm=TRUE) #665
n_distinct((Nov2020$start_station_id),na.rm=TRUE) #661
n_distinct((Nov2020$end_station_name),na.rm=TRUE) #657
n_distinct((Nov2020$end_station_id),na.rm=TRUE) #656
n_distinct(Nov2020$member_casual) #2

all(Nov2020$end_station_name %in% Nov2020$start_station_name, na.rm=TRUE) #FALSE

range(Nov2020$start_lat) #41.6485 42.0800
range(Nov2020$start_lng) #-87.79000 -87.52823
range(Nov2020$end_lat, na.rm=TRUE) #41.54 42.15
range(Nov2020$end_lng, na.rm=TRUE) #-87.87 -87.44

nrow(filter(Nov2020, started_at>ended_at)) #865

#CLEANING DECEMBER DATA

which(is.na(Dec2020$ride_id)) 
which(is.na(Dec2020$rideable_type))
which(is.na(Dec2020$started_at))
which(is.na(Dec2020$ended_at))
which(is.na(Dec2020$start_station_name)) #11699 nulls around
which(is.na(Dec2020$start_station_id)) #11699 nulls around
which(is.na(Dec2020$end_station_name)) #13237 nulls around
which(is.na(Dec2020$end_station_id)) #13237 nulls around
which(is.na(Dec2020$start_lat))
which(is.na(Dec2020$start_lng))
which(is.na(Dec2020$end_lat)) #112 nulls
which(is.na(Dec2020$end_lng)) #112 nulls
which(is.na(Dec2020$member_casual))

n_distinct(unique(Dec2020))

n_distinct(Dec2020$ride_id)
n_distinct(Dec2020$rideable_type) #3
unique(Dec2020$rideable_type) #classic_bike,  electric_bike, docked_bike
n_distinct((Dec2020$start_station_name),na.rm=TRUE) #644
n_distinct((Dec2020$start_station_id),na.rm=TRUE) #642
n_distinct((Dec2020$end_station_name),na.rm=TRUE) #643
n_distinct((Dec2020$end_station_id),na.rm=TRUE) #641
n_distinct(Dec2020$member_casual) #2

all(Dec2020$end_station_name %in% Dec2020$start_station_name, na.rm=TRUE) #FALSE

range(Dec2020$start_lat) #41.65 42.07
range(Dec2020$start_lng) #-87.78000 -87.52823
range(Dec2020$end_lat, na.rm=TRUE) #41.65 42.07
range(Dec2020$end_lng, na.rm=TRUE) #-87.79000 -87.52823

nrow(filter(Dec2020, started_at>ended_at)) #434

#CLEANING JANUARY2021 DATA

which(is.na(Jan2021$ride_id)) 
which(is.na(Jan2021$rideable_type))
which(is.na(Jan2021$started_at))
which(is.na(Jan2021$ended_at))
which(is.na(Jan2021$start_station_name)) #8625 nulls
which(is.na(Jan2021$start_station_id)) #8625 nulls
which(is.na(Jan2021$end_station_name)) #9377 nulls
which(is.na(Jan2021$end_station_id)) #9377 nulls
which(is.na(Jan2021$start_lat))
which(is.na(Jan2021$start_lng))
which(is.na(Jan2021$end_lat)) #100 nulls around
which(is.na(Jan2021$end_lng)) #100 nulls around
which(is.na(Jan2021$member_casual))

n_distinct(unique(Jan2021))

n_distinct(Jan2021$ride_id)
n_distinct(Jan2021$rideable_type) #3
unique(Jan2021$rideable_type) #electric_bike" "classic_bike"  "docked_bike"
n_distinct((Jan2021$start_station_name),na.rm=TRUE) #640
n_distinct((Jan2021$start_station_id),na.rm=TRUE) #638
n_distinct((Jan2021$end_station_name),na.rm=TRUE) #632
n_distinct((Jan2021$end_station_id),na.rm=TRUE) #629
n_distinct(Jan2021$member_casual) #2

all(Jan2021$end_station_name %in% Jan2021$start_station_name, na.rm=TRUE) #FALSE

range(Jan2021$start_lat) #41.64000 42.06485
range(Jan2021$start_lng) #-87.78000 -87.52823
range(Jan2021$end_lat, na.rm=TRUE) #41.64 42.07
range(Jan2021$end_lng, na.rm=TRUE) #-87.81 -87.51

nrow(filter(Jan2021, started_at>ended_at)) #2

#CLEANING FEBRUARY 2021 DATA

which(is.na(Feb2021$ride_id)) 
which(is.na(Feb2021$rideable_type))
which(is.na(Feb2021$started_at))
which(is.na(Feb2021$ended_at))
which(is.na(Feb2021$start_station_name)) #4046 nulls around
which(is.na(Feb2021$start_station_id)) #4046 nulls around
which(is.na(Feb2021$end_station_name)) #5358 nulls around
which(is.na(Feb2021$end_station_id)) #5358 nulls around
which(is.na(Feb2021$start_lat))
which(is.na(Feb2021$start_lng))
which(is.na(Feb2021$end_lat)) #214 nulls around
which(is.na(Feb2021$end_lng)) #214 nulls around
which(is.na(Feb2021$member_casual))

n_distinct(unique(Feb2021))

n_distinct(Feb2021$ride_id)
n_distinct(Feb2021$rideable_type) #3
unique(Feb2021$rideable_type) #"classic_bike"  "electric_bike" "docked_bike" 
n_distinct((Feb2021$start_station_name),na.rm=TRUE) #582
n_distinct((Feb2021$start_station_id),na.rm=TRUE) #582
n_distinct((Feb2021$end_station_name),na.rm=TRUE) #584
n_distinct((Feb2021$end_station_id),na.rm=TRUE) #584
n_distinct(Feb2021$member_casual) #2

all(Feb2021$start_station_name %in% Feb2021$end_station_name, na.rm=TRUE) #FALSE

range(Feb2021$start_lat) #41.65000 42.06485
range(Feb2021$start_lng) #-87.77470 -87.53481
range(Feb2021$end_lat, na.rm=TRUE) #41.54 42.07
range(Feb2021$end_lng, na.rm=TRUE) #-87.77470 -87.53481

nrow(filter(Feb2021, started_at>ended_at)) #0

#CLEANING MARCH 2021 DATA

which(is.na(Mar2021$ride_id)) 
which(is.na(Mar2021$rideable_type))
which(is.na(Mar2021$started_at))
which(is.na(Mar2021$ended_at))
which(is.na(Mar2021$start_station_name)) #14848 nulls around
which(is.na(Mar2021$start_station_id)) #14848 nulls around
which(is.na(Mar2021$end_station_name)) #16727 nulls around
which(is.na(Mar2021$end_station_id)) #16727 nulls around
which(is.na(Mar2021$start_lat))
which(is.na(Mar2021$start_lng))
which(is.na(Mar2021$end_lat)) #167 nulls around
which(is.na(Mar2021$end_lng)) #167 nulls
which(is.na(Mar2021$member_casual))

n_distinct(unique(Mar2021))

n_distinct(Mar2021$ride_id)
n_distinct(Mar2021$rideable_type) #3
unique(Mar2021$rideable_type) #"classic_bike"  "electric_bike" "docked_bike" 
n_distinct((Mar2021$start_station_name),na.rm=TRUE) #673
n_distinct((Mar2021$start_station_id),na.rm=TRUE) #673
n_distinct((Mar2021$end_station_name),na.rm=TRUE) #673
n_distinct((Mar2021$end_station_id),na.rm=TRUE) #673
n_distinct(Mar2021$member_casual) #2

all(Mar2021$start_station_name %in% Mar2021$end_station_name, na.rm=TRUE) #FALSE

range(Mar2021$start_lat) #41.6485 42.0700
range(Mar2021$start_lng) #-87.78000 -87.52823
range(Mar2021$end_lat, na.rm=TRUE) #41.64 42.08
range(Mar2021$end_lng, na.rm=TRUE) #-88.07000 -87.52823

nrow(filter(Mar2021, started_at>ended_at)) #2

#TRANSFORMING DATA


tripdata <- rbind(apr1, May2020, June2020, July2020, Aug2020, Sep2020, Oct2020, Nov2020, Dec2020, Jan2021, Feb2021, Mar2021)

tripdata1 <- tripdata %>% 
  filter(started_at<ended_at | started_at==ended_at) #filtering out wrong entries

station_name_lat_lng <- select(tripdata1,c(station_name="start_station_name",lat="start_lat",lng="start_lng"))
station_name_lat_lng1 <- station_name_lat_lng %>% 
  na.omit(station_name_lat_lng) %>% 
  distinct(station_name,lat,lng,.keep_all=TRUE)
#start_lat and start_lng has no missing, so I made a table with them and station_name. Dropped the nulls and then the duplicates.

station_id_lat_lng <- select(tripdata1,c(station_id="start_station_id",lat='start_lat',lng="start_lng"))
station_id_lat_lng1 <- station_id_lat_lng %>% 
  na.omit(station_id_lat_lng) %>% 
  distinct(station_id,lat,lng,.keep_all=TRUE)
#start_lat and start_lng has no missing, so I made a table with them and station_id. Dropped the nulls and then the duplicates.


tripdata_2 <- tripdata1
index <- with(tripdata_2, which(is.na(tripdata_2$start_station_id)))
for(i in 1:length(index))
{
  indexofcorrection <- with(station_id_lat_lng1, which(station_id_lat_lng1$lat == tripdata_2$start_lat[index[i]] & station_id_lat_lng1$lng == tripdata_2$start_lng[index[i]]))
  if (length(indexofcorrection) > 0)
  {
    actualind <- indexofcorrection[1]
    tripdata_2$start_station_id[index[i]] <- station_id_lat_lng1$station_id[actualind]
  }
}

length(which(is.na(tripdata_2$start_station_id))) #112873 nulls

#i ran a loop. Here I filled the nulls at start_station_id, by looking at their corresponding start_lat and start_lng. I searched this combo of lat and lng
#in the mastertable called "station_id_lat_lng1", found its corresponding station_id and replaced the nulls in tripdata_2 col with this one.


index2 <- with(tripdata_2, which(is.na(tripdata_2$start_station_name)))
for(i in 1:length(index2))
{
  indexofcorrection2 <- with(station_name_lat_lng1, which(station_name_lat_lng1$lat == tripdata_2$start_lat[index2[i]] & station_name_lat_lng1$lng == tripdata_2$start_lng[index2[i]]))
  if (length(indexofcorrection2) > 0)
  {
    actualind2 <- indexofcorrection2[1]
    tripdata_2$start_station_name[index2[i]] <- station_name_lat_lng1$station_name[actualind2]
  }
}

length(which(is.na(tripdata_2$start_station_name))) #110744 nulls

#filled the nulls at start_station_name

index3 <- with(tripdata_2, which(is.na(tripdata_2$end_station_name)))
for(i in 1:length(index3))
{
  indexofcorrection3 <- with(station_name_lat_lng1, which(station_name_lat_lng1$lat == tripdata_2$end_lat[index3[i]] & station_name_lat_lng1$lng == tripdata_2$end_lng[index3[i]]))
  if (length(indexofcorrection3) > 0)
  {
    actualind3 <- indexofcorrection3[1]
    tripdata_2$end_station_name[index3[i]] <- station_name_lat_lng1$station_name[actualind3]
  }
}

length(which(is.na(tripdata_2$end_station_name))) # 129816 nulls

#filled nulls at end_station_name

index4 <- with(tripdata_2, which(is.na(tripdata_2$end_station_id)))
for(i in 1:length(index4))
{
  indexofcorrection4 <- with(station_id_lat_lng1, which(station_id_lat_lng1$lat == tripdata_2$end_lat[index4[i]] & station_id_lat_lng1$lng == tripdata_2$end_lng[index4[i]]))
  if (length(indexofcorrection4) > 0)
  {
    actualind4 <- indexofcorrection4[1]
    tripdata_2$end_station_id[index4[i]] <- station_id_lat_lng1$station_id[actualind4]
  }
}
length(which(is.na(tripdata_2$end_station_id))) #132336

#filled nulls at end_station_id

length(which(is.na(tripdata1$end_station_name))) # 143158 nulls before running the loop
length(which(is.na(tripdata1$end_station_id))) #143619 nulls before running the loop....i don't think filling the nulls made much of a difference.

tripdata_3 <- tripdata_2 %>% 
  mutate(rideduration = ended_at - started_at) %>% 
  mutate(lat_difference = start_lat - end_lat, lng_difference = start_lng - end_lng) %>% 
  mutate(north_south_dir = ifelse(lat_difference<0,"north",(ifelse(lat_difference==0,"no change",(ifelse((!is.na(lat_difference)),"south",NA)))))) %>% 
  mutate(east_west_dir = ifelse(lng_difference<0, "east",(ifelse(lng_difference==0, "no change", (ifelse((!is.na(lng_difference)),"west",NA)))))) %>% 
  mutate(dir = (ifelse((!is.na(north_south_dir)) & (!is.na(east_west_dir)), paste(north_south_dir,east_west_dir), NA))) %>%
  mutate(dir = str_replace(string= dir, pattern = "no change no change", replacement = "roundtrip")) %>% 
  mutate(dir = str_replace(string= dir, pattern = "no change east", replacement = "east")) %>% 
  mutate(dir = str_replace(string= dir, pattern = "no change west", replacement = "west")) %>% 
  mutate(dir = str_replace(string= dir, pattern = "north no change", replacement = "north")) %>% 
  mutate(dir = str_replace(string= dir, pattern = "south no change", replacement = "south")) %>% 
  mutate(route = (ifelse((!is.na(start_station_id)) & (!is.na(end_station_id)), as.numeric(paste0(start_station_id,end_station_id)), NA))) %>% 
  mutate(started_at_weekday = weekdays(started_at)) %>% 
  mutate(started_in_month = month(started_at))

View(tripdata_3)

n_distinct(tripdata_3$route) #143963 routes. Very large number so this is no good for analysis.

#here I inserted a duration column, then inserted a direction column by determining north vs south and east vs west directions.
#then i pasted them to make northeast, southwest etc. gave condition that if both are not nulls then paste or else just write null
#then i replaced to change the "no change" phrase into sensible information
#then i tried to find the routes, so i gave condition that if both start_station_id and end_station_id are there get a route or put na.
#then i found day of month and then the month
  

#ANALYSIS STARTS HERE

#now i'll make a table separately for casuals and then for members. tripdata_2 here will be treated as a master table.

casual <- tripdata_3 %>% 
  filter(member_casual == "casual")

member <- tripdata_3 %>% 
  filter(member_casual == "member")

#CASUAL analysis:

casual_analysis <- casual %>% 
  summarize(total_rides_casual = n_distinct(ride_id), mean_rideduration_casual = mean(rideduration), min_rideduration_casual = min(rideduration), max_rideduration_casual = max(rideduration))
View(casual_analysis)

casual_rides_various_hours <- casual %>% 
  count(hour(started_at)) %>% 
  rename(count=n) %>% 
  rename(hour=1)
View(casual_rides_various_hours)

casual_dayofweek_distribution <- casual %>% 
  group_by(started_at_weekday) %>% 
  count(started_at_weekday) %>% 
  rename(count=n) %>% 
  mutate(percent= round(c((count/1427134)*100), 2))
View(casual_dayofweek_distribution)

casual_monthly_rides <- casual %>% 
  group_by(started_in_month) %>% 
  count(started_in_month) %>% 
  rename(count=n) %>% 
  mutate(percent= round(c((count/1427134)*100), 2))
View(casual_monthly_rides)

casual_dir_distribution <- casual %>% 
  group_by(dir) %>% 
  count(dir) %>% 
  rename(count=n) %>% 
  mutate(percent= round(c((count/1427134)*100), 2))
View(casual_dir_distribution)

#MEMBER analysis:

member_analysis <- member %>% 
  summarize(total_rides_member = n_distinct(ride_id), mean_rideduration_member = mean(rideduration), min_rideduration_member = min(rideduration), max_rideduration_member = max(rideduration))
View(member_analysis)

member_rides_various_hours <- member %>% 
  count(hour(started_at)) %>% 
  rename(count=n) %>% 
  rename(hour=1)
View(member_rides_various_hours)

member_dayofweek_distribution <- member %>% 
  group_by(started_at_weekday) %>% 
  count(started_at_weekday) %>% 
  rename (count=n) %>% 
  mutate(percent= round(c((count/2052099)*100), 2))
View(member_dayofweek_distribution)

member_monthly_rides <- member %>% 
  group_by(started_in_month) %>% 
  count(started_in_month) %>% 
  rename (count=n) %>% 
  mutate(percent= round(c((count/2052099)*100), 2))
View(member_monthly_rides)

member_dir_distribution <- member %>% 
  group_by(dir) %>% 
  count(dir) %>% 
  rename (count=n) %>% 
  mutate(percent= round(c((count/2052099)*100), 2))
View(member_dir_distribution)

#making these tables below for direction distribution grouped bar chart

master_table <- rbind(member_dir_distribution,casual_dir_distribution)
type= c("member","member","member","member","member","member","member","member","member","member","casual","casual","casual","casual","casual","casual","casual","casual","casual","casual")
View(type)

master_table <- data.frame(type, master_table)

View(master_table)

#making these tables below for day of the week distribution grouped bar chart

master_table_weekday <- rbind(member_dayofweek_distribution,casual_dayofweek_distribution)
types= c("member","member","member","member","member","member","member","casual","casual","casual","casual","casual","casual","casual")
master_table_weekday <- data.frame(types,master_table_weekday)
View(master_table_weekday)


#VIZUALIZATION:

#1Member vs Casual: Rides at various hours of the day - Line chart

ggplot()+geom_line(member_rides_various_hours,mapping=aes(x=hour, y=count, color="member")) + geom_line(casual_rides_various_hours, mapping=aes(x=hour, y=count, color="casual"))+
  labs(title="Member vs Casual: Rides at various hours of the day", subtitle="Comparison between members vs casuals",caption="Data is acquired from two tables in the analysis part of r-script of the project")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) + scale_x_continuous(breaks = seq(0, 23, by = 1))

ggsave("D:\\Data Analytics Projects\\CAPSTONE project- Data Analytics Course - Courseera and Google\\MembervCasual.png")
ggsave("MemberVsCasual-Rides at various hours of the day-Line chart.png")

#INSIGHT: we find out that casual riders do not peak in morning, they only peak in evening however there is a small spike in morning for members.
#color component is kept inside aes or else the legend was not coming.
#scale_x_continuous things helps in getting all 24 labels for x-axis.




#2Member Vs Casual: Rides in various months - Line chart

ggplot()+geom_line(member_monthly_rides,mapping=aes(x=factor(month.abb[started_in_month],levels=month.abb),y=count/100000, group=1,color="member"))+geom_line(casual_monthly_rides,mapping=aes(x=factor(month.abb[started_in_month],levels=month.abb),y=count/100000, group=1,color="casual"))+
  labs(y="Count(in Lakhs)", x= "months", title="Member Vs Casual: Rides in various months", subtitle="Members vs Casual", caption="Data is acquired from two tables in the analysis part of rscript of the project")

ggsave("MemberVsCasual-Rides in various months-Line Chart.png")

#INSIGHT: Not much difference
#factor(month.abb[started_in_month],levels=month.abb is done so that we can get names of month instead of 1,2,3,.. and so that they are arranged sequentially.
#y=count/100000 , this is done so that the figures in y axis are readable
#group=1, so many transformation must have maddened R so it was not running as group size was only 1, so I had to specify it is cool boy, please run.
#then changed the label for x and y axis for comprehensibility




#3Member vs Casual: Rides in various months - stacked column chart

ggplot()+geom_col(member_monthly_rides,mapping=aes(x=factor(month.abb[started_in_month],levels=month.abb),y=count/100000),fill="purple")+geom_col(casual_monthly_rides,mapping=aes(x=factor(month.abb[started_in_month],levels=month.abb),y=count/100000),fill="grey")+
  labs(y="Count (in Lakhs)", x= "months", title="Member vs Casual: Rides in various months column chart", subtitle="Members vs Casual", caption="Data is acquired from two tables in the analysis part of rscript of the project")

ggsave("MembervsCasual-Rides in various months-stacked column chart.png")

#INSIGHT: Difference between no.of Member vs no.of Casual is very high for Jan, Dec. Members were much higher. Moderately high for Nov and Oct. Least for July.




#4Direction taken by Member- PIE chart

ggplot(member_dir_distribution,aes(x="", y=percent, fill=dir))+ geom_col(color="black")+coord_polar(theta="y")+geom_text(aes(label=percent),position = position_stack(vjust=0.5))+labs(title="Directions taken by Member", subtitle="Members direction preference", caption="Data taken from a table from analysis portion og r-script")

ggsave("Direction taken by Member-pie chart.png")

#INSIGHT: The pie chart looks dirty and hard to understand.
#INSIGHT: Northwest > Southeast> Southwest> Northeast > Roundtrip. The first two has very less difference. Also 3,4 has very less difference between them.
#this is kind of bar chart only, geom_col here decides border color




#5Direction taken by Casual - PIE Chart

ggplot(casual_dir_distribution,aes(x="", y=percent, fill=dir))+ geom_col(color="black")+coord_polar(theta="y")+geom_text(aes(label=percent),position = position_stack(vjust=0.5))+labs(title="Directions taken by Casual", subtitle="Casuals direction preference", caption="Data taken from a table from analysis portion og r-script")

ggsave("Direction taken by Casual-pie Chart.png")

#INSIGHT: The pie chart looks dirty and hard to understand. 
#INSIGHT: Northwest> Southeast > Northeast > Southwest > Roundtrip. The first two has very less difference. Also 3,4,5 has very less difference between them.
#INSIGHT: Casual member do take a lot of roundtrip it seems.




#6Member vs Casual: Direction taken - Grouped bar chart

ggplot(master_table,aes(x=dir,y=percent,fill=type))+geom_col(position="dodge")+labs(x="direction", y="percentage", title="Member vs Casual: Direction taken", subtitle="Group bar chart comparing the directions taken by casuals and members")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

ggsave("Member vs Casual-Direction taken-Grouped bar chart.png")

#INSIGHT: Casual take far more roundtrips than Member.
#used dodge here to make sure this is a group bar chart and not a stacked bar chart.




#7Day of week distribution individual Pie charts

ggplot(member_dayofweek_distribution,aes(x="", y=percent, fill=started_at_weekday))+ geom_col(color="black")+coord_polar(theta="y")+geom_text(aes(label=percent),position = position_stack(vjust=0.5))+labs(title="Members- Day of week distri", subtitle="Members starting rides on various days of week", caption="Data taken from a table from analysis portion og r-script")

ggsave("Members-Day of week distri-pie chart.png")

ggplot(casual_dayofweek_distribution,aes(x="",y=percent, fill=started_at_weekday))+ geom_col(color="black")+coord_polar(theta="y")+geom_text(aes(label=percent),position = position_stack(vjust=0.5))+labs(title="Casual- Day of week distri", subtitle="Casual starting rides on various days of week", caption="Data taken from a table from analysis portion og r-script")

ggsave("Casual-Day of week distri-pie chart.png")

#INSIGHT: We see here that casual people ride most during weekend, followed by friday. There rides weekdays vs weekends is huge. The same is not true for member
#members ride almost equally throughout the week with the least on sundays and most on saturdays, variance is minimum




#8Member vs Casual : Day of week distribution - Grouped bar chart

ggplot(master_table_weekday, aes(x=started_at_weekday, y=percent, fill=types))+geom_col(position= "dodge")+labs(x="weekday",y="percentage",title="Member vs Casual:Day of week distribution")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))

ggsave("MemberVsCasual-Day of week distribution-grouped bar chart.png")

#INSIGHT: Member use everyday equivalently. Casual use mostly on Saturday, Sunday, closely followed by Friday.




#9Member vs Casual: Mean rideduration - Bar chart

mean_rideduration_secs <- c(966.8946,2698.147)
member_type <- c('member','casual')
mean_rideduration_mem_cas <- data.frame(member_type,mean_rideduration_secs)
View(mean_rideduration_mem_cas)

ggplot()+geom_col(mean_rideduration_mem_cas,mapping=aes(x=member_type,y=mean_rideduration_secs,fill=member_type))+labs(title="Member Vs Casual: Mean rideduration")

ggsave("MemberVsCasual-Mean rideduration-bar chart.png")

#INSIGHT: The mean of Casual is much higher(more than double) that of Member.




#10Member vs Casual: Bike types usage - Facet bar graph

ggplot(tripdata_3)+geom_bar(mapping=aes(x=rideable_type,fill=member_casual))+ scale_y_continuous(labels=comma)+ theme(axis.text.x=element_text(angle=45,vjust=0.5))+ facet_wrap(~member_casual)+labs(title="Member vs Casual: Bike types usage")

ggsave("MembervsCasual-Bike types usage-bar chart.png")

#INSIGHT: No major difference found.




#11Member vs Casual: How the 3 types of bikes were used in different months

ggplot(tripdata_3)+geom_bar(mapping=aes(x=rideable_type,fill=rideable_type))+ scale_y_continuous(labels=comma)+ facet_grid(member_casual~started_in_month)+theme(axis.text.x=element_blank()) +labs(title="Member vs Casual: How the 3 types of bikes were used in different months")

ggsave("MemberVsCasual-3 types of bikes usage in different months-bar chart.png")

#INSIGHT: Found that in Jan members used much more classic bikes compared to casuals.
#INSIGHT: In March casual who are less in number used docked bikes but none of the members used it.
#INSIGHT: In Dec comparatively a large set of members used classic bikes.
#theme function is used to remove text from x axis, they were cluttering everything




#12Member Vs Casual: How the 3 types of bikes were used in different days of week

ggplot(tripdata_3)+geom_bar(mapping=aes(x=rideable_type,fill=rideable_type))+ scale_y_continuous(labels=comma)+ facet_grid(member_casual~started_at_weekday)+theme(axis.text.x=element_blank()) +labs(title="Member vs Casual: How the 3 types of bikes were used in different days of week")

ggsave("MemberVsCasual-3 types of bikes usage in different days of week-grouped bar chart.png")

#Insight: No significant revelation. ELectric bike consumption almost same for Member and Casual on Saturdays.



#13Member Vs Casual: How the 3 types of bikes were used in different directions

ggplot(tripdata_3)+geom_bar(mapping=aes(x=rideable_type,fill=rideable_type))+ scale_y_continuous(labels=comma)+ facet_grid(member_casual~dir)+theme(axis.text.x=element_blank()) +labs(title="Member vs Casual: How the 3 types of bikes were used in different directions")

#INSIGHT: None found, hence made no graph



#ROUGH SHEET


location_id <- select(tripdata1,c(station_name="start_station_name",station_id="start_station_id",lat="start_lat",lng="start_lng"))
View(location_id) #made this subtable where to get station name and ID for every combo of lat and lng
str(location_id)
location_id2 <- location_id %>% 
  na.omit(location_id) #got rid of all NA
View(location_id2)
location_id3 <- location_id2 %>% 
  distinct(station_name,station_id,lat,lng,.keep_all=TRUE) #got rid of all duplicate values
View(location_id3)






#THE BELOW STUFF IS LIKE ROUGH SHEET

  
#SAYAN

tripdata2 <- tripdata1;
indx <- with(tripdata2, which(is.na(tripdata2$start_station_id)));
for(i in 1:length(indx))
{
  indexofcorrecttable <- with(location_id3, which(location_id3$lat == tripdata2$start_lat[indx[i]] & location_id3$lng == tripdata2$start_lng[indx[i]]));
  if(length(indexofcorrecttable) > 0)
  {
    actualindex <- indexofcorrecttable[1];
    tripdata2$start_station_id[indx[i]] <- location_id3$station_id[actualindex];
  }
}

#this is called a loop.Indx is a variable with all nulls of tripdata2$start_station_id, now I am taking every value of indx and running
#this command on them. Indexofcorrectable is a vector with all station_id (drawn from location_id3) for the combo of lat and lng which had nulls in tripdata2,
#actualindex is taking the first value of each location_id
#the last line explains how the replacement happens

sum(is.na(tripdata1$start_station_id))
sum(is.na(tripdata2$start_station_id))


length(which(is.na(tripdata1$start_station_id)))
length(which(is.na(tripdata2$start_station_id)))

#finding total number of the respective things. I found 122754 and 112873 are answers. So only 10k nulls got filled.
#SAYAN
#TANIA SATURDAY 8TH OCT NIGHT

which(is.na(tripdata2$start_station_id))
tripdata2[685975,]
with(location_id3, which(lat==41.9 & lng==-87.6))

tripdata3 <- tripdata1;
indx2 <- with(tripdata3[629703:3479233,],which(is.na(tripdata3$start_station_id)));
for(i in 1:length(indx2))
{
  indexofcorrecttable2 <- with (location_id3, which(location_id3$lat == tripdata3$start_lat[indx[i]] & location_id3$lng == tripdata3$start_lng[indx[i]]));
  if(length(indexofcorrecttable2) > 0)
  {
    actualindex2 <- indexofcorrecttable2[1];
    tripdata3$start_station_id[indx[i]] <- location_id3$station_id[actualindex2];
  }
}
which(is.na(tripdata3$start_station_id))
length(which(is.na(tripdata3$start_station_id)))
#TANIA SATURDAY 8TH OCT NIGHT
