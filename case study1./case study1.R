getwd()
feb4<-read.csv("202101-divvy-tripdata.csv")
View(feb4)
dec8<-read.csv("202211-divvy-tripdata.csv")
jan6<-read.csv("202112-divvy-tripdata.csv")
mar2<-read.csv("202112-divvy-tripdata.csv")
apr6<-read.csv("202203-divvy-tripdata.csv")
jan3<-read.csv("202212-divvy-tripdata.csv")
library(tidyverse)
install.packages("janitor")
install.packages("ggmap")
## checking data sets for consistency
colnames(apr6)
colnames(dec8)
colnames(feb4)
colnames(jan3)
colnames(jan6)
colnames(mar2)
## to check data structures (dbl, chr, date)
str(apr6)
str(dec8)
str(feb4)
str(jan3)
str(jan6)
## checking merged data frame
# List for column names 
colnames(tripdata)
#See the first 6 rows of data frame.also tail(tripdata)
head(tripdata)
#See list of colnames and data types (numeric, character.ect)
str(tripdata)
# Statistical summry of data ,Mainly for numeric
summary(tripdata)
## Adding date, month, year, day of week columns
tripdata <- tripdata %>% 
  mutate(year = format(as.Date(started_at), "%Y")) %>%
  # extract year
  mutate(month = format(as.Date(started_at), "%B")) %>% 
  #extract month
  mutate(date = format(as.Date(started_at), "%d")) %>%
  # extract date
  mutate(day_of_week = format(as.Date(started_at), "%A")) %>% 
  # extract day of week
  mutate(ride_length = difftime(ended_at, started_at)) %>% 
  mutate(start_time = strftime(started_at, "%H"))

# converting 'ride_length' to numeric for calculation on data

tripdata <- tripdata %>% 
  mutate(ride_length = as.numeric(ride_length))
is.numeric(tripdata$ride_length) # to check it is right format
                    
# to check it is right format
is.numeric(tripdata$ride_length)
install.packages("janitor")
install.packages("geosphere")
install.packages("lubridate")
install.packages("tidyverse")
library(tidyverse)
library(janitor)
library(ggmap)
library(geosphere)
library(lubridate)
# adding ride distance in km
tripdata$ride_distance <- distGeo(matrix(c(tripdata$start_lng, tripdata$start_lat), ncol = 2), matrix(c(tripdata$end_lng, tripdata$end_lat), ncol = 2))

tripdata$ride_distance <- tripdata$ride_distance/1000 #distance in km
# Romve "bad" data
# the data frame includes a few hundred entries when bikes were taken out docks
# and checked for quality by Divvy where ride_lenght was negtive or'zero'
tripdata_clean <- tripdata[!(tripdata$ride_length <=0),]
# first lets check the cleand data frame 
str(tripdata_clean)
# lets check summarised details about the cleande dataset
summary(tripdata_clean)
# Conduct descriptive analysis
# descriptive analysis on 'ride_length'
# mean = straight average (total ride lenght / total rides)
# median = midpoint number of ride lenght array
# max = longest ride
# min = shortest ride
tripdata_clean %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))
# members vs casual riders difference depending on total rides taken
tripdata_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id), ride_percentage = (length(ride_id) / nrow(tripdata_clean)) * 100)
ggplot(tripdata_clean, aes(x = member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casuals vs Members", y="Number Of Rides", title= "Casuals vs Members distribution")
# lets fix the days of rhe week order
tripdata_clean$day_of_week <- ordered(tripdata_clean$day_of_week,levels=c("sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"  ))
tripdata_clean %>%
  # group by member casual
  group_by(member_casual,day_of_week) %>%
  # calculates the number if rides and average duration 
  summarise(number_of_rides = n()
  # calculates the average duration 
  , average_ride_length = mean(ride_length),.groups = "drop")%>%
  arrange(member_casual, day_of_week)
tripdata_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
tripdata_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length),.groups = "drop")%>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill= member_casual))+
  geom_col(width = 0.5,position = position_dodge(width = 0.5))+
  labs(title = "Average ride time by Members and Casual riders Vs. Day of the week")
  # First lets fic the month order 
tripdata_clean$month <- ordered(tripdata_clean$month,levels=c("January ","February","March","April ","May","June","July","August ","September ","October ","November ","December "))
tripdata_clean %>% 
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length), .groups="drop") %>% 
  arrange(member_casual, month)
tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Month", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length),.groups="drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride length by Members and Casual riders Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))
tripdata_clean %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by Members and Casual riders", x="Member and Casual riders", y="Average distance In Km")
tripdata_clean%>%
  ggplot(aes(start_time,fill=member_casual))+
  labs(x=" Houer of the day",title = "Cyclistics Bike demend by houer in a day")+
  geom_bar()
tripdata_clean %>%
  ggplot(aes(start_time, fill=member_casual)) +
  geom_bar() +
  labs(x="Hour of the day", title="Cyclistic's bike demand per hour by day of the week") +
  facet_wrap(~ day_of_week)
tripdata_clean%<%
  group_by(rideable_type)%>%
  summarise(count=length(ride_id))
ggplot(tripdata_clean,aes(x=rideable_type,fill=member_casual))+
  labs(x="Rideable type",title = "Rideable type Vs. Total rides by Members and casual riders")+geom_bar()
#Lets check the coordinates data of the rides.
#adding a new data frame only for the most popular routes >200 rides
coordinates_df <- tripdata_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total_rides = n(),.groups="drop") %>%
  filter(total_rides > 200)

# now lets create two different data frames depending on rider type (member_casual)

casual_riders <- coordinates_df %>% filter(member_casual == "casual")
member_riders <- coordinates_df %>% filter(member_casual == "member")
chicago <- c(left = -87.700424, bottom = 41.790769, right = -87.554855, top = 41.990119)

chicago_map <- get_stamenmap(bbox = chicago, zoom = 12, maptype = "terrain")
# maps on casual riders
ggmap(chicago_map,darken = c(0.1, "white")) +
  geom_point(casual_riders, mapping = aes(x = start_lng, y = start_lat, color=rideable_type), size = 2) +
  coord_fixed(0.8) +
  labs(title = "Most used routes by Casual riders",x=NULL,y=NULL) +
  theme(legend.position="none")

#map on member riders
ggmap(chicago_map,darken = c(0.1, "white")) +
  geom_point(member_riders, mapping = aes(x = start_lng, y = start_lat, color=rideable_type), size = 2) +  
  coord_fixed(0.8) +
  labs(title = "Most used routes by Member riders",x=NULL,y=NULL) +
  theme(legend.position="none")

install.packages("rmarkdown")
rmarkdown::render("case study1.R")
