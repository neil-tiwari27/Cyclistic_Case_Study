#Install and load relevant packages
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("chron")

#Load relevant packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(chron)
library(tibble)

#Load data frame
trip_data <-read.csv("combined-csv-files.csv") #Load dataset
trip_data1 <- trip_data %>%
  select(ride_id, rideable_type, Weekday_start, total_time, member_casual)
rm(trip_data)

#Set order for x axis on graphs
level_order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

#Weekly data
##Singular graph showing number of rides per day
ggplot(data=trip_data1) + 
  geom_bar(mapping=aes(x=factor(Weekday_start,levels=level_order), fill=member_casual)) + 
  labs(title='Daily Ridership', x='', y='Count', fill = 'Rider Type') +
  scale_fill_hue(labels = c("Casual", "Member"))

##Two graphs showing number of rides per day for each type of rider
ggplot(data=trip_data1) + 
  geom_bar(mapping=aes(x=factor(Weekday_start,levels=level_order), fill=member_casual)) + 
  labs(title='Daily Number of Rides', x='', y='Count', fill='Rider Type') + 
  facet_wrap(~member_casual) + 
  theme(axis.text.x = element_text(angle=45)) +
  scale_fill_hue(labels = c("Casual", "Member"))

#Comparing mean ridership time for members Vs casual riders
trip_data1$total_time <- hms(trip_data1$total_time) #Convert to HMS format
trip_data1$total_time <- as.period(trip_data1$total_time) #Convert to period type
trip_data1$number_of_seconds <- period_to_seconds(trip_data1$total_time) #Convert period to seconds and assign to column

#Calculations for mean ridership per day
mean_minutes<- trip_data1 %>%
  group_by(member_casual) %>%
  group_by(Weekday_start, .add=TRUE) %>%
  drop_na() %>%
  summarise(mean(number_of_seconds/60))
colnames(mean_minutes)[3] <- "avg_mintues"

#Graph for mean casual and member ridership per day
ggplot(data=mean_minutes) + 
  geom_col(mapping = aes(x=factor(Weekday_start,level_order), y=avg_mintues, fill=member_casual)) + 
  facet_wrap(~member_casual) +
  labs(title='Average Minutes Per Ride', x='', y='Average # of Minutes', fill='Rider Type') + 
  theme(axis.text.x = element_text(angle=45)) +
  scale_fill_hue(labels = c("Casual", "Member"))
