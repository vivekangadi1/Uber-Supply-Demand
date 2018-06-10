

#install.packages("lubridate")

library(lubridate)
library(dplyr)
library(ggplot2)

uber_masterdata <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)
#Extract the first two characters of Request time column 
#Save it as a seperate column of the dataframe
#Converting the date to one format
uber_masterdata$Request.timestamp <- parse_date_time(x = uber_masterdata$Request.timestamp,
                                                         orders = c("d/m/Y H:M", "d-B-Y H:M", "d/m/Y H:M:S", "d-B-Y H:M:S"),
                                                         locale = "eng")
uber_masterdata$Drop.timestamp <- parse_date_time(x = uber_masterdata$Drop.timestamp,
                                                     orders = c("d/m/Y H:M", "d-B-Y H:M", "d/m/Y H:M:S", "d-B-Y H:M:S"),
                                                  locale = "eng")

sum(is.na(uber_masterdata$Driver.id))
#Extractint the request hour
uber_masterdata$Request.hour <- format(uber_masterdata$Request.timestamp,"%H")
typeof(uber_masterdata$Request.hour)
#convert the separated charcters to data type numeric
uber_masterdata$Request.hour <- as.numeric(uber_masterdata$Request.hour)
#Extracting Week days
uber_masterdata$weekDay <- weekdays(uber_masterdata$Request.timestamp)
hourwise_request_count <- ggplot(uber_masterdata,aes(x=factor(Request.hour),fill=factor(Pickup.point)))
#Add title and lables to the plot and save it as a object
plot1 <- hourwise_request_count+geom_bar(stat='count',position = "dodge")+
  ggtitle("Hourly Demand for Uber Cabs")+
  labs(x="Time in Hours", y="Number of Cabs Requested")+
  labs(fill="Pickup Point")
#hourwiseplot for cabs
#hourwiseplot for pickup point airport with fill of status
hourewieseplotForAirport <- ggplot(uber_masterdata %>% filter(Pickup.point == "Airport"),
                                   aes(x=factor(Request.hour),fill = factor(Status)))
plot11 <- hourewieseplotForAirport + geom_bar(stat = "count",position = "dodge")+
          ggtitle("Hourly Demand From the Airport")+
          labs(x="Time in Hours", y="Number of Cabs Requested")+
          labs(fill="Status")
plot11
#hourwiseplot for pickup point city with fill of status
hourewieseplotForcity <- ggplot(uber_masterdata %>% filter(Pickup.point == "City"),
                                   aes(x=factor(Request.hour),fill = factor(Status)))
plot12 <- hourewieseplotForcity + geom_bar(stat = "count",position = "dodge")+ 
          ggtitle("Hourly Demand From the city")+
          labs(x="Time in Hours", y="Number of Cabs Requested")+
          labs(fill="Status")
plot12
#weekwisePlot for pickup point airport with fill of status
#view the plot
weekwisePlot <- ggplot(uber_masterdata,aes(x=factor(weekDay),fill=factor(Status)))
plot13 <- weekwisePlot + geom_bar(stat = "count",position = "dodge")
plot13
# Generate a sequence of numbers from 0 to 23 
# save it as a vector
Request.hour <- c(0:23)
# create a vector of names of time slots
Time_Slot1 <- c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night")
# create a vector which represents the number of times time slots are to be repeated
times <- c(4,6,7,5,2)
# Repeat the time slots number of times required and save it as a vector
# The number of elements in this vector should correspond to 24
Time_Slot <- rep(Time_Slot1,times)
# create a new dataframe with sequence of number generated and time slots
new_frame <- data.frame(Time_Slot,Request.hour)
#Merge the main uber request dataframe with the new dataframe created
uber_masterdata <- merge(uber_masterdata,new_frame,by="Request.hour",all.x=TRUE)
#Change the sequence of columns of dataframe
uber_masterdata <- uber_masterdata[,c(2,3,4,5,6,7,8,1,9)]
slot.wise.city <- ggplot(uber_masterdata %>% filter(Pickup.point == "City"),
                                aes(x=factor(Time_Slot),fill = factor(Status)))
plot21 <- slot.wise.city + geom_bar(stat = "count",position = "dodge")+ 
  ggtitle("Slot Demand From the city")+
  labs(x="Slot", y="Number of Cabs Requested")+
  labs(fill="Status")+scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                                               "Late_Night","Pre_Morning"))
plot21
slot.wise.Airport <- ggplot(uber_masterdata %>% filter(Pickup.point == "Airport"),
                         aes(x=factor(Time_Slot),fill = factor(Status)))
plot22 <- slot.wise.Airport + geom_bar(stat = "count",position = "dodge")+ 
  ggtitle("Slot Demand From the Airport")+
  labs(x="Slot", y="Number of Cabs Requested")+
  labs(fill="Status")+scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                                                "Late_Night","Pre_Morning"))
plot22
slot.wise <- ggplot(uber_masterdata,
                         aes(x=factor(Time_Slot),fill = factor(Status)))
plot23 <- slot.wise + geom_bar(stat = "count")+ 
  ggtitle("Total Slot Demand")+
  labs(x="Slot", y="Number of Cabs Requested")+
  labs(fill="Status")+scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                                                "Late_Night","Pre_Morning"))

plot23
slot.wise1 <- ggplot(uber_masterdata,
                    aes(x=factor(Status),fill = factor(Pickup.point)))
plot24 <- slot.wise1 + geom_bar(stat = "count",alpha =0.9)+
  ggtitle("Status Based on City and Airport")+
  labs(x="Slot", y="Number of Cabs Requested")+
  labs(fill="PickUP Point")
plot24
a <- uber_masterdata %>% filter(Pickup.point == "Airport") %>% filter(Status == "No Cars Available")
b <- uber_masterdata %>% filter(Pickup.point == "City") %>% filter(Status == "Cancelled")
c <- uber_masterdata %>% filter(Pickup.point == "Airport") %>% filter(Time_Slot == "Morning_Rush")
d<- uber_masterdata %>% filter(Pickup.point == "Airport") %>% filter(Time_Slot == "Morning_Rush") %>% filter(Status == "Trip Completed")
e<- uber_masterdata %>% filter(Pickup.point == "Airport") %>% filter(Time_Slot == "Evening_Rush")
nrow(a)
nrow(b)
nrow(c)
nrow(d)
nrow(e)