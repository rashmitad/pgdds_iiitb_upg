library(stringr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

## Read the Uber Taxi Request Data File - remove row names
uber_masterdata <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE,row.names = NULL)
 
## Function to Standardize timestamp data - correct formatting issues and then convert the string to POSICct that is interconvertible with POSIXlt 
standardize_timestamp <- function ( datestr ) {
  
  res<-datestr
  
  if ( !is.na(as.POSIXct(datestr, format="%d/%m/%Y %I:%M:%S %p",tz="Asia/Calcutta"))) 
    { 
      res<-as.POSIXct(datestr, format="%d/%m/%Y %I:%M:%S %p",tz="Asia/Calcutta")
      return(res)
    } 
  else if (!is.na(as.POSIXct(datestr, format="%d/%m/%Y %H:%M:%S", tz="Asia/Calcutta")))
    {
      res<-as.POSIXct(datestr, format="%d/%m/%Y %H:%M:%S", tz="Asia/Calcutta")
      return(res)
    }
  else
    {
      if(!is.na(as.POSIXct(datestr, format="%d/%m/%Y %H:%M",tz="Asia/Calcutta")))
        res<-as.POSIXct(datestr, format="%d/%m/%Y %H:%M", tz="Asia/Calcutta")
    }
  return(res)
}

## Standardize timestamp data replace - to / in both of the timstamp columns
uber_masterdata$Request.timestamp<-str_replace_all(uber_masterdata$Request.timestamp,"-","/")
uber_masterdata$Drop.timestamp<-str_replace_all(uber_masterdata$Drop.timestamp,"-","/")

## Use sapply on the request and drop timestamp columns - standardize using user defined function standardize_timestamp
uber_req_time_formatted<-sapply(uber_masterdata$Request.timestamp,standardize_timestamp)
uber_drop_time_formatted<-sapply(uber_masterdata$Drop.timestamp,standardize_timestamp)

## Add type derived metrics data - such as day of the month, hour of request/drop
uber_masterdata$req_ts_mday<-as.POSIXlt.POSIXct(uber_req_time_formatted,tz="Asia/Calcutta")$mday 
uber_masterdata$drop_ts_mday<-as.POSIXlt.POSIXct(uber_drop_time_formatted,tz="Asia/Calcutta")$mday 

uber_masterdata$req_ts_hour<-as.POSIXlt.POSIXct(uber_req_time_formatted,tz="Asia/Calcutta")$hour 
uber_masterdata$drop_ts_hour<-as.POSIXlt.POSIXct(uber_drop_time_formatted,tz="Asia/Calcutta")$hour

## Add derived interval data - based on the hour the request was made timeslots are assigned
timeslots <- c("small_hours","early_morning","morning", "afternoon", "evening","late_evening")

## Convert continuous data to categorical data - use hour of request/drop and map to an interval i.e timeslot
uber_masterdata$uber_req_ts<- cut(uber_masterdata$req_ts_hour,
                                breaks=c(0,4,8,12,17,20,23),
                                include.lowest = TRUE, right = TRUE,
                                labels=timeslots)

uber_masterdata$uber_drop_ts<- cut(uber_masterdata$drop_ts_hour,
                                  breaks=c(0,4,8,12,17,20,23),
                                  include.lowest = TRUE, right = TRUE,
                                  labels=timeslots)
								  
#supply_demand
supply_demand_gap <- length(which((uber_masterdata$Status=="Cancelled")|(uber_masterdata$Status=="No Cars Available")))
supply_demand_met <- length(which(uber_masterdata$Status=="Trip Completed"))								  
								  
								  
##Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'

#Plot to show counts of cab orders categorised into cancelled,no_car_available,completed cabs over 24 hours
req_freq_plot_0<-ggplot(uber_masterdata, aes(req_ts_hour,fill=factor(Status)))+ 
                 geom_histogram(stat = "count",position="dodge")+
                 scale_x_continuous(name="hour",breaks = seq(0, 23, 1),limits=c(0, 23))+
                 theme_wsj() + scale_colour_wsj("colors6", "")+
                 theme(axis.title=element_text(size=12))
req_freq_plot_0

#Plot to show summary counts of cab orders categories - cancelled,no_car_available,completed
req_freq_plot <-  ggplot(uber_masterdata, aes(Status,fill=factor(Status)))+ylab("count")+geom_histogram(stat = "count")

req_freq_plot_1<- req_freq_plot+geom_text(stat='count',aes(label=..count..),vjust=-1)+
                  ggtitle("Uber Taxi Request Frequency") + 
                  theme_wsj() + scale_colour_wsj("colors6", "")+
                  theme(axis.title=element_text(size=12))
req_freq_plot_1

## identify the most problematic types of requests (city to airport / airport to city etc.)
req_freq_plot_2<-ggplot(uber_masterdata, aes(Status,fill=factor(Pickup.point))) + ylab("count") +
                 geom_histogram(stat="count",lwd=1) + 
                 #geom_text(stat='count',aes(label=..count..),vjust=-0.1,position = position_dodge(width=0.5))+ 
                 ggtitle("Problematic Requests w.r.t Pickup Points") +
                 theme_wsj() + scale_colour_wsj("colors6", "") +
                 theme(axis.title=element_text(size=12))
req_freq_plot_2

## identify the most problematic types of requests  and the time slots (early mornings, late evenings etc.) 
req_freq_plot_3 <- ggplot(uber_masterdata, aes(Status,fill=factor(Pickup.point))) +  ylab("count") +
                   geom_histogram(stat="count",lwd=1) + 
                   facet_wrap(~uber_masterdata$uber_req_ts)+
                   #geom_text(stat='count',aes(label=..count..),position = position_dodge(width=0.5),  size=3)+ 
                   ggtitle("Uber Taxi Request at Top Pick Points in Different Timeslots") +
                   theme_wsj() + scale_colour_wsj("colors6", "") +
                   theme(axis.title=element_text(size=12))

req_freq_plot_3 

##Find out the gap between supply and demand and show the same using plots.
### Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

req_freq_plot_4_data <- uber_masterdata %>%
  filter(Status %in% c("Cancelled","No Cars Available"))

### Find the time slots when the highest gap exists
req_freq_plot_4 <- ggplot(req_freq_plot_4_data, aes(Status,fill=factor(Pickup.point))) + ylab("count") +
  geom_histogram(stat="count",position="dodge") + 
  facet_wrap(~req_freq_plot_4_data$uber_req_ts)+
  geom_text(stat='count',aes(label=..count..),vjust=-0.01,position = position_dodge(width=0.5),  size=3)+ 
  ggtitle("Uber Taxi Supply Demand Gap in Different Timeslots") +
  theme_wsj() + scale_colour_wsj("colors6", "")+
  theme(axis.title=element_text(size=12))

req_freq_plot_4 



 