
#Data Visualization of days (Heat Map of each day in Month Juky and August)
dev.off()
rm(list = ls())
cat('\013')
setwd('C:/Users/avind/Desktop/Research Group/July2020')
#the necessary packages
#install.packages("lubridate") # used to create the time and date data of our missing time gaps
#install.packages("dplyr") # used to merge the missing time gaps data with the fitbit data
#install.packages("imputeTS") # used to fill in NA's in the HR column after we fill in the missing time gaps
#install.packages("changepoint")# detect change point
#install.packages("tsoutliers")#detect outliers
#install.packages("tidyverse")
#install.packages("forecast")#display and analyze univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modelling
#install.packages("Metrics")# for computing the RMSE
#install.packages("backports")
#install.packages("xcode")
#install.packages("tidyverse")

library(lubridate)
library(imputeTS)
#library(tidyverse)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("openair")
library(openair)
library(lubridate)
library(dplyr)
library(imputeTS)
#library(changepoint)
#library(tsoutliers)
#library(tidyverse)
library(ggplot2)
library(forecast)
library(Metrics)
library(imputeTS)
library (data.table)
library(TSstudio)
library(changepoint)

#__________________________Data Extraction and data preprocessing for jyly month________________________________________
csv_files <-   list.files (path       = "C:/Users/avind/Desktop/Research Group/July2020", 
                           pattern    = "*.csv", 
                           full.names = T)

DATA <-  as_tibble (rbindlist (lapply (csv_files, fread))) #Data frame for july month
DATA_ALL <- DATA
Heatmapsdata <- DATA

## Formatting time for the aggregation command further
DATA_ALL$Time <- as.POSIXct(DATA_ALL$Time, tz = "", "%Y-%m-%d %H:%M:%S")

## Aggregated the HR column for 1 minute intervals
DATA_ALL <- aggregate(DATA_ALL["HR"], list(time = cut(DATA_ALL$Time, "1 min")),mean)
## Created Date Column
DATA_ALL$date <- DATA_ALL$time

## Formatting of date and time Columns into Y-M-D and H:M:S format respectively.
DATA_ALL$time <- format(ymd_hms(DATA_ALL$time), "%H:%M:%S")
DATA_ALL$date <- strptime(DATA_ALL$date,format = "%Y-%m-%d") 
DATA_ALL$AHR <- DATA_ALL$HR

#___________________________Data Extraction and data preprocessing for August Month_____________________________________________________________________________________
# merge csvs ## Data loading
csv_files1 <-   list.files (path       = "C:/Users/avind/Desktop/Research Group/August_HR", 
                            pattern    = "*.csv", 
                            full.names = T)
DATA1 <-  as_tibble (rbindlist (lapply (csv_files1, fread))) #Data frame for August month
DATA_ALL_August <- DATA1
Heatmapsdata_August <- DATA1
#View(DATA1)


DATA_ALL_August$Time <- as.POSIXct(DATA_ALL_August$Time, tz = "", "%Y-%m-%d %H:%M:%S")

## Aggregated the HR column for 1 minute intervals
DATA_ALL_August <- aggregate(DATA_ALL_August["HR"], list(time = cut(DATA_ALL_August$Time, "1 min")),mean)
## Created Date Column
DATA_ALL_August$date <- DATA_ALL_August$time

## Formatting of date and time Columns into Y-M-D and H:M:S format respectively.
DATA_ALL_August$time <- format(ymd_hms(DATA_ALL_August$time), "%H:%M:%S")
DATA_ALL_August$date <- strptime(DATA_ALL_August$date,format = "%Y-%m-%d") 
DATA_ALL_August$AHR <- DATA_ALL_August$HR



#_______________Heat Map for july month dates__________________________________________________________

#The heat map represntation of every day in july month
Heatmapsdata$Time <- ymd_hms(Heatmapsdata$Time)#regulate data in POSIXct form
#add column named as date
Heatmapsdata$date <- as.POSIXct(Heatmapsdata$Time, tz = "", "%Y-%m-%d %H:%M:%S")
#use timeaverage function to calculate mean heart rate every minute
min <-  timeAverage(mydata = Heatmapsdata, avg.time = "min")
#heatmap display time series:https://www.r-graph-gallery.com/283-the-hourly-heatmap.html
#Creating a new data frame for heatmaping which contains seperate date, time min, hour columns
dft<-min %>%select(date,HR)
## 
dft <- dft %>% mutate(year = year(date),
                      month = month(date),
                      day = day(date),
                      hour = hour(date),
                      minute = minute(date)
)

for (i in 1:31) {
  df1<-dft[dft$day == i,]
  assign(paste("DATA_ALL_",i,sep = ""))}



dno<-unique(dft$day)
pday <-ggplot(dft,aes(hour,minute,fill=HR))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_continuous(na.value="red")+
  labs(title= paste("daily NAs",dft$day), x="Hour(h)", y="Minute(min)")
#generate daily plot through iteration
for (i in 1:31) {
  dfd<-dft[dft$day == i,]
  assign(paste("pday_",i,sep = ""),ggplot(dfd,aes(hour,minute,fill=HR))+
           geom_tile(color= "white",size=0.1) +
           scale_fill_continuous(na.value="red")+
           labs(title= paste("2020-7-",dfd$day), x="Hour(h)", y="Minute(min)")+
           theme(legend.position = "none"))
}
#the way seperate legend in from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
legend<- g_legend(pday)
#output weekly heatmap
grid.arrange(pday_1,pday_2,pday_3,pday_4,legend,ncol=2,top="Weekly NAs(1)")
grid.arrange(pday_5,pday_6,pday_7,pday_8,pday_9,pday_10,pday_11,legend,ncol=2,top="Weekly NAs(2)")
grid.arrange(pday_12,pday_13,pday_14,pday_15,pday_16,pday_17,pday_18,legend,ncol=2,top="Weekly NAs(3)")
grid.arrange(pday_19,pday_20,pday_21,pday_22,pday_23,pday_24,pday_25,legend,ncol=2,top="Weekly NAs(4)")
grid.arrange(pday_26,pday_27,pday_28,pday_29,pday_30,pday_31,legend,ncol=2,top="Weekly NAs(5)")

#________________Heat for month of August___________________________________________________________________

## Heat Mapping for cheacking the missing values
Heatmapsdata_August$Time <- ymd_hms(Heatmapsdata_August$Time)#regulate data in POSIXct form
#add column named as date
Heatmapsdata_August$date <- as.POSIXct(Heatmapsdata_August$Time, tz = "", "%Y-%m-%d %H:%M:%S")
#use timeaverage function to calculate mean heart rate every minute
min <-  timeAverage(mydata = Heatmapsdata_August, avg.time = "min")
#heatmap display time series:https://www.r-graph-gallery.com/283-the-hourly-heatmap.html
#Creating a new data frame for heatmaping which contains seperate date, time min, hour columns
dft1<-min %>%select(date,HR)
## 
dft1 <- dft1 %>% mutate(year = year(date),
                        month = month(date),
                        day = day(date),
                        hour = hour(date),
                        minute = minute(date)
)

dno<-unique(dft1$day)
pday1 <-ggplot(dft1,aes(hour,minute,fill=HR))+
  geom_tile(color= "white",size=0.1) +
  scale_fill_continuous(na.value="red")+
  labs(title= paste("daily NAs",dft1$day), x="Hour(h)", y="Minute(min)")
#generate daily plot through iteration
for (i in 1:31) {
  dfd1<-dft1[dft1$day == i,]
  assign(paste("pday1_",i,sep = ""),ggplot(dfd1,aes(hour,minute,fill=HR))+
           geom_tile(color= "white",size=0.1) +
           scale_fill_continuous(na.value="red")+
           labs(title= paste("2020-8-",dfd1$day), x="Hour(h)", y="Minute(min)")+
           theme(legend.position = "none"))
}
#the way seperate legend in from https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
g_legend1<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
legend<- g_legend1(pday1)
#output weekly heatmap
grid.arrange(pday1_1,pday1_2,pday1_3,pday1_4,legend,ncol=2,top="Weekly NAs(1)")
grid.arrange(pday1_5,pday1_6,pday1_7,pday1_8,pday1_9,pday1_10,pday1_11,legend,ncol=2,top="Weekly NAs(2)")
grid.arrange(pday1_12,pday1_13,pday1_14,pday1_15,pday1_16,pday1_17,pday1_18,legend,ncol=2,top="Weekly NAs(3)")
grid.arrange(pday1_19,pday1_20,pday1_21,pday1_22,pday1_23,pday1_24,pday1_25,legend,ncol=2,top="Weekly NAs(4)")
grid.arrange(pday1_26,pday1_27,pday1_28,pday1_29,pday1_30,pday1_31,legend,ncol=2,top="Weekly NAs(5)")


  
