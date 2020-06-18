library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)

##download data
if(!(file.exists("Activity monitoring data")) {
    archiveFile <- "repdata_data_activity.zip"
    if(!file.exists(archiveFile)){
        archiveURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(url = archiveURL, destfile = archiveFile, method = "curl")
    }
unzip(archiveFile)

##load data into R
data.raw <- read.csv("activity.csv", header = T, sep = ",")

##clean up data
#move NA
data. <- na.omit(data.raw)
summary(data)

#formlate date
data$date <- ymd(data$date)
summary(data)
str(data)
head(data)
tail(data)

##Q1
#prepare data for histogram
data_Q1 <- summarise(group_by(data, date), daily.step = sum(steps))
mean <- as.integer(mean(data_Q1$daily.step))
median <- as.integer(median(data_Q1$daily.step))

#plot
plot1 <- ggplot(data_Q1, aes(x=daily.step)) + 
    geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
    geom_vline(xintercept=mean, colour="red", linetype="dashed", size=1) +
    geom_vline(xintercept=median, colour="green" , linetype="dotted", size=1) +
    labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
plot1


##time-series plot
#prepare data for plot
data_Q3 <- summarise(group_by(data, interval), mean.step = mean(steps))
#plot
plot3 <- ggplot(data_Q3, aes(x=interval,y=mean.step)) + 
    geom_line(color="red") + 
    labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot3

#find max
max.step <- which.max(data_Q3$mean.step)
max.interval <- data_Q3$interval[max.step]

##imputing missing values 
#number of NA
sum(is.na(data.raw))
data.impute <- data.raw
data.impute$steps[is.na(data.impute$steps)] <- mean(data.impute$steps, na.rm=T)
data.impute$steps <- as.numeric(data.impute$steps)
data.impute$interval <- as.numeric(data.impute$interval)
colSums(is.na(data.impute))
data.impute$date <- ymd(data.impute$date)
summary(data.impute)

#prepare data for plot
data.imputeQ1 <- summarise(group_by(data.impute, date), daily.steps = sum(steps))
mean.impute <- as.integer(mean(data.imputeQ1$daily.steps))
median.impute <- as.integer(median(data.imputeQ1$daily.steps))

#plot
plot.impute.Q1 <- ggplot(data.imputeQ1, aes(x=daily.steps)) + 
    geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
    geom_vline(xintercept=mean.impute, colour="red", linetype="dashed", size=1) +
    geom_vline(xintercept=median.impute, colour="green" , linetype="dotted", size=1) +
    labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
plot.impute.Q1



data.impute$day <- ifelse(weekdays(data.impute$date) %in% c("Saturday", "Sunday"), "weekday", "weekend")
# Preparing data for ggplot
impute.df <- data.impute %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))

# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
    facet_grid(day~.) +
    geom_line() + 
    labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval
