## Loading and preprocessing the data
activity <- read.csv("activity/activity.csv")
library(dplyr)
activity <- mutate(activity, date = as.Date(date))



## What is mean total number of steps taken per day?
days <- group_by(activity, date) %>% summarize(total = sum(steps))
hist(days$total)
mean(days$total, na.rm=T)
median(days$total, na.rm=T)

## What is the average daily activity pattern?
intervals <- group_by(activity, interval) %>% summarize(avg = mean(steps, na.rm=T))
with(intervals, plot(interval, avg, type="l"))
max.interval <- intervals[which.max(intervals$avg),]$interval


## Imputing missing values
activity %>% filter(is.na(steps)) %>% summarize(n())

activity2 <- left_join(activity, intervals, by="interval") %>% 
  mutate(steps = ifelse(is.na(steps),avg, steps)) %>%
  select(-avg)
days2 <- group_by(activity2, date) %>% summarize(total = sum(steps))
hist(days2$total)
mean(days2$total, na.rm=T)
median(days2$total, na.rm=T)

## Are there differences in activity patterns between weekdays and weekends?
library(chron)
intervals2 <- activity2 %>% 
  mutate(daytype = factor(ifelse(is.weekend(date), "weekend", "weekday"))) %>%
  group_by(interval, daytype) %>%
  summarize(avg = mean(steps))
library(ggplot2)
ggplot(intervals2, aes(interval, avg)) + geom_line() + facet_grid(daytype ~ .)
