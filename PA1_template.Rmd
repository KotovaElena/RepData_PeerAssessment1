---
title: "Reproducible Research Course Project 1"
author: "Elena Kotova"
date: "27 08 2020"
output: html_document
---

## Loading and preprocessing the data

```{r, echo=TRUE }
if(!exists("data_activity ")){
  data_activity <- read.csv("./activity.csv")
}

library(lubridate)
data_activity$date <- ymd(data_activity$date)

```

## What is mean total number of steps taken per day?

```{r, echo = TRUE}
library(dplyr)
total_steps <- data_activity %>% group_by(date) %>%summarise(tot_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) %>% print
hist(total_steps$tot_steps,main="Total number of steps per day",xlab="Total steps",ylim = c(0,25), breaks = seq(0,25000, by=2500))

abline(v=mean(total_steps$tot_steps), lty=2,lwd=3, col="gray20")
abline(v=median(total_steps$tot_steps), lty=2,lwd=3, col="indianred4")
text(7500,22,'mean',col="gray20")
text(12500,22,'median',col="indianred4")

paste("Mean steps per day =", mean(total_steps$tot_steps), na.rm=TRUE)
paste("Median steps per day =", median(total_steps$tot_steps), na.rm=TRUE)

dev.copy(png,file="plot_pr1.png",width=480, height=480)
dev.off()
```

## What is the average daily activity pattern?

```{r, echo = TRUE}
activity_pattern <- data_activity %>% group_by(interval) %>%summarise(mean_steps=mean(steps,na.rm=TRUE)) %>% print
plot(activity_pattern$interval,activity_pattern$mean_steps,
     type="l",
     main = "Average Daily Activity Pattern",
     xlab="Interval",
     ylab = "Average number of steps")

dev.copy(png,file="plot_pr2.png",width=480, height=480)
dev.off()

max_steps <- filter(activity_pattern,mean_steps==max(mean_steps))
paste("Interval with max steps =", max_steps$interval)
```

## Imputing missing values

```{r, echo = TRUE}
na_number <- sum(is.na(data_activity$steps))
paste("Total number of NA =", na_number)

imputed_data <- data_activity
for (i in 1:length(imputed_data$steps)) {
  if (is.na(imputed_data$steps[i])) {
    imputed_data$steps[i] <- activity_pattern$mean_steps[activity_pattern$interval == imputed_data$interval[i]]
  }
}

imput_steps <- imputed_data %>% group_by(date) %>% summarise(steps=sum(steps,na.rm=TRUE)) %>% print
na_number2 <- sum(is.na(imput_steps$steps))
na_number2


hist(imput_steps$steps,main="Total number of steps per day",xlab="Total steps",ylim = c(0,25), breaks = seq(0,25000, by=2500))

abline(v=mean(imput_steps$steps), lty=2,lwd=3, col="gray20")
abline(v=median(imput_steps$steps), lty=2,lwd=3, col="indianred4")
text(6500,22,'mean & median',col="indianred4")

dev.copy(png,file="plot_pr3.png",width=480, height=480)
dev.off()

paste("Mean steps per day =", mean(imput_steps$steps), na.rm=TRUE)
paste("Median steps per day =", median(imput_steps$steps), na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?

```{r, echo = TRUE}
library(ggplot2)
imputed_data$weekday <- weekdays(imputed_data$date)
imputed_data$daytype <- ifelse(imputed_data$weekday=='суббота' | imputed_data$weekday=='воскресенье', 'weekend','weekday')

steps_daytype <- aggregate(steps~interval+daytype,data=imputed_data,FUN=mean,na.action=na.omit)
steps_daytype$time <- activity_pattern$interval

g <- ggplot(steps_daytype, aes(time, steps))
g+geom_line(col="darkred")+
  ggtitle("Average steps per time interval: weekdays vs. weekends")+
  xlab("Time")+ylab("Steps")+
  theme(plot.title = element_text(face="bold", size=12))+
  facet_grid(daytype ~ .)

dev.copy(png,file="plot_pr4.png",width=480, height=480)
dev.off()
```


