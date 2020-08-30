Loading and preprocessing the data
----------------------------------

    if(!exists("data_activity ")){
      data_activity <- read.csv("./activity.csv")
    }

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    data_activity$date <- ymd(data_activity$date)

What is mean total number of steps taken per day?
-------------------------------------------------

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    total_steps <- data_activity %>% group_by(date) %>%summarise(tot_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) %>% print

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 61 x 3
    ##    date       tot_steps    na
    ##    <date>         <int> <dbl>
    ##  1 2012-10-01         0     1
    ##  2 2012-10-02       126     0
    ##  3 2012-10-03     11352     0
    ##  4 2012-10-04     12116     0
    ##  5 2012-10-05     13294     0
    ##  6 2012-10-06     15420     0
    ##  7 2012-10-07     11015     0
    ##  8 2012-10-08         0     1
    ##  9 2012-10-09     12811     0
    ## 10 2012-10-10      9900     0
    ## # ... with 51 more rows

    hist(total_steps$tot_steps,main="Total number of steps per day",xlab="Total steps",ylim = c(0,25), breaks = seq(0,25000, by=2500))

    abline(v=mean(total_steps$tot_steps), lty=2,lwd=3, col="gray20")
    abline(v=median(total_steps$tot_steps), lty=2,lwd=3, col="indianred4")
    text(7500,22,'mean',col="gray20")
    text(12500,22,'median',col="indianred4")

![Plot 1](/figure/plot_pr1.png)

    paste("Mean steps per day =", mean(total_steps$tot_steps), na.rm=TRUE)

    ## [1] "Mean steps per day = 9354.22950819672 TRUE"

    paste("Median steps per day =", median(total_steps$tot_steps), na.rm=TRUE)

    ## [1] "Median steps per day = 10395 TRUE"

    dev.copy(png,file="plot_pr1.png",width=480, height=480)

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

What is the average daily activity pattern?
-------------------------------------------

    activity_pattern <- data_activity %>% group_by(interval) %>%summarise(mean_steps=mean(steps,na.rm=TRUE)) %>% print

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 288 x 2
    ##    interval mean_steps
    ##       <int>      <dbl>
    ##  1        0     1.72  
    ##  2        5     0.340 
    ##  3       10     0.132 
    ##  4       15     0.151 
    ##  5       20     0.0755
    ##  6       25     2.09  
    ##  7       30     0.528 
    ##  8       35     0.868 
    ##  9       40     0     
    ## 10       45     1.47  
    ## # ... with 278 more rows

    plot(activity_pattern$interval,activity_pattern$mean_steps,
         type="l",
         main = "Average Daily Activity Pattern",
         xlab="Interval",
         ylab = "Average number of steps")

![Plot 2](/figure/plot_pr2.png)

    dev.copy(png,file="plot_pr2.png",width=480, height=480)

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

    max_steps <- filter(activity_pattern,mean_steps==max(mean_steps))
    paste("Interval with max steps =", max_steps$interval)

    ## [1] "Interval with max steps = 835"

Imputing missing values
-----------------------

    na_number <- sum(is.na(data_activity$steps))
    paste("Total number of NA =", na_number)

    ## [1] "Total number of NA = 2304"

    imputed_data <- data_activity
    for (i in 1:length(imputed_data$steps)) {
      if (is.na(imputed_data$steps[i])) {
        imputed_data$steps[i] <- activity_pattern$mean_steps[activity_pattern$interval == imputed_data$interval[i]]
      }
    }

    imput_steps <- imputed_data %>% group_by(date) %>% summarise(steps=sum(steps,na.rm=TRUE)) %>% print

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 61 x 2
    ##    date        steps
    ##    <date>      <dbl>
    ##  1 2012-10-01 10766.
    ##  2 2012-10-02   126 
    ##  3 2012-10-03 11352 
    ##  4 2012-10-04 12116 
    ##  5 2012-10-05 13294 
    ##  6 2012-10-06 15420 
    ##  7 2012-10-07 11015 
    ##  8 2012-10-08 10766.
    ##  9 2012-10-09 12811 
    ## 10 2012-10-10  9900 
    ## # ... with 51 more rows

    na_number2 <- sum(is.na(imput_steps$steps))
    na_number2

    ## [1] 0

    hist(imput_steps$steps,main="Total number of steps per day",xlab="Total steps",ylim = c(0,25), breaks = seq(0,25000, by=2500))

    abline(v=mean(imput_steps$steps), lty=2,lwd=3, col="gray20")
    abline(v=median(imput_steps$steps), lty=2,lwd=3, col="indianred4")
    text(6500,22,'mean & median',col="indianred4")

![Plot 3](/figure/plot_pr3.png)

    dev.copy(png,file="plot_pr3.png",width=480, height=480)

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2

    paste("Mean steps per day =", mean(imput_steps$steps), na.rm=TRUE)

    ## [1] "Mean steps per day = 10766.1886792453 TRUE"

    paste("Median steps per day =", median(imput_steps$steps), na.rm=TRUE)

    ## [1] "Median steps per day = 10766.1886792453 TRUE"

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

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

![Plot 4](/figure/plot_pr4.png)

    dev.copy(png,file="plot_pr4.png",width=480, height=480)

    ## png 
    ##   3

    dev.off()

    ## png 
    ##   2
