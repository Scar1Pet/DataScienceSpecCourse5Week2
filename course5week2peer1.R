# Task 1. Code for reading in the dataset and/or processing the data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile="activity.zip")
unzip("activity.zip")
data <- read.csv("activity.csv")
require(lubridate)
data$date <- ymd(data$date)

# Task 2. Histogram of the total number of steps taken each day
total.steps.by.day <- tapply(data$steps, 
    data$date, 
    sum, 
    na.rm=T)
hist(total.steps.by.day, 
    main="Total Steps By Day", 
    xlab="Steps")

# Task 3. Mean and median number of steps taken each day
mean.steps.day <- tapply(data$steps, 
    data$date, 
    mean, 
    na.rm=T)
median.steps.day <- tapply(data$steps, 
    data$date, 
    median, 
    na.rm=T)
my.result <- data.frame(mean.steps = mean.steps.day, 
    median.steps = median.steps.day)
rownames(my.result) <- rownames(mean.steps.day)
my.result


# Task 4. Time series plot of the average number of steps taken
mean.steps.by.day <- aggregate(data$steps, 
    by=list(data$date), 
    FUN=mean, 
    na.rm=T)
mean.steps.by.day <- mean.steps.by.day[complete.cases(mean.steps.by.day),]
names(mean.steps.by.day) <- c("date", "mean.steps")
with(mean.steps.by.day, 
    plot(date, 
    mean.steps, 
    type="l", 
    xlab="Date",
    ylab="Steps", 
    main="Average Steps per Day"))
points(mean.steps.by.day$date, 
    mean.steps.by.day$mean.steps, 
    pch=16, cex=.5)
                         
# Task 5. The 5-minute interval that, on average, 
#    contains the maximum number of steps
steps.by.interval <- aggregate(data$steps, 
    by=list(data$interval), 
    FUN=mean, 
    na.rm=T)
names(steps.by.interval) <- c("interval","sum.steps")
max.steps <- max(steps.by.interval$sum.steps)
max.interval <- steps.by.interval[steps.by.interval$sum.steps==max.steps,1]
max.interval

# Task 6. Code to describe and show a strategy for imputing missing data
fraction.missing <- mean(is.na(data$steps))
fraction.missing

data$week <- factor(week(data$date))
avg.steps.week <- tapply(data$steps, data$week, mean, na.rm=T)
data$imputed.steps <- {
    imputed.steps <- data$steps
    for(i in seq_along(data$steps)){
        step = data$steps[i]
        if(is.na(step)){
            week <-  data$week[i]
            avg.steps <- avg.steps.week[as.character(week)]
            imputed.steps[i] <- avg.steps
        }
        else(imputed.steps[i] <- step)
    }
    imputed.steps
}

# Task 7. Histogram of the total number of steps 
#    taken each day after missing values are imputed
total.steps.by.day <- tapply(data$imputed.steps, 
    data$date, 
    sum, 
    na.rm=T)
hist(total.steps.by.day, 
    main="Total Steps By Day (Imputed Values)", 
    xlab="Steps")

# Task 8. Panel plot comparing the average number of steps 
#    taken per 5-minute interval across weekdays and weekends

data$wkd <- weekdays(data$date)
data$day.type <- {
    day.type <- as.character()
    for(i in seq_along(data$wkd)){
        weekday <- data$wkd[i]
        if(weekday %in% c("Saturday", "Sunday")){
            day.type[i] <- "Weekend"
        }
        else{
            day.type[i] <- "Weekday"
        }
    }
    day.type <- factor(day.type)
}

steps.time <- aggregate(data$steps,  
    by=list(data$interval,data$day.type),
    FUN=mean,
    na.rm=T)
names(steps.time) <- c("Interval", "Day.Type", "Avg.Steps")
       
require(ggplot2)
g <- ggplot(steps.time, aes(Interval, Avg.Steps))
g <- g + geom_line()
g <- g + facet_grid(.~Day.Type)
g <- g + labs(x="Interval", y="Average Steps")
g <- g + ggtitle("Average Steps per Interval", subtitle="Weekend Vs Weekday")
g

# Task 9. All of the R code needed to reproduce the results 
#    (numbers, plots, etc.) in the report

