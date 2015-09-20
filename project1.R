activity <- read.csv("activity.csv")

daySteps <- aggregate(steps ~ date, data=activity, sum)



hist(daySteps$steps, 
     breaks=10, 
     col="cornflowerblue", 
     xlab = "No. of Steps per Day", 
     main = "Histogram of Steps per Day")

mean(daySteps$steps)
median(daySteps$steps)


intervalSteps <- aggregate(steps ~ interval, data=activity, mean)
minute <- intervalSteps$interval%%100
hour <- as.integer(intervalSteps$interval/100)
new <- cbind(intervalSteps, hour, minute)
x <- paste(hour, minute, sep=":")
new2 <- cbind(new, x)
new2$y <- as.POSIXlt(new2$x, format = "%H:%M")
plot(new2$y, new2$steps,type="l", xlab = "", ylab="")
title(xlab = "Time of Day (HH:MM)",
      ylab = "Average Number of Steps", 
      col.lab = "red",
      main = "Steps as a function of Time of Day in 5 Min Intervals",
      col.main = "blue")

format(new2$y[which.max(new2$steps)], "%H:%M")

sum(is.na(activity))

cleanActivity <- transform(activity, 
                           steps = ifelse(is.na(activity$steps), 
                           new2$steps[match(activity$interval, 
                           new2$interval)], 
                           activity$steps))

daySteps <- aggregate(steps ~ date, data=cleanActivity, sum)

hist(daySteps$steps, 
     breaks=10, 
     col="cornflowerblue", 
     xlab = "No. of Steps per Day", 
     main = "Histogram of Steps per Day")

mean(daySteps$steps)
median(daySteps$steps)

cleanActivity$days <- weekdays(as.Date(cleanActivity$date))
cleanActivity$week <- "weekday"
index <- cleanActivity$days %in% c("Saturday", "Sunday")
cleanActivity$week[index] <- "weekend"

intervalSteps <- aggregate(steps ~ interval + week, data=cleanActivity, mean)
minute <- intervalSteps$interval%%100
hour <- as.integer(intervalSteps$interval/100)
new <- cbind(intervalSteps, hour, minute)
x <- paste(hour, minute, sep=":")
new2 <- cbind(new, x)
new2$y <- as.POSIXlt(new2$x, format = "%H:%M")



xyplot(new2$steps ~ new2$interval|new2$week, 
       xlab = "Interval",
       ylab = "Average Number of Steps", 
       main = "Steps as a function of Time of Day in 5 Min Intervals",
       type = "l")
