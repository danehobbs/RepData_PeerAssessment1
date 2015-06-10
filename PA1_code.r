wd <- dirname(parent.frame(2)$ofile)
setwd(wd)
data <- read.csv("activity.csv")
library(sqldf)
dailysteps <- sqldf("select sum(steps) as steps, date
      from data
      group by date
      ")
hist(dailysteps$steps)
mean(dailysteps$steps, na.rm=TRUE)

median(dailysteps$steps, na.rm=TRUE)

avgintervalsteps <-sqldf("select avg(steps) as steps, interval
      from data
      group by interval
      ")
data$missing <- is.na(data$steps)
imputeddata <- sqldf("select data.steps,
                     data.date,
                     data.interval,
                     data.missing,
                     avgintervalsteps.steps as avgsteps
                     from data
                     left join
                        avgintervalsteps on data.interval = avgintervalsteps.interval")
for (i in 1:length(imputeddata$missing)) {
        if (imputeddata$missing[i] == TRUE) {imputeddata$newsteps[i] <- imputeddata$avgsteps[i]}
        else
        {imputeddata$newsteps[i] <- imputeddata$steps[i]}
                
}
dailysteps2 <- sqldf("select sum(newsteps) as steps, date
      from imputeddata
      group by date
      ")
hist(dailysteps2$steps)
mean(dailysteps2$steps, na.rm=TRUE)

median(dailysteps2$steps, na.rm=TRUE)

for (i in 1:length(imputeddata$date)) {
        if ((weekdays(as.POSIXct(imputeddata$date[i])) == "Sunday") 
          | (weekdays(as.POSIXct(imputeddata$date[i])) == "Saturday"))
        {imputeddata$weekday[i] <- "Weekend"}
        else
        {imputeddata$weekday[i] <- "Weekday"}
        
}
imputeddata$weekday<- as.factor(imputeddata$weekday)
avgintervalsteps2 <-sqldf("select avg(newsteps) as steps, interval, weekday
      from imputeddata
      group by interval, weekday
      ")
library(lattice)
xyplot(avgintervalsteps2$steps ~ avgintervalsteps2$interval | avgintervalsteps2$weekday,
       layout = c(1,2), type = "l")

