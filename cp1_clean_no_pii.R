# Coursera - 4 - Reproducible Research
# Peer Assessment 1
# Submitter:  A. Nichols
# 19 July 2015

library(dplyr); library(lubridate); library(tidyr); library(lattice) 
library(ggplot2)

data1df <- read.table(
	"C:/Users/.../activity.csv",
	header=TRUE,
	sep = ",",
	col.names = c("steps", "date", "interval"),
	colClasses=c("numeric", "character", "numeric"), 
	na.strings=c("NA")
)
data1df$date <- strptime(data1df$date, format="%Y-%m-%d")
data1df$date <- ymd(data1df$date)	# Use lubridate to fix date format
data1tbl <- tbl_df(data1df) 		# Create tbl out of initial data frame
data2tbl <- mutate(data1tbl, 
	new_steps=steps,
	day_num=wday(date), 
	day=weekdays(date))
data2df <- data.frame(data2tbl)
data3df <- cbind(data2df, "weekday"=0)  # Identify weekdays & weekend days
	for (i in 1:17568) {
		if ((data3df$day_num[i] >= 2)&(data3df$day_num[i] <= 6)){
			data3df$weekday[i] <- 1
		}
	}
	for (i in 1:17568) {
		if (is.na(data3df$steps[i])){
			data3df$new_steps[i] <- 0
		}
	}
data3tbl <- tbl_df(data3df)
data3tbl <- select(data3tbl, date, interval, day, day_num, weekday,
	steps, new_steps)
data4df <- data.frame(data3tbl)  	# Baseline data sets
data4tbl <- data3tbl

# Histogram 1 (NA's Omitted)##################################
data5tbl <- select(data4tbl, date, steps)
data5tbl <- filter(data5tbl, !is.na(steps))
data5df <- data.frame(data5tbl)
data5df$date <- as.character(data5df$date)
by_date_sums <- aggregate(steps ~ date, data=data5df, FUN=sum)

hist(by_date_sums$steps, 
	col="Red",
	bg="White",
	main="Steps per Day (NA's Omitted)",
	xlab="Total Steps per Day",
	ylab="Frequency of Occurence Over 53 Days",
	ylim=c(0, 30)
)
dev.copy(png, file="H1.png",width=480, height=480, bg="White")
dev.off()

# Time Series 1 (Average Steps per Day ################################
data10tbl <- select(data4tbl, interval, steps)
data10tbl <- filter(data10tbl, !is.na(steps))
data10df <- data.frame(data10tbl)
by_interval <- aggregate(steps ~ interval, data=data10df, FUN=mean)

with(by_interval, plot(interval, steps, type="n",
	main="Average Steps per 5-Minute Interval",
	sub="(Data at 5-minute intervals with NA values omitted)", 
	xlab="Hour of Day",
	ylab="Steps"
	)
)
with(by_interval, points(interval, steps,
		type="l",
		lwd="1.9", 
		col="Blue",
		axis(side=1, at = c(0, 2400))
	)
)
text1 <- c("Steps")
legend("topright", 
	legend=text1,
	col=c("Blue"),
	lty=1,
	cex=0.7
)
dev.copy(png, file="TS1.png",width=480, height=480, bg="White")
dev.off()

# Time Series 2 (Median Steps per Day) ###############################
data20tbl <- select(data4tbl, interval, steps)
data20tbl <- filter(data20tbl, !is.na(steps))
data20df <- data.frame(data20tbl)
by_interval2 <- aggregate(steps ~ interval, data=data20df, FUN=median)

with(by_interval2, plot(interval, steps, type="n",
	main="Median Steps per 5-Minute Interval",
	sub="(Data at 5-minute intervals with NA values omitted)", 
	xlab="Hour of Day",
	ylab="Steps"
	)
)
with(by_interval2, points(interval, steps,
		type="l",
		lwd="1.9", 
		col="Red",
		axis(side=1, at = c(0, 2400))
	)
)
text1 <- c("Steps")
legend("topright", 
	legend=text1,
	col=c("Red"),
	lty=1,
	cex=0.7
)
dev.copy(png, file="TS2.png",width=480, height=480, bg="White")
dev.off()

# Weekday Average #################################################
data30tbl <- select(data4tbl, interval, steps, weekday)
data30tbl <- filter(data30tbl, weekday==1, !is.na(steps))
data30df <- data.frame(data30tbl)
by_interval <- aggregate(steps ~ interval, data=data30df, FUN=mean)

with(by_interval, plot(interval, steps, type="n",
	main="Weekday Average Steps per 5-Minute Interval",
	sub="(Data at 5-minute intervals with NA values omitted)", 
	xlab="Hour of Day",
	ylab="Steps"
	)
)
with(by_interval, points(interval, steps,
		type="l",
		lwd="1.9", 
		col="Purple",
		axis(side=1, at = c(0, 2400))
	)
)
text1 <- c("Steps")
legend("topright", 
	legend=text1,
	col=c("Purple"),
	lty=1,
	cex=0.7
)
dev.copy(png, file="WD1.png",width=480, height=480, bg="White")
dev.off()

# Weekend Daily Avg ##############################################
data40tbl <- select(data4tbl, interval, steps, weekday)
data40tbl <- filter(data40tbl, weekday==0, !is.na(steps))
data40df <- data.frame(data40tbl)
by_interval3 <- aggregate(steps ~ interval, data=data40df, FUN=mean)

with(by_interval3, plot(interval, steps, type="n",
	main="Weekend Daily Average Steps per 5-Minute Interval",
	sub="(Data at 5-minute intervals with NA values omitted)", 
	xlab="Hour of Day",
	ylab="Steps"
	)
)
with(by_interval3, points(interval, steps,
		type="l",
		lwd="1.9", 
		col="Brown",
		axis(side=1, at = c(0, 2400))
	)
)
text1 <- c("Steps")
legend("topright", 
	legend=text1,
	col=c("Brown"),
	lty=1,
	cex=0.7
)
dev.copy(png, file="WE1.png",width=480, height=480, bg="White")
dev.off()



