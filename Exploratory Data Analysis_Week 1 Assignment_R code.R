library(ggplot2)
library(plyr)
dir()


##Read the Data in R

PowerConsumption <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
## Format date to required format
PowerConsumption$Date <- as.Date(PowerConsumption$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
PowerConsumption <- subset(PowerConsumption,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
PowerConsumption <- PowerConsumption[complete.cases(PowerConsumption),]

## Combine Date and Time column
dateTime <- paste(PowerConsumption$Date, PowerConsumption$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
PowerConsumption <- PowerConsumption[ ,!(names(PowerConsumption) %in% c("Date","Time"))]

## Add DateTime column
PowerConsumption <- cbind(dateTime, PowerConsumption)

## Format dateTime Column
PowerConsumption$dateTime <- as.POSIXct(dateTime)  

##Creating the Histogram
hist(PowerConsumption$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="blue")
## Creating the second plot
plot(PowerConsumption$Global_active_power~PowerConsumption$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

##Creating the third plot
with(PowerConsumption, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
##Creating the fourth plot
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(PowerConsumption, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
