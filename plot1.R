library(lubridate)
GettingSubset<-function()
{
  # reading
  t<-read.table("household_power_consumption.txt", header=TRUE, sep=";", nrows=2075260, colClasses = c("character", "character", "character","character","character","character","character","character","character"))
  
  # replacing ? by NA
  t[t$Global_active_power=="?",]<-c("NA")
  t[t$Global_reactive_power=="?",]<-c("NA")
  t[t$Voltage=="?",]<-c("NA")
  t[t$Global_intensity=="?",]<-c("NA")
  t[t$Sub_metering_1=="?",]<-c("NA")
  t[t$Sub_metering_2=="?",]<-c("NA")
  t[t$Sub_metering_3=="?",]<-c("NA")
  
  t1<-filter(t, Date=="1/2/2007" | Date=="2/2/2007")
  
  # setting numeric
  t1$Global_active_power<-as.numeric(t1$Global_active_power)
  t1$Global_reactive_power<-as.numeric(t1$Global_reactive_power)
  t1$Voltage<-as.numeric(t1$Voltage)
  t1$Global_intensity<-as.numeric(t1$Global_intensity)
  t1$Sub_metering_1<-as.numeric(t1$Sub_metering_1)
  t1$Sub_metering_2<-as.numeric(t1$Sub_metering_2)
  t1$Sub_metering_3<-as.numeric(t1$Sub_metering_3)
  t1$DateTime<-paste(t1$Date, t1$Time)
  t1$DateTime<-dmy_hms(t1$DateTime)
  t1
}
##getting subset data: 1/2/2007 to 2/2/2007
t1<-GettingSubset()
# setting png, keeping width and height by default
png(filename="plot1.png")
par(mfrow=c(1,1))

hist(as.numeric(t1$Global_active_power), xlab="Global Active Power (kilowatts)", ylab="Frequency", col="red", main="Global Active Power")
dev.off()