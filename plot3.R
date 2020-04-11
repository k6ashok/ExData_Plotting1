plot3 <- function(){

	## downloading data from source, unzipping it and reading to R object org_data
	activity_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
	temp <- tempfile()
	download.file(activity_url, temp, method = "libcurl", mode = "wb")
	unzip(temp, "household_power_consumption.txt")
	org_data <- read.delim("household_power_consumption.txt",header=TRUE,sep=";")#download full data
	unlink(temp)
	fulldata <- org_data # creates a copy to work on
	
	install.packages("magrittr")
	library(magrittr)
	install.packages("dplyr")
	library(dplyr)
	install.packages("lubridate")
	library(lubridate)


## converts all factors into characters. converts Date 
## to Date format. All others to numeric except Time
	fulldata$Date <- as.character(fulldata$Date)
	fulldata$Date <- as.Date(fulldata$Date,format = "%d/%m/%Y")
	fulldata$Time <- as.character(fulldata$Time)
	fulldata$Global_active_power <- as.numeric(as.character(fulldata$Global_active_power))
	fulldata$Global_reactive_power <- as.numeric(as.character(fulldata$Global_reactive_power))
	fulldata$Voltage <- as.numeric(as.character(fulldata$Voltage))
	fulldata$Global_intensity <- as.numeric(as.character(fulldata$Global_intensity))
	fulldata$Sub_metering_1 <- as.numeric(as.character(fulldata$Sub_metering_1))
	fulldata$Sub_metering_2 <- as.numeric(as.character(fulldata$Sub_metering_2))

## Data is subsetted for creating plots
	mydata <- fulldata %>% filter(Date == "2007-02-01" |  Date == "2007-02-02")
	mydata <- mydata %>% mutate("DateTime" = as.POSIXct(paste(Date, Time),format="%Y-%m-%d %H:%M:%S"))
	str(mydata) # mydata is checked to contain 24*60*2 = 2880 obs of 9 variables for 2 days



##create plot and store as png file
 
	ylist <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
	plot(Global_intensity ~ DateTime,data = mydata, type = "n",ylab = "Energy sub metering",xlab = "")
	lines(mydata$DateTime,mydata$Sub_metering_1,col ="blue")
	lines(mydata$DateTime,mydata$Sub_metering_2,col ="red")
	lines(mydata$DateTime,mydata$Sub_metering_3,col ="green")	
	legend("topright", lty = 1, col = c("blue","red","green" ), legend = ylist)
	dev.copy(png,"Plot3.png")
	dev.off()
}
