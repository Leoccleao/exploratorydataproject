#Reading RDS files
sourcecode <- readRDS("Source_Classification_Code.rds")
summarySCC <- readRDS("summarySCC_PM25.rds")

library(dplyr)

#Getting Baltimore City (fips == "24510") data
baltimore <- summarySCC %>% filter(fips == "24510")

#total PM2.5 emission from all sources by each of the years
yeartotal <- with(baltimore,tapply(Emissions,year,FUN = sum))
yeartotal <- data.frame(Year = row.names(yeartotal),Emissions.Tons = yeartotal,row.names = NULL)

# Creating file PLOT 2
png(filename = "plot2.png", width = 480, height = 480) # starting PNG device with proper dims
par(bg = "transparent") # background colour

#Plotting total PM2.5 emission from all sources by each of the years
with(yeartotal,plot(Year,Emissions.Tons, ylab = expression(PM[2.5] *" Emissions (Tons)"), xlab = "Year",xlim = c(1998,2008), ylim = c(1500,3500), col = Year,pch = 20, cex = 2))
title(main = expression("Baltimore City - Total "* PM[2.5]))

#Plotting total PM2.5 emission trend
yeartotal$Year <- as.numeric(yeartotal$Year)
model <- lm(Emissions.Tons ~ Year,yeartotal)
abline(model)


dev.off()