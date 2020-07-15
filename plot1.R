#Reading RDS files
sourcecode <- readRDS("Source_Classification_Code.rds")
summarySCC <- readRDS("summarySCC_PM25.rds")

#total PM2.5 emission from all sources by each of the years
yeartotal <- with(summarySCC,tapply(Emissions,year,FUN = sum))
yeartotal <- data.frame(Year = row.names(yeartotal),Emissions.MTons = yeartotal/1000000,row.names = NULL)

# Creating file PLOT 1
png(filename = "plot1.png", width = 480, height = 480) # starting PNG device with proper dims
par(bg = "transparent") # background colour

#Plotting total PM2.5 emission from all sources by each of the years
with(yeartotal,plot(Year,Emissions.MTons, ylab = expression(PM[2.5] *" Emissions (Million Tons)"), xlab = "Year",xlim = c(1998,2008), ylim = c(0,10), col = Year,pch = 20, cex = 2))
title(main = expression("Total "* PM[2.5]))

#Plotting total PM2.5 emission trend
yeartotal$Year <- as.numeric(yeartotal$Year)
model <- lm(Emissions.MTons ~ Year,yeartotal)
abline(model)


dev.off()


