#Reading RDS files
sourcecode <- readRDS("Source_Classification_Code.rds")
summarySCC <- readRDS("summarySCC_PM25.rds")

library(dplyr)
library(ggplot2)


#Finding COAL sources and US observations 
coal <- filter(sourcecode, grepl("Coal",EI.Sector))

uscoalobs <- filter(summarySCC,SCC %in% coal$SCC)


#total PM2.5 emission from all sources by each of the years
yeartotal <- with(uscoalobs,tapply(Emissions,year,FUN = sum))
yeartotal <- data.frame(Year = row.names(yeartotal),Emissions.Tons = yeartotal,row.names = NULL)

# Creating file PLOT 4
png(filename = "plot4.png", width = 480, height = 480) # starting PNG device with proper dims
par(bg = "transparent") # background colour

#Plotting total PM2.5 emission from selected sources by each of the years
with(yeartotal,plot(Year,Emissions.Tons, ylab = expression(PM[2.5] *" Emissions (Tons)"), xlab = "Year",xlim = c(1998,2008), ylim = c(300000,600000), col = Year,pch = 20, cex = 2))
title(main = expression("Coal combustion-related sources - US Emissions"))

#Plotting total PM2.5 emission trend
yeartotal$Year <- as.numeric(yeartotal$Year)
model <- lm(Emissions.Tons ~ Year,yeartotal)
abline(model)


dev.off()
