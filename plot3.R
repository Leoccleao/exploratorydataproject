#Reading RDS files
sourcecode <- readRDS("Source_Classification_Code.rds")
summarySCC <- readRDS("summarySCC_PM25.rds")

library(dplyr)
library(ggplot2)

#Getting Baltimore City (fips == "24510") data for each type and year
baltimore <- summarySCC %>% filter(fips == "24510") %>% group_by(type,year)%>%summarize(TotalEmissions = sum(Emissions))



# Creating plot with ggplot2

g <- ggplot(baltimore, aes(year,TotalEmissions)) + geom_line(aes(color=type))+ labs(title = expression("Baltimore City - Total "* PM[2.5]*"by Source Type"), x ="Year", y = expression(PM[2.5] *" Emissions (Tons)"), color = "Source Type")

# Creating file PLOT 3
png(filename = "plot3.png") # starting PNG device


print(g)

dev.off()
