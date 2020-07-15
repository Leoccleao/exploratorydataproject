#Reading RDS files
sourcecode <- readRDS("Source_Classification_Code.rds")
summarySCC <- readRDS("summarySCC_PM25.rds")

library(dplyr)
library(ggplot2)


#Finding ON Road sources and Baltimore and LA observations
automotive <- summarySCC %>% 
              filter(type == "ON-ROAD" & fips == "24510" | fips == "06037") %>%
              group_by(fips,year)%>%
              summarize(TotalEmissions = sum(Emissions))


#Creating PLot with:  total PM2.5 emission from selected fips and source by each of the years

g <- ggplot(automotive, aes(year,TotalEmissions)) + 
    facet_grid(fips~., scales = "free", labeller = labeller(fips = cityname)) + 
    geom_line(aes(color = fips), lwd = 2) + 
    geom_smooth(method = "lm", se = FALSE, lwd = 0.5, color = "black", linetype = 2) +
    theme(legend.position = "none")+labs(title = expression(PM[2.5] *" Motor vehicle emission"), y = expression(PM[2.5] *" Emissions (Tons)"), x = "Year")

# Creating file PLOT 6
png(filename = "plot6.png", width = 480, height = 480) # starting PNG device with proper dims

print(g)

dev.off()