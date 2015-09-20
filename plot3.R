#################################################################################
#
#  Filename: plot3.R
#   Version: 1.0.0
#      Date: 2015.09.20
#    Author: Mariusz Musiał
# Rev. Info: 1.0 - initial version of the script
# 
# This script generates a plot of PM25 emissions in Baltimore City in years
# 1999-2008 split between source types.
# ggplot2 grapics system is used.
# Original data is available at: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# More information about data: http://www.epa.gov/ttn/chief/eiinformation.html
# 
#     Input: summarySCC_PM25.rds file located in the current working directory
#    Output: plot3.png written to the current working directory
# Execution: source(plot3.R)
# 
# Prerequisites: 1) this script and the data files are located in the current
#                   working directory
#                2) dplyr package is installed
#                3) ggplot2 package is installed
# 
#################################################################################

if(!require("dplyr")) {
    stop("Please install dplyr package.")
}

if(!require("ggplot2")) {
    stop("Please install ggplot3 package.")
}

summaryFile <- "summarySCC_PM25.rds"
pngFile <- "plot3.png"

BaltimoreFips <- "24510"

pm <- readRDS(summaryFile)

# Calculate total emissions levels in Baltimore City for each year
baltpmtype <- pm %>% filter(fips == BaltimoreFips) %>% group_by(type, year) %>% summarise(Emissions = sum(Emissions))

# Plot the emissions levels vs. year along with the linear regression line
png(pngFile, width = 640, units = "px")
ggpl <- ggplot(data = baltpmtype, aes(year, Emissions, color = type))
ggpl <- ggpl + geom_point(size = 4)
ggpl <- ggpl + geom_smooth(method = "lm", se = F)
ggpl <- ggpl + ylab("Emissions [tons]")
ggpl <- ggpl + ggtitle(expression('Baltimore City PM'[2.5]*' Emission per Source Type in Years 1999-2008'))
ggpl <- ggpl + scale_x_continuous(breaks = baltpmtype$year, labels = baltpmtype$year)
ggpl
dev.off()
