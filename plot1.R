#################################################################################
#
#  Filename: plot1.R
#   Version: 1.0.0
#      Date: 2015.09.20
#    Author: Mariusz Musiał
# Rev. Info: 1.0 - initial version of the script
# 
# This script generates a plot of total PM25 emissions in US in years 1999-2008.
# Base grapics system is used.
# Original data is available at: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# More information about data: http://www.epa.gov/ttn/chief/eiinformation.html
# 
#     Input: summarySCC_PM25.rds file located in the current working directory
#    Output: plot1.png written to the current working directory
# Execution: source(plot1.R)
# 
# Prerequisites: 1) this script and the data files are located in the current
#                   working directory
#                2) dplyr package is installed
# 
#################################################################################

if(!require("dplyr")) {
    stop("Please install dplyr package.")
}

summaryFile <- "summarySCC_PM25.rds"
pngFile <- "plot1.png"

pm <- readRDS("summarySCC_PM25.rds")

# Calculate total emissions levels for each year
pmtot <- pm %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

# Plot the emissions levels vs. year along with the linear regression line
png(pngFile)
plot(pmtot, ylab = "Emissions [tons]", xaxt = "n")
abline(lm(Emissions ~ year, pmtot),col = "red")
title(main = expression('U.S. Total PM'[2.5]*' Emissions in Years 1999-2008'))
axis(side = 1, at = pmtot$year,labels = T)
abline(h = seq(par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1), col="lightgray", lty="dotted")
abline(v = pmtot$year, col="lightgray", lty="dotted")
legend("topright", legend = c("Total Emissions", "Lin. Regr. Tot. Emiss."), col = c("black", "red"), pch = c(1,45))
dev.off()
