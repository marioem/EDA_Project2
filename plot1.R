#################################################################################
#
#  Filename: plot1.R
#   Version: 1.0.0
#      Date: 2015.09.20
#    Author: Mariusz Musiał
# Rev. Info: 1.0 - initial version of the script
# 
# This script generates a plot of total PM25 emissions in US in years 1999-2008.
# Base graphics system is used.
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

# Calculate total emissions levels for each year.
# Some of the SCC codes bear notion of "Total" or "All" (those ending with "000").
# This suggests that it can be some kind of subsum. A random check of a certain
# series didn't confirm this, so for the purpose of this assignment I assume it
# is OK to sum over SCCs without excluding suspected subsums.
pmtot <- pm %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

# Plot the emissions levels vs. year along with the linear regression line
#
# Design decision: As PM25 emissions can be controlled only "approximately"
# (due to high number of influencing factors) as a user of this grpah I'd be 
# more interested in the trend over years rather than in the exact shape of
# the emissions curve. Hence the yearly emisions are marked as points 
# and a trend line (linear regression) is added. This kind of plot allows to
# answer the question if the total emissions decreased between 1999 and 2008
# (compare extreme point values) and provides additional information if
# the overall trend is also decreasing.

png(pngFile)
plot(pmtot, ylab = "Emissions [tons]", xaxt = "n")
abline(lm(Emissions ~ year, pmtot), col = "red")
title(main = expression('U.S. Total PM'[2.5]*' Emissions in Years 1999-2008'))
axis(side = 1, at = pmtot$year,labels = T)
abline(h = seq(par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1), col="lightgray", lty="dotted")
abline(v = pmtot$year, col="lightgray", lty="dotted")
legend("topright", legend = c("Total Emissions", "Lin. Regr. Tot. Emiss."), col = c("black", "red"), pch = c(1,45))
dev.off()
