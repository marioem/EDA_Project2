#################################################################################
#
#  Filename: plot3.R
#   Version: 1.1.0
#      Date: 2015.09.21
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
#            1.1.0 - changed combined plot to 2x2 faceted plot
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

# Calculate emissions levels in Baltimore City for each year for each of 4 types
baltpmtype <- pm %>% filter(fips == BaltimoreFips) %>% group_by(type, year) %>% summarise(Emissions = sum(Emissions))

# Plot the emissions levels vs. year along with the linear regression line
#
# Design decision: As PM25 emissions can be cotrolled only "approximately"
# (due to high number of influencing factors) as a user of this grpah I'd be 
# more interested in the trend over years rather than in the exact shape of
# the emissions curve. Hence the yearly emisions are marked as points 
# and a trend line (linear regression) is added.
# In order to ease the analysis of each type of emission a plot with 4 facets
# with free y axis has been chosen. The answer to the question if the emission
# level for each type decreased or increased between 1999 and 2008 in Baltimore
# City can thus be easier spotted.

png(pngFile, width = 640, height = 640, units = "px")
ggpl <- ggplot(data = baltpmtype, aes(year, Emissions, color = type))
ggpl <- ggpl + theme(legend.position = "none")
ggpl <- ggpl + geom_point(size = 4)
ggpl <- ggpl + facet_wrap(~type, ncol = 2, scales = "free_y")
ggpl <- ggpl + geom_smooth(method = "lm", se = F)
ggpl <- ggpl + ylab("Emissions [tons]")
ggpl <- ggpl + ggtitle(expression('Baltimore City PM'[2.5]*' Emission per Source Type in Years 1999-2008'))
ggpl <- ggpl + scale_x_continuous(breaks = baltpmtype$year, labels = baltpmtype$year)

## Without faceting horizontal reference lines can be added on the height of first
## and last point for each emission type to ease a bit checking if the emission
## level between first and last analyzed year increased and decreased.
## For faceting plot it is not so easy to separate each pair of lines between each
## facet, so I'll leave it for another day.
## 
# library(scales)
# pal <- scales::hue_pal()(4)
# for(i in seq(0,3)) {
#     ggpl <- ggpl + annotate("segment", x=baltpmtype$year[1+4*i], xend=baltpmtype$year[4+4*i],y=baltpmtype$Emissions[1+4*i], yend=baltpmtype$Emissions[1+4*i], linetype = "dashed", color = pal[i+1])
#     ggpl <- ggpl + annotate("segment", x=baltpmtype$year[1+4*i], xend=baltpmtype$year[4+4*i],y=baltpmtype$Emissions[4+4*i], yend=baltpmtype$Emissions[4+4*i], linetype = "dashed", color = pal[i+1])
# }

ggpl
dev.off()
