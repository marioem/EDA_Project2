#################################################################################
#
#  Filename: plot4.R
#   Version: 1.0.0
#      Date: 2015.09.21
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
# 
# This script generates a plot of PM25 emissions in US related to coal combustion
# related sources, in years 1999-2008.
# ggplot2 grapics system is used.
# Original data is available at: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# More information about data: http://www.epa.gov/ttn/chief/eiinformation.html
# 
#     Input: summarySCC_PM25.rds, Source_Classification_Code.rds files located
#            in the current working directory
#    Output: plot4.png written to the current working directory
# Execution: source(plot4.R)
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
sccFile <- "Source_Classification_Code.rds"
pngFile <- "plot4.png"

pm <- readRDS(summaryFile)
scc <- readRDS(sccFile)

# Coal combustion related data are identified based on SCC codes for which variable
# EI.Sector contains both word "Coal" (case sensitive) and particle "comb" (case
# insensitive). Two logical vectors are generated, for each pattern in separation.
# Then theis conjunction is used to subset scc dataframe.
scoal <- grepl("Coal", scc$EI.Sector)
scomb <- grepl("comb", scc$EI.Sector, ignore.case = T)

scc$SCC <- as.character(scc$SCC) # so that SCC var in both scc and pm data frame are of character type
subscc <- scc[scoal & scomb,]
subpm <- semi_join(pm, subscc, by = "SCC")

# Calculate total coal combustion-related emissions per measurement year
# Some of the SCC codes bear notion of "Total" or "All" (those ending with "000").
# This suggests that it can be some kind of subsum. A random check of a certain
# series didn't confirm this, so for the purpose of this assignment I assume it
# is OK to sum over SCCs without excluding suspected subsums.
coalcomb <- subpm %>% group_by(year) %>% summarise(Emissions = sum(Emissions))


# Plot the emissions levels vs. year along with the linear regression line
#
# Design decision: In order to support the answer to the question *how* emission
# changed a line graph option was chosen.

png(pngFile, width = 640)
ggpl <- ggplot(data = coalcomb, aes(year, Emissions))
ggpl <- ggpl + geom_point(size = 8, shape = 18)
ggpl <- ggpl + geom_line(size = 2, linetype = 4)
ggpl <- ggpl + ylab("Emissions [tons]")
ggpl <- ggpl + ggtitle(expression('U.S. Total PM'[2.5]*' Coal Combustion-related Emission in Years 1999-2008'))
ggpl <- ggpl + theme(plot.title = element_text(hjust = 0.5))
ggpl <- ggpl + scale_x_continuous(breaks = coalcomb$year, labels = coalcomb$year)
ggpl
dev.off()
