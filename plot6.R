#################################################################################
#
#  Filename: plot6.R
#   Version: 1.0.0
#      Date: 2015.09.21
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
# 
# This script generates a plot of PM25 motor vehicle-related emissions in
# Baltimore City and LA County in years 1999-2008. For detailed discussion
# of data selection and presentation see comments in the code.
# ggplot2 grapics system is used.
# Original data is available at: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# More information about data: http://www.epa.gov/ttn/chief/eiinformation.html
# 
#     Input: summarySCC_PM25.rds, Source_Classification_Code.rds files located
#            in the current working directory
#    Output: plot6.png written to the current working directory
# Execution: source(plot6.R)
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
pngFile <- "plot6.png"

BaltimoreFips <- "24510"
LACountyFips <- "06037"

pm <- readRDS(summaryFile)
scc <- readRDS(sccFile)

# Data selection decision:
# For the present analysis a "motor vehicle" is deemed a mobile source which is
# descibed as a bus, truck, vehicle or a motorcycle, powered by either diesel or 
# gasoline fuel.
# Excluded are therefore aircrafts, vessels, construction equipment, agriculture
# or garden equipment, etc. Excluded are also emissions not related to motor
# fuel combustion, e.g. dust emitted from paved/unpaved roads, etc.
# This decision is based on the assumption that because the area of interest in
# Question 5 is an urban area, the interpretation of "motor vehicle" is that of
# a motor-powered vehicle used for any kind of road transportation.
# This decision led to the selection of SCC codes which describe "Mobile Sources"
# on Level One and bus or vehicle or truck or motorcycle on Level Three.
# (This selection leads basically to Level Two Highway Vehicles without
# LevelThree "NOT USED - Previously all LDGT (1&2) under M5" group).

smobile <- grepl("mobile", scc$SCC.Level.One, ignore.case = T)

struck <- grepl("truck", scc$SCC.Level.Three, ignore.case = T)
sbus <- grepl("bus", scc$SCC.Level.Three, ignore.case = T)
svehi <- grepl("vehicle", scc$SCC.Level.Three, ignore.case = T)
smoto <- grepl("motorcycle", scc$SCC.Level.Three, ignore.case = T)

scc$SCC <- as.character(scc$SCC) # so that SCC var in both scc and pm data frame are of character type
subscc <- scc[smobile & (struck | sbus | svehi | smoto),]
subpm <- semi_join(pm, subscc, by = "SCC")

# Calculate total coal combustion-related emissions per measurement year
# Some of the SCC codes bear notion of "Total" or "All" (those ending with "000").
# This suggests that it can be some kind of subsum. A random check of a certain
# series didn't confirm this, so for the purpose of this assignment I assume it
# is OK to sum over SCCs without excluding suspected subsums.
motorvehi <- subpm %>% filter(fips %in% c(BaltimoreFips, LACountyFips)) %>% group_by(fips, year) %>% summarise(Emissions = sum(Emissions))

# Question 6 is related to the change over the years and its comparision between
# LA and Baltimore county. Hence we anchor plots for both counties on the relative
# emission level of 0 tons in 1999. Let's do the level shift in a simple way:

center <- c(rep(motorvehi$Emissions[1],4), rep(motorvehi$Emissions[5],4))
motorvehi$Emissions <- motorvehi$Emissions - center

# Plot semi-normalized emissions levels vs. year
#
# Design decision: In order to compare the changes in emissions between the two
# locations a composite line plot was chosen. Relative data anchored at 0 tons
# emission in 1999 is plotted.

png(pngFile, width = 640)
ggpl <- ggplot(data = motorvehi, aes(year, Emissions, color = fips))
ggpl <- ggpl + geom_point(size = 4, alpha = 1/2)
ggpl <- ggpl + geom_line()
ggpl <- ggpl + ylab("Emission change [tons]")
ggpl <- ggpl + ggtitle(expression('Baltimore vs. LA PM'[2.5]*' Motor Vehicle-related Emission Change from 1999 to 2008'))
ggpl <- ggpl + scale_x_continuous(breaks = motorvehi$year, labels = motorvehi$year)
ggpl <- ggpl + scale_colour_discrete(name = "County", labels = c("LA", "Baltimore"))
ggpl
dev.off()
