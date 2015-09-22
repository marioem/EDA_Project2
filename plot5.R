#################################################################################
#
#  Filename: plot5.R
#   Version: 1.0.0
#      Date: 2015.09.21
#    Author: Mariusz Musiał
# Rev. Info: 1.0.0 - initial version of the script
# 
# This script generates a plot of PM25 motor vehicle-related emissions in
# Baltimore City in years 1999-2008. For detailed discussion of data selection
# and presentation see comments in the code.
# ggplot2 grapics system is used.
# Original data is available at: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# More information about data: http://www.epa.gov/ttn/chief/eiinformation.html
# 
#     Input: summarySCC_PM25.rds, Source_Classification_Code.rds files located
#            in the current working directory
#    Output: plot5.png written to the current working directory
# Execution: source(plot5.R)
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
pngFile <- "plot5.png"

BaltimoreFips <- "24510"

pm <- readRDS(summaryFile)
scc <- readRDS(sccFile)

# Data selection decision:
# For the present analysis a "motor vehicle" is deemed a mobile source which is
# descibed as a bus, truck, vehicle or a motorcycle, powered by either diesel or 
# gasoline fuel.
# Excluded are therefore aircrafts, vessels, construction equippment, agriculture
# or garden equippment, etc. Excluded are also emissions not related to motor
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
motorvehi <- subpm %>% filter(fips == BaltimoreFips) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

# Plot the emissions levels vs. year along with the linear regression line
#
# Design decision: As PM25 emissions can be cotrolled only "approximately"
# (due to high number of influencing factors) as a user of this grpah I'd be 
# more interested in the trend over years rather than in the exact shape of
# the emissions curve. Hence the yearly emisions are marked as points 
# and a trend line (linear regression) is added.

png(pngFile, width = 640)
ggpl <- ggplot(data = motorvehi, aes(year, Emissions))
ggpl <- ggpl + geom_point(size = 4, color = "darkgreen")
ggpl <- ggpl + geom_smooth(method = "lm", se = F, color = "darkgreen")
ggpl <- ggpl + ylab("Emissions [tons]")
ggpl <- ggpl + ggtitle(expression('Baltimore City PM'[2.5]*' Motor Vehicle-related Emission in Years 1999-2008'))
ggpl <- ggpl + scale_x_continuous(breaks = motorvehi$year, labels = motorvehi$year)
ggpl
dev.off()
