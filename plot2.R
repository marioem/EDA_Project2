require(dplyr)

BaltimoreFips <- "24510"

pm <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

baltpmtot <- pm %>% filter(fips == BaltimoreFips) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

plot(baltpmtot, ylab = "Emissions [tons]", xaxt = "n")
abline(lm(Emissions ~ year, baltpmtot),col = "red")
legend("bottomleft", legend = c("Total Emissions", "Lin. Regr. Tot. Emiss."), col = c("black", "red"), pch = c(1,45))
title(main = expression('Baltimore City Total PM'[2.5]*' Emissions in Years 1999-2008'))
axis(side = 1, at = pmtot$year,labels = T)
