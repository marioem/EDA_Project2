require(dplyr)

pm <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

pmtot <- pm %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

plot(pmtot, ylab = "Emissions [tons]", xaxt = "n")
abline(lm(Emissions ~ year, pmtot),col = "red")
legend("topright", legend = c("Total Emissions", "Lin. Regr. Tot. Emiss."), col = c("black", "red"), pch = c(1,45))
title(main = expression('U.S. Total PM'[2.5]*' Emissions in Years 1999-2008'))
axis(side = 1, at = pmtot$year,labels = T)
