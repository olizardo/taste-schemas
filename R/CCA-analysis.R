#load packages
require(corclass)
require(foreign)

setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
cca.data <- read.dta("taste.dta")
#Five Class Solution
cca.res <- cca(cca.data[, c(2:21)], filter.value = 0.01)
cca.data <- data.frame(cca.data, cca.res$membership)
colnames(cca.data)[colnames(cca.data) == "cca.res.membership"] <- "classmem"
write.dta(cca.data, "schematic-class.dta")


