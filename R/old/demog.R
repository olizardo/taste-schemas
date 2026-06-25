require("ggplot2")
require("Hmisc")
require("reshape")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("schematic-class-demog.dta")

plot.data$order <- c(1:45)
plot.data$demog <- reorder(plot.data$demog, plot.data$order)
plot.data.m <- melt(plot.data[,c(1:5, 6)], id = "demog")

dev.off()
dev.new(height = 14, width = 18)
p <- ggplot(plot.data.m, aes(x = demog, y = value))
p <- p + geom_point(aes(color = variable), size = 3.25) + coord_flip() 
p <- p + geom_segment(aes(x = demog, y = value, xend = demog, yend = 0.2, color = variable))
p <- p + geom_hline(aes(yintercept = 0.2, color = variable))
p <- p + facet_wrap( ~ variable, nrow = 1)
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", strip.text.x = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "demog-by-schematic-class.png", type = "png")	







