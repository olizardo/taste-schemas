require(ggplot2)
require(Hmisc)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("fav-plot.dta")
lis.rank.data <- stata.get("lis-rank.dta")

plot.data$genre2 <- reorder(plot.data$genre, lis.rank.data$lis.)
p <- ggplot(plot.data, aes(x = factor(classmem), y = Fav.))
p <- p + geom_bar(aes(fill = factor(classmem)), stat = "identity") 
p <- p + theme_classic() 
p <- p + scale_color_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "", fill = "") 
p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p <- p + facet_wrap( ~ genre2, ncol = 5, scales = "free")
p

setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "favorite-by-schematic-class.png", type = "png")	


