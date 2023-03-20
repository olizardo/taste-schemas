require(ggplot2)
require(Hmisc)

setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("scales-demog.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Avant-Garde.Clumper", "Folksy.Clumper", "Generational.Splitter", "Multicultural.Clumper", "Conventional.Splitter"))


p <- ggplot(na.omit(plot.data), aes(x = c.inc, y = art.scal, colour = classmem))
p <- p + geom_smooth(se = TRUE, span = 1.5)
p <- p + theme_minimal()
p <- p + facet_wrap( ~ classmem, nrow = 1)
p <- p + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), strip.text = element_blank())
p1 <- p + labs(x = "education", y = "Art Scale") + ggtitle(" ")
p1

p <- ggplot(na.omit(plot.data), aes(x = c.age, y = folk.scal, colour = classmem))
p <- p + geom_smooth(se = TRUE, span = 1.5)
p <- p + theme_minimal()
p <- p + facet_wrap( ~ classmem, nrow = 1)
p <- p + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), strip.text = element_blank())
p2 <- p + labs(x = "education", y = "Folk Scale") + ggtitle(" ")
p2


p <- ggplot(na.omit(plot.data), aes(x = c.age, y = rock.scal, colour = classmem))
p <- p + geom_smooth(se = TRUE, span = 1.5)
p <- p + theme_minimal()
p <- p + facet_wrap( ~ classmem, nrow = 1)
p <- p + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), strip.text = element_blank())
p3 <- p + labs(x = "education", y = "Rock Pop Scale") + ggtitle(" ")
p3


p <- ggplot(na.omit(plot.data), aes(x = c.age, y = afro.scal, colour = classmem))
p <- p + geom_smooth(se = TRUE, span = 1.5)
p <- p + theme_minimal()
p <- p + facet_wrap( ~ classmem, nrow = 1)
p <- p + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), legend.title = element_blank(), strip.text = element_blank())
p4 <- p + labs(x = "education", y = "Afro Pop Scale") + ggtitle(" ")
p4

dev.off()
dev.new(height = 12, width = 8)
require(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow =2)
