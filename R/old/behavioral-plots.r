require(ggplot2)
require(Hmisc)


#univariate distribution of genres chosen by schematic class
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("class-num-lis-collapsed.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Anything (But Country)", "Anything (But Heavy Metal)", "Contempo-Traditional", "Omnivore-Univore", "High-Low"))
plot.data$numgenrescat <- factor(plot.data$numgenrescat, labels = c("1-2", "3-4", "5-6", "7-8", "9+"))
windows(height = 12, width = 16)
p <- ggplot(na.omit(plot.data), aes(x = numgenrescat, y = per, color = factor(classmem), group = factor(classmem)))
p <- p +  geom_line(size = 1.5) + geom_point(size = 8)
p <- p + theme_minimal() + theme(legend.position = "bottom", axis.text = element_text(size = 20), plot.title = element_text(size = 20), legend.title = element_blank(), legend.text = element_text(size = 12))
p <- p + scale_color_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "") + ggtitle("Univariate Distribution of Genres Chosen by Schematic Class")
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "omnivore-by-schematic-class.png", type = "png")	
dev.off()

#probability of chosing a genre by schematic class and number of genres chosen
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("genre-class-num-lis.dta")
plot.data$genre <- reorder(plot.data$genre, plot.data$lis.)
plot.data$classmem <- factor(plot.data$classmem, 
c("Anything (But Country)", "Anything (But Heavy Metal)", "Contempo-Traditional", "Omnivore-Univore", "High-Low"))
windows(height = 12, width = 16)
p <- ggplot(plot.data, aes(x = numgenrescat, y = lis., color = classmem, group = classmem))
p <- p +  geom_line() + geom_point(size = 3)
p <- p + facet_wrap( ~ genre, ncol = 5)
p <- p + labs(x = "", y = "") 
p <- p + theme_classic() 
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12), strip.text.x = element_text(size = 10))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "listening-by-schematic-class-and-omni.png", type = "png")	
dev.off()

#genres chosen by schematic class (all respondents)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("genre-class-lis.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Anything (But Country)", "Anything (But Heavy Metal)", "Contempo-Traditional", "Omnivore-Univore", "High-Low"))
plot.data$genre <- reorder(plot.data$genre, plot.data$lis.)
windows(height = 12, width = 16)
p <- ggplot(plot.data, aes(x = classmem, y = lis.))
p <- p + geom_segment(aes(x = classmem, y = lis., xend = classmem, yend = avg.lis, color = classmem), size = 1.25)
p <- p + geom_point(aes(color = classmem), size = 3.5) 
p <- p + geom_hline(aes(yintercept = avg.lis), color = "gray")
p <- p + facet_wrap( ~ genre, nrow = 5, scales = "free") 
p <- p + ggtitle("Comparative Probabilities of Listening to Each Genre Regularly by Schematic Class (All Respondents)")
p <- p + theme_classic() + labs(x = "", y = "", color = "", size = "")
p <- p + theme(legend.position = "bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text.x = element_text(size = 12), legend.text = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "listening-by-schematic-class.png", type = "png")	
dev.off()

#genres chosen by schematic class (omnivores)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("genre-class-lis-omni.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Avant-Garde.Clumper", "Folksy.Clumper", "Generational.Splitter", "Multicultural.Clumper", "Conventional.Clumper"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg.lis)

windows(height = 12, width = 16)
p <- ggplot(plot.data, aes(x = classmem, y = lis.))
p <- p + geom_segment(aes(x = classmem, y = lis., xend = classmem, yend = avg.lis, color = classmem), size = 1.25)
p <- p + geom_point(aes(color = classmem), size = 3.5) 
p <- p + geom_hline(aes(yintercept = avg.lis), color = "gray")
p <- p + facet_wrap( ~ genre, nrow = 5, scales = "free") 
p <- p + ggtitle("Comparative Probabilities of Listening to Each Genre Regularly by Schematic Class (Omnivores)")
p <- p + theme_classic() + labs(x = "", y = "", color = "", size = "")
p <- p + theme(legend.position = "bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text.x = element_text(size = 12), legend.text = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "listening-by-schematic-class-omnivores.png", type = "png")	
dev.off()

#genres chosen by schematic class (univores)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("genre-class-lis-univ.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Anything (But Country)", "Anything (But Heavy Metal)", "Contempo-Traditional", "Omnivore-Univore", "High-Low"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg.lis)
windows(height = 12, width = 16)
p <- ggplot(plot.data, aes(x = classmem, y = lis.))
p <- p + geom_segment(aes(x = classmem, y = lis., xend = classmem, yend = avg.lis, color = classmem), size = 1.25)
p <- p + geom_point(aes(color = classmem), size = 3.5) 
p <- p + geom_hline(aes(yintercept = avg.lis), color = "gray")
p <- p + facet_wrap( ~ genre, nrow = 5, scales = "free") 
p <- p + ggtitle("Comparative Probabilities of Listening to Each Genre Regularly by Schematic Class (Univores)")
p <- p + theme_classic() + labs(x = "", y = "", color = "", size = "")
p <- p + theme(legend.position = "bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text.x = element_text(size = 12), legend.text = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "listening-by-schematic-class-univores.png", type = "png")	
dev.off()

#favorite genre by schematic class (all respondents)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("genre-class-fav.dta")
plot.data$classmem <- factor(plot.data$classmem, 
labels = c("Avant-Garde.Clumper", "Folksy.Clumper", "Generational.Splitter", "Multicultural.Clumper", "Conventional.Clumper"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg.fav)
windows(height = 12, width = 16)
p <- ggplot(plot.data, aes(x = classmem, y = Fav.))
p <- p + geom_segment(aes(x = classmem, y = Fav., xend = classmem, yend = avg.fav, color = classmem), size = 1.35)
p <- p + geom_point(aes(color = classmem), size = 4) 
p <- p + geom_hline(aes(yintercept = avg.fav))
p <- p + facet_wrap( ~ genre, nrow = 5, scales = "free") 
p <- p + ggtitle("Probability of Picking Genre as Favorite by Schematic Class")
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "bottom", axis.text.y = element_text(size = 8), strip.text.x = element_text(size = 12), legend.text = element_text(size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank())
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "favorite-by-schematic-class.png", type = "png")	
dev.off()

#demographics by schematic class 
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
plot.data <- stata.get("schematic-class-demog.dta")
plot.data$order <- c(1:45)
plot.data$demog <- reorder(plot.data$demog, plot.data$order)
colnames(plot.data)[1] <- "Avant-Garde Clumper"
colnames(plot.data)[2] <- "Folksy Clumper"
colnames(plot.data)[3] <- "Generational Splitter"
colnames(plot.data)[4] <- "Multicultural Clumper"
colnames(plot.data)[5] <- "Conventional Clumper"
plot.data.m <- melt(plot.data[,c(1:6)], id = "demog")

windows(height = 12, width = 16)
p <- ggplot(plot.data.m, aes(x = demog, y = value))
p <- p + geom_point(aes(color = variable), size = 2.25) + coord_flip() 
p <- p + geom_segment(aes(x = demog, y = value, xend = demog, yend = 0.2, color = variable))
p <- p + geom_hline(aes(yintercept = 0.2, color = variable))
p <- p + facet_wrap( ~ variable, nrow = 1)
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", strip.text.x = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "demog-by-schematic-class.png", type = "png")	
dev.off()

