require(ggplot2)
require(haven)

#univariate distribution of genres chosen by schematic class
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
plot.data <- read_dta("class-numlis.dta")
plot.data$classmem <- factor(plot.data$classmem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal", "Contemporary-Traditional", 
                                        "Omnivore-Univore", "Artistic-Popular"))
plot.data$numgenrescat <- factor(plot.data$numgenrescat, labels = c("1-2", "3-4", "5-6", "7-8", "9+"))
windows(height = 14, width = 20)
p <- ggplot(plot.data, aes(x = as.factor(numgenrescat), y = per, color = classmem, group = classmem))
p <- p +  geom_line(size = 1.5) + geom_point(size = 8)
p <- p + theme_minimal() + theme(legend.position = "bottom", axis.text = element_text(size = 20), plot.title = element_text(size = 20), legend.title = element_blank(), legend.text = element_text(size = 16))
p <- p + scale_color_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "") + ggtitle(" ")
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "numlis-by-schematic-class.png", type = "png")	
dev.off()

#genres chosen by schematic class (all respondents; dropline plot)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
plot.data <- read_dta("class-genre-lis-omni.dta")
plot.data$classmem <- factor(plot.data$classmem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal", "Contemporary-Traditional", 
                                        "Omnivore-Univore", "Artistic-Popular"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg.lis)
dev.off()
windows(height = 14, width = 20)
p <- ggplot(plot.data, aes(x = genre, y = lis_))
p <- p + geom_point(aes(color = classmem), size = 3.25) + coord_flip() 
p <- p + geom_segment(aes(x = genre, y = lis_, xend = genre, yend = 0, color = classmem))
p <- p + geom_hline(aes(yintercept = 0, color = classmem))
p <- p + facet_wrap( ~ classmem, nrow = 1)
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-lis-by-schematic-class.png", type = "png")	
dev.off()

#probability of chosing a genre by schematic class and number of genres chosen
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
plot.data <- read_dta("class-genre-numlis-lis.dta")
plot.data$genre <- reorder(plot.data$genre, plot.data$lis.)
plot.data$classmem <- factor(plot.data$classmem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal", "Artistic-Popular", 
                                        "Omnivore-Univore", "Contemporary-Traditional"))
plot.data$numgenrescat <- factor(plot.data$numgenrescat, labels = c("1-2", "3-4", "5-6", "7-8", "9+"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg_lis)
windows(height = 14, width = 20)
p <- ggplot(plot.data, aes(x = numgenrescat, y = lis_, color = classmem, group = classmem))
p <- p +  geom_line() + geom_point(size = 3)
p <- p + theme_minimal() 
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), 
               legend.text = element_text(size = 12), strip.text.x = element_text(size = 10))
p <- p + scale_color_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "") 
p <- p + facet_wrap( ~ genre, ncol = 5)
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-lis-by-schematic-class-and-numlis.png", type = "png")	
dev.off()

#favorite genre by schematic class (all respondents)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
plot.data <- read_dta("class-genre-fav.dta")
plot.data$classmem <- factor(plot.data$classmem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal", "Artistic-Popular", 
                                        "Omnivore-Univore", "Contemporary-Traditional"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg_lis)
windows(height = 14, width = 20)
p <- ggplot(plot.data, aes(x = genre, y = Fav_))
p <- p + geom_point(aes(color = classmem), size = 3.25) + coord_flip() 
p <- p + geom_segment(aes(x = genre, y = Fav_, xend = genre, yend = 0, color = classmem))
p <- p + geom_hline(aes(yintercept = 0, color = classmem))
p <- p + facet_wrap( ~ classmem, nrow = 1) 
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", axis.text = element_text(size = 12), strip.text.x = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-fav-by-schematic-class.png", type = "png")	
dev.off()



















