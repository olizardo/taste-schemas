require("qgraph")
require("haven")
require("networktools")
############ getting and setting up data #################################
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
netcor.data <- read_dta("schematic-class.dta")

############ estimating polychoric correlations #################################
for (i in 1:5) {
  x <- subset(netcor.data, classmem == i)[,c(2:21)]
  c <- paste("cg", i, sep = "") 
  g <- qgraph(cor_auto(x), labels = c(colnames(x)))
  assign(c, g, pos = .GlobalEnv)
  ei <- paste("inf", i, sep = "")
  assign(ei, expectedInf(g), pos = .GlobalEnv)
  }
inf.dat <- as.data.frame(rbind(inf1[[1]], inf2[[1]], inf2[[1]], inf3[[1]], inf4[[1]]), inf5[[1]])
require("reshape")
inf.dat <- melt(inf.dat)
names(inf.dat) <- c("genre", "step1")
inf.dat$classmem <- c(rep(c(1:5), 20))

############ validation plot #################################
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
lis.data <- read_dta("class-genre-corr-omni.dta")
plot.data <- merge(lis.data, inf.dat, by = c("classmem", "genre"))
plot.data$classmem <- factor(plot.data$classmem,
                           labels = c("Anything but Country", "Anything but Heavy Metal", 
                                      "Contempo-Trad", "Omnivore-Univore", "Art-Pop"))

require("foreign")
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
write.dta(plot.data, "plot.data.dta1")

require("ggrepel")
windows(height = 8, width = 16)
p <- ggplot(plot.data, aes(y = lis_, x = step1, fill = classmem))
p <- p + geom_smooth(method = "lm", alpha = 0.05, size = 0.5, aes(color = classmem))
p <- p + geom_text_repel(aes(label = genre), size = 2.35)
p <- p + geom_vline(xintercept = 0, color = "gray")
p <- p + geom_hline(yintercept = 0, color = "gray")
p <- p + labs(y = "", x = "")
p <- p + facet_wrap(~classmem, ncol = 5)
p <- p + theme_classic() + theme(legend.position = "none")
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "schema-validation1", type = "png")
dev.off()

#genres chosen by schematic class (all respondents; dropline plot)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
plot.data <- read_dta("class-genre-lis-omni.dta")
plot.data$classmem <- factor(plot.data$classmem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal", "Contemporary-Traditional", 
                                        "Omnivore-Univore", "Artistic-Popular"))
plot.data$genre <- reorder(plot.data$genre, plot.data$avg_lis)
windows(height = 14, width = 20)
p <- ggplot(plot.data, aes(x = classmem, y = lis_, fill = classmem))
p <- p + geom_point(aes(color = classmem), size = 3.25) + coord_flip()
p <- p + geom_segment(aes(x = classmem, y = lis_, xend = classmem, yend = 0, color = classmem))
p <- p + geom_hline(aes(yintercept = 0, color = classmem))
p <- p + facet_wrap( ~ genre, nrow = 5)
p <- p + theme_classic() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "schema-validation2", type = "png")
dev.off()

############ validation plot #####################################
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
lis.data <- read_dta("class-genre-lis-omni.dta")
plot.data <- merge(lis.data, inf.dat, by = c("classmem", "genre"))
plot.data$classmem <- factor(plot.data$classmem,
                             labels = c("Anything but Country", "Anything but Heavy Metal", 
                                        "Contempo-Trad", "Omnivore-Univore", "Art-Pop"))

require("foreign")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
write.dta(plot.data, "plot.data.dta")

require("ggrepel")
windows(height = 16, width = 16)
p <- ggplot(plot.data, aes(y = lis_, x = step1))
p <- p + geom_smooth(method = "lm", alpha = 0.05, size = 0.5)
p <- p + geom_text_repel(aes(label = genre, color = classmem), size = 2.3)
p <- p + geom_vline(xintercept = 0, color = "gray")
p <- p + geom_hline(yintercept = 0, color = "gray")
p <- p + labs(y = "", x = "")
p <- p + theme_classic() + theme(legend.position = "none")
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "schema-validation3", type = "png")
dev.off()


require("ggrepel")
windows(height = 8, width = 16)
p <- ggplot(plot.data, aes(y = lis_, x = step1))
p <- p + geom_smooth(method = "lm", alpha = 0.05, size = 0.5)
p <- p + geom_text_repel(aes(label = genre, color = classmem), size = 2)
p <- p + geom_vline(xintercept = 0, color = "gray")
p <- p + geom_hline(yintercept = 0, color = "gray")
p <- p + labs(y = "", x = "")
p <- p + facet_wrap(~std_lis3, ncol = 4)
p <- p + theme_classic() + theme(legend.position = "none")
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "schema-validation4", type = "png")
dev.off()

############ centrality plots #####################################
p <- ggplot(plot.data[c(1:20), ], aes(x = step1, y = reorder(genre, step1), group = classmem))
p <- p + geom_point() + geom_line() + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(y = "", x = "", title = "Anything (but Country)")
p1 <- p + theme_minimal() + theme(legend.position = "none")

p <- ggplot(plot.data[c(21:40), ], aes(x = step1, y = reorder(genre, step1), group = classmem))
p <- p + geom_point() + geom_line() + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(y = "", x = "", title = "Anything (but Heavy Metal)")
p2 <- p + theme_minimal() + theme(legend.position = "none")

p <- ggplot(plot.data[c(41:60), ], aes(x = step1, y = reorder(genre, step1), group = classmem))
p <- p + geom_point() + geom_line() + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(y = "", x = "", title = "Contemporary/Traditional")
p3 <- p + theme_minimal() + theme(legend.position = "none")

p <- ggplot(plot.data[c(61:80), ], aes(x = step1, y = reorder(genre, step1), group = classmem))
p <- p + geom_point() + geom_line() + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(y = "", x = "", title = "Omnivore/Univore")
p4 <- p + theme_minimal() + theme(legend.position = "none")

p <- ggplot(plot.data[c(81:100), ], aes(x = step1, y = reorder(genre, step1), group = classmem))
p <- p + geom_point() + geom_line() + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(y = "", x = "", title = "Artistic/Pop")
p5 <- p + theme_minimal() + theme(legend.position = "none")


setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
windows(height = 16, width = 8)
p1
savePlot(filename = "taste-centrality-class1", type = "png")
p2
savePlot(filename = "taste-centrality-class2", type = "png")
p3
savePlot(filename = "taste-centrality-class3", type = "png")
p4
savePlot(filename = "taste-centrality-class4", type = "png")
p5
savePlot(filename = "taste-centrality-class5", type = "png")

























