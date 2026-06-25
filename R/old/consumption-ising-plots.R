require("bootnet")
require("foreign")

############ getting and setting up data #########################
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
netcor.data <- read.dta("schematic-class-lis.dta")

netcor.data1 <- subset(netcor.data, classmem == 1)
netcor.data2 <- subset(netcor.data, classmem == 2)
netcor.data3 <- subset(netcor.data, classmem == 3)
netcor.data4 <- subset(netcor.data, classmem == 4)
netcor.data5 <- subset(netcor.data, classmem == 5)

netcor.data1 <- netcor.data1[, c(3:22)]
netcor.data2 <- netcor.data2[, c(3:22)]
netcor.data3 <- netcor.data3[, c(3:22)]
netcor.data4 <- netcor.data4[, c(3:22)]
netcor.data5 <- netcor.data5[, c(3:22)]

f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)
gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")

############ estimating consumption correlation network models #########################
results1 <- estimateNetwork(netcor.data1, default = "IsingFit")
results2 <- estimateNetwork(netcor.data2, default = "IsingFit")
results3 <- estimateNetwork(netcor.data3, default = "IsingFit")
results4 <- estimateNetwork(netcor.data4, default = "IsingFit")
results5 <- estimateNetwork(netcor.data5, default = "IsingFit")

############ consumption correlation network plot by schematic class ##################
windows(height = 14, width = 14)
  plot(results1, 
       groups = gen.groups, palette = "colorblind", legend = FALSE, borders = FALSE, esize = 15, posCol = "azure4", cut = 0.1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-isingnet-class1", type = "png")
dev.off()

windows(height = 14, width = 14)
  plot(results2, 
       groups = gen.groups, palette = "colorblind", legend = FALSE, borders = FALSE, esize = 15, posCol = "azure4", cut = 0.1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-isingnet-class2", type = "png")
dev.off()

windows(height = 14, width = 14)
  plot(results3, 
       groups = gen.groups, palette = "colorblind", legend = FALSE, borders = FALSE, esize = 10, posCol = "azure4", cut = 0.1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-isingnet-class3", type = "png")
dev.off()

windows(height = 14, width = 14)
  plot(results4, 
       groups = gen.groups, palette = "colorblind", legend = FALSE, borders = FALSE, esize = 10, posCol = "azure4", cut = 0.1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-isingnet-class4", type = "png")
dev.off()

windows(height = 14, width = 14)
  plot(results5, 
       groups = gen.groups, palette = "colorblind", legend = FALSE, borders = FALSE, esize = 10, posCol = "gray", cut = 0.1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-isingnet-class5", type = "png")
dev.off()

plot1 <- centralityTable(results3)
windows(height = 18, width = 14)
p <- ggplot(plot1, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p


plot1 <- clusteringTable(results4)
windows(height = 18, width = 14)
p <- ggplot(plot1, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p


############ creating intercept plot by genre ##########################
int1 <- as.data.frame(results1$intercepts)
int2 <- as.data.frame(results2$intercepts)
int3 <- as.data.frame(results3$intercepts)
int4 <- as.data.frame(results4$intercepts)
int5 <- as.data.frame(results5$intercepts)
int <- cbind(int1, int2, int3, int4, int5)
colnames(int) <- c("ABC", "ABHM1", "CT", "ABHM2", "HL")
int$id <- as.factor(rownames(int))
require("reshape")
int <- melt(int, id = "id")
colnames(int) <- c("Genre", "Class", "Intercept")

windows(height = 14, width = 18)
  p <- ggplot(int, aes(x = Class, y = Intercept, group = Genre, color = Genre))
  p <- p + geom_point(size = 3) + geom_line() 
  p <- p + facet_wrap(~Genre) + theme_minimal()
  p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "consumption-intercept-by-genre-class", type = "png")
dev.off()

############ genre centrality scores #########################
cent1 <- centrality(results1$graph)
cent2 <- centrality(results2$graph)
cent3 <- centrality(results3$graph)
cent4 <- centrality(results4$graph)
cent5 <- centrality(results5$graph)

require("reshape")
genre <- rownames(results1$graph)
genre.bet <- data.frame(genre, scale(cent1$Betweenness), scale(cent2$Betweenness), scale(cent3$Betweenness), 
                        scale(cent4$Betweenness), scale(cent5$Betweenness))
plot.bet <- melt(genre.bet, id = "genre")
genre.deg <-data.frame(genre, scale(cent1$OutDegree), scale(cent2$OutDegree), scale(cent3$OutDegree), 
                       scale(cent4$OutDegree), scale(cent5$OutDegree))
plot.deg <- melt(genre.deg, id = "genre")


class.mem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
plot.bet <- data.frame(plot.bet[c(1:100), ], class.mem)
plot.deg <- data.frame(plot.deg[c(1:100), ], class.mem)

plot.bet$class.mem <- factor(plot.bet$class.mem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal 1", 
                                        "Contempo-Trad", "Anything but Heavy Metal 2", "High-Low"))
plot.deg$class.mem <- factor(plot.deg$class.mem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal 1", 
                                        "Contempo-Trad", "Anything but Heavy Metal 2", "High-Low"))
#genre centrality plots
windows(height = 14, width = 18)
p <- ggplot(plot.bet, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5) 
p <- p + theme(legend.position = "none") 
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-betweenness-isingnet.png", type = "png")
dev.off()

windows(height = 14, width = 18)
p <- ggplot(plot.deg, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5) 
p <- p + theme(legend.position = "none")
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-strength-isingnet.png", type = "png")
dev.off()
