require("bootnet")
require("qgraph")
require("haven")
require("reshape")


############ getting and setting up data #########################
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
netcor.data <- read_dta("schematic-class.dta")

netcor.data1 <- subset(netcor.data, classmem == 1)
netcor.data2 <- subset(netcor.data, classmem == 2)
netcor.data3 <- subset(netcor.data, classmem == 3)
netcor.data4 <- subset(netcor.data, classmem == 4)
netcor.data5 <- subset(netcor.data, classmem == 5)

netcor.data1 <- netcor.data1[, c(2:21)]
netcor.data2 <- netcor.data2[, c(2:21)]
netcor.data3 <- netcor.data3[, c(2:21)]
netcor.data4 <- netcor.data4[, c(2:21)]
netcor.data5 <- netcor.data5[, c(2:21)]

f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)
gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")

cor1 <- cor_auto(netcor.data1)
cor2 <- cor_auto(netcor.data2)
cor3 <- cor_auto(netcor.data3)
cor4 <- cor_auto(netcor.data4)
cor5 <- cor_auto(netcor.data5)

############ estimating taste correlation network models #########################
pcor1 <- estimateNetwork(netcor.data1, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
pcor2 <- estimateNetwork(netcor.data2, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
pcor3 <- estimateNetwork(netcor.data3, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
pcor4 <- estimateNetwork(netcor.data4, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
pcor5 <- estimateNetwork(netcor.data5, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)

corrnetclust1 <- scale(clustcoef_auto(cor1))
corrnetclust2 <- scale(clustcoef_auto(cor1))
corrnetclust3 <- scale(clustcoef_auto(cor1))
corrnetclust4 <- scale(clustcoef_auto(cor1))
corrnetclust5 <- scale(clustcoef_auto(cor1))

pcorrnetclust1 <- scale(clustcoef_auto(pcor1))
pcorrnetclust2 <- scale(clustcoef_auto(pcor2))
pcorrnetclust3 <- scale(clustcoef_auto(pcor3))
pcorrnetclust4 <- scale(clustcoef_auto(pcor4))
pcorrnetclust5 <- scale(clustcoef_auto(pcor5))


clustdat1 <- cbind(corrnetclust1[, 6], pcorrnetclust1[,6])
clustdat2 <- cbind(corrnetclust2[, 6], pcorrnetclust2[,6])
clustdat3 <- cbind(corrnetclust3[, 6], pcorrnetclust3[,6])
clustdat4 <- cbind(corrnetclust4[, 6], pcorrnetclust4[,6])
clustdat5 <- cbind(corrnetclust5[, 6], pcorrnetclust5[,6])

colnames(clustdat1) <- c("Raw", "Partial")
colnames(clustdat2) <- c("Raw", "Partial")
colnames(clustdat3) <- c("Raw", "Partial")
colnames(clustdat4) <- c("Raw", "Partial")
colnames(clustdat5) <- c("Raw", "Partial")

clustdat1 <- melt(clustdat1, id = rownames(clustdat1))
clustdat2 <- melt(clustdat2, id = rownames(clustdat2))
clustdat3 <- melt(clustdat3, id = rownames(clustdat3))
clustdat4 <- melt(clustdat4, id = rownames(clustdat4))
clustdat5 <- melt(clustdat5, id = rownames(clustdat5))

windows(height = 18, width = 14)
p <- ggplot(clustdat1, aes(x = factor(X1), y = value, fill = factor(X2)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-clustering-comparison-class1.png", type = "png")
dev.off()

windows(height = 18, width = 14)
p <- ggplot(clustdat2, aes(x = factor(X1), y = value, fill = factor(X2)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-clustering-comparison-class2.png", type = "png")
dev.off()

windows(height = 18, width = 14)
p <- ggplot(clustdat3, aes(x = factor(X1), y = value, fill = factor(X2)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-clustering-comparison-class3.png", type = "png")
dev.off()

windows(height = 18, width = 14)
p <- ggplot(clustdat4, aes(x = factor(X1), y = value, fill = factor(X2)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-clustering-comparison-class4.png", type = "png")
dev.off()


windows(height = 18, width = 14)
p <- ggplot(clustdat5, aes(x = factor(X1), y = value, fill = factor(X2)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-clustering-comparison-class5.png", type = "png")
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
genre.clos <- data.frame(genre, scale(cent1$Closeness), scale(cent2$Closeness), scale(cent3$Closeness), 
                         scale(cent4$Closeness), scale(cent5$Closeness))
plot.clos <- melt(genre.clos, id = "genre")
genre.deg <-data.frame(genre, scale(cent1$OutDegree), scale(cent2$OutDegree), scale(cent3$OutDegree), 
                       scale(cent4$OutDegree), scale(cent5$OutDegree))
plot.deg <- melt(genre.deg, id = "genre")



class.mem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
plot.bet <- data.frame(plot.bet[c(1:100), ], class.mem)
plot.deg <- data.frame(plot.deg[c(1:100), ], class.mem)
plot.clos <- data.frame(plot.clos[c(1:100), ], class.mem)

plot.bet$class.mem <- factor(plot.bet$class.mem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal 1", 
                                        "Contempo-Trad", "Anything but Heavy Metal 2", "High-Low"))
plot.deg$class.mem <- factor(plot.deg$class.mem, 
                             labels = c("Anything but Country", "Anything but Heavy Metal 1", 
                                        "Contempo-Trad", "Anything but Heavy Metal 2", "High-Low"))
plot.clos$class.mem <- factor(plot.clos$class.mem, 
                              labels = c("Anything but Country", "Anything but Heavy Metal 1", 
                                         "Contempo-Trad", "Anything but Heavy Metal 2", "High-Low"))

#genre centrality plots
plot1 <- centralityTable(results1)
windows(height = 18, width = 14)
p <- ggplot(plot1, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-pcorrnet-class1.png", type = "png")
dev.off()

plot <- centralityTable(results2)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-pcorrnet-class2.png", type = "png")
dev.off()

plot <- centralityTable(results3)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-pcorrnet-class3.png", type = "png")
dev.off()

plot <- centralityTable(results4)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-pcorrnet-class4.png", type = "png")
dev.off()

plot <- centralityTable(results5)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-pcorrnet-class5.png", type = "png")
dev.off()

#genre centrality plots
windows(height = 14, width = 18)
p <- ggplot(plot.bet, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "none")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5) 
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-betweenness-pcorrnet.png", type = "png")
dev.off()

windows(height = 14, width = 18)
p <- ggplot(plot.clos, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "none")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5) 
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-closeness-pcorrnet.png", type = "png")
dev.off()

windows(height = 14, width = 18)
p <- ggplot(plot.deg, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "none")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5) 
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-strength-pcorrnet.png", type = "png")
dev.off()

