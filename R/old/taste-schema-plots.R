require("haven")
require("qgraph")
require("bootnet")
require("reshape")
require("ggplot2")
require("networktools")

############ getting and setting up data ################################################
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
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

############ estimating polychoric correlations #################################
cor1 <- cor_auto(netcor.data1)
cor2 <- cor_auto(netcor.data2)
cor3 <- cor_auto(netcor.data3)
cor4 <- cor_auto(netcor.data4)
cor5 <- cor_auto(netcor.data5)


f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)
gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")

#correlation plots for class 1
windows(height = 14, width = 14)
cg1 <- qgraph(cor1, layout = "spring", labels = c(rownames(cor1)), 
              graph = "assosciation", sampleSize = nrow(netcor.data1), alpha = 0.05, minimum = 0.01, threshold = "sig",
              groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", borders = FALSE)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class1", type = "png")
dev.off()
#correlation plot for class 2
windows(height = 14, width = 14)
cg2 <- qgraph(cor2, layout = "spring", labels = c(rownames(cor1)), 
              graph = "assosciation", sampleSize = nrow(netcor.data2), alpha = 0.05, minimum = 0.05, threshold = "sig",
              groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", borders = FALSE)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class2", type = "png")
#correlation plots for class 3
windows(height = 14, width = 14)
cg3 <- qgraph(cor3, layout = "spring", labels = c(rownames(cor1)),
              graph = "assosciation", sampleSize = nrow(netcor.data3), alpha = 0.05, minimum = 0.05, threshold = "sig",
              groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", borders = FALSE)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class3", type = "png")
dev.off()
#correlation plots for class 4
windows(height = 14, width = 14)
cg4 <- qgraph(cor4, layout = "spring", labels = c(rownames(cor1)), 
              graph = "assosciation", sampleSize = nrow(netcor.data4), alpha = 0.05, minimum = 0.05, threshold = "sig",
              groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", borders = FALSE)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class4", type = "png")
dev.off()
#correlation plots for class 5
windows(height = 14, width = 14)
cg5 <- qgraph(cor5, layout = "spring", labels = c(rownames(cor1)),
              graph = "assosciation", sampleSize = nrow(netcor.data5), alpha = 0.05, minimum = 0.05, threshold = "sig",
              groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", borders = FALSE)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class5", type = "png")
dev.off()


#partial correlation plot for class 1
pcor1 <- estimateNetwork(netcor.data1, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
windows(height = 14, width = 14)
qgraph(pcor1$graph, labels = c(rownames(cor1)), layout = cg1$layout, borders = FALSE,
               groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class1", type = "png")
dev.off()
#partial correlation plot for class 2
pcor2 <- estimateNetwork(netcor.data2, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
windows(height = 14, width = 14)
pcg2 <- qgraph(pcor2$graph, labels = c(rownames(cor1)), layout = cg2$layout, borders = FALSE,
               groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class2", type = "png")
dev.off()
#partial correlation plot for class 3
pcor3 <- estimateNetwork(netcor.data3, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
windows(height = 14, width = 14)
pcg3 <- qgraph(pcor2$graph, labels = c(rownames(cor1)), layout = cg3$layout, borders = FALSE,
               groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class3", type = "png")
dev.off()
#partial correlation plot for class 4
pcor4 <- estimateNetwork(netcor.data4, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
windows(height = 14, width = 14)
pcg4 <- qgraph(pcor4$graph, labels = c(rownames(cor1)), layout = cg4$layout, borders = FALSE,
               groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class4", type = "png")
dev.off()
#partial correlation plot for class 5
pcor5 <- estimateNetwork(netcor.data5, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5)
windows(height = 14, width = 14)
pcg5 <- qgraph(pcor5$graph, labels = c(rownames(cor1)), layout = cg5$layout, borders = FALSE,
               groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class5", type = "png")
dev.off()



pcor1.mat <- pcor1$graph
pcor1.neg <- ifelse(pcor1.mat > 0, 0, pcor1.mat)
pcor1.pos <- ifelse(pcor1.mat < 0, 0, pcor1.mat)
windows(height = 14, width = 14)
qgraph(pcor1.neg, labels = c(rownames(cor1)), layout = cg1$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class1-neg", type = "png")
windows(height = 14, width = 14)
qgraph(pcor1.pos, labels = c(rownames(cor1)), layout = cg1$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class1-pos", type = "png")
dev.off()

pcor2.mat <- pcor2$graph
pcor2.neg <- ifelse(pcor2.mat > 0, 0, pcor2.mat)
pcor2.pos <- ifelse(pcor2.mat < 0, 0, pcor2.mat)
windows(height = 14, width = 14)
qgraph(pcor2.neg, labels = c(rownames(cor1)), layout = cg2$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class2-neg", type = "png")
windows(height = 14, width = 14)
qgraph(pcor2.pos, labels = c(rownames(cor1)), layout = cg2$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class2-pos", type = "png")
dev.off()


pcor3.mat <- pcor3$graph
pcor3.neg <- ifelse(pcor3.mat > 0, 0, pcor3.mat)
pcor3.pos <- ifelse(pcor3.mat < 0, 0, pcor3.mat)
windows(height = 14, width = 14)
qgraph(pcor3.neg, labels = c(rownames(cor1)), layout = cg3$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class3-neg", type = "png")
windows(height = 14, width = 14)
qgraph(pcor3.pos, labels = c(rownames(cor1)), layout = cg3$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class3-pos", type = "png")
dev.off()


pcor4.mat <- pcor4$graph
pcor4.neg <- ifelse(pcor4.mat > 0, 0, pcor4.mat)
pcor4.pos <- ifelse(pcor4.mat < 0, 0, pcor4.mat)
windows(height = 14, width = 14)
qgraph(pcor4.neg, labels = c(rownames(cor1)), layout = cg4$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class4-neg", type = "png")
windows(height = 14, width = 14)
qgraph(pcor4.pos, labels = c(rownames(cor1)), layout = cg4$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class4-pos", type = "png")
dev.off()

pcor5.mat <- pcor5$graph
pcor5.neg <- ifelse(pcor5.mat > 0, 0, pcor5.mat)
pcor5.pos <- ifelse(pcor5.mat < 0, 0, pcor5.mat)
windows(height = 14, width = 14)
qgraph(pcor5.neg, labels = c(rownames(cor1)), layout = cg5$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 10, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class5-neg", type = "png")
windows(height = 14, width = 14)
qgraph(pcor5.pos, labels = c(rownames(cor1)), layout = cg5$layout, borders = FALSE,
       groups = gen.groups, palette = "colorblind", legend = FALSE, esize = 15, posCol = "gray", minimum = 0.05)
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class5-pos", type = "png")
dev.off()
############ genre centrality scores #########################

cent.pos1 <- centrality(pcor1.pos)
cent.pos2 <- centrality(pcor2.pos)
cent.pos3 <- centrality(pcor3.pos)
cent.pos4 <- centrality(pcor4.pos)
cent.pos5 <- centrality(pcor5.pos)
cent.pos1 <- scale(cent.pos1$OutExpectedInfluence)
cent.pos2 <- scale(cent.pos2$OutExpectedInfluence)
cent.pos3 <- scale(cent.pos3$OutExpectedInfluence)
cent.pos4 <- scale(cent.pos4$OutExpectedInfluence)
cent.pos5 <- scale(cent.pos5$OutExpectedInfluence)
genre <- rownames(cor1)
cent.inf.pos <- cbind.data.frame(genre, cent.pos1, cent.pos2, cent.pos3, cent.pos4, cent.pos5)
cent.inf.pos <- melt(cent.inf.pos, by = "genre")
cent.inf.pos <- cent.inf.pos[, c("genre", "value")]

cent.inf.pos$classmem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))

setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
lis.data <- read_dta("class-genre-numlis-lis.dta")
lis.data5 <- subset(lis.data, numgenrescat == 5)
lis.data1 <- subset(lis.data, numgenrescat == 1)
lis.data <- cbind(lis.data1, lis.data5[, 4])
names(lis.data)[c(4, 6)] <- c("lis_1", "lis_2")
lis.data$lis_ <- (lis.data$lis_2 - lis.data$lis_1) 
genre.cat <- as.data.frame(lis.data[c(1:20), c("genre")])
cat <- c(1, 2, 3, 4, 2, 3, 4, 2, 3, 3, 4, 2, 4, 1, 2, 1, 2, 1, 4, 1)
cat <- factor(cat, labels = c("Rock.Pop", "Art", "Folk", "Afro.Pop"))
genre.cat$cat <- cat
names(genre.cat)[1] <- "genre"
plot.data <- merge(lis.data, cent.inf.pos, by = c("classmem", "genre"))
plot.data <- merge(plot.data, genre.cat, by = c("genre"))
plot.data$classmem <- factor(plot.data$classmem, 
                     labels = c("Anything but Country", "Anything but Heavy Metal", 
                                "Art-Popular", "Omnivore-Univore", "Contempo-Trad"))
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
require("foreign")
write.dta(plot.data, "plot.data.dta")

plot.data <- read.dta("plot.data.dta")

require("ggrepel")
windows(height = 8, width = 9)
p <- ggplot(plot.data, aes(y = omnibeta, x = value, color = classmem, fill = classmem))
p <- p + geom_point(size = 1)
p <- p + geom_smooth(method = "lm", alpha = 0.08)
p <- p + theme_classic() + theme(legend.position = "none")
p





cent.neg1 <- centrality(pcor1.neg)
cent.neg2 <- centrality(pcor2.neg)
cent.neg3 <- centrality(pcor3.neg)
cent.neg4 <- centrality(pcor4.neg)
cent.neg5 <- centrality(pcor5.neg)
cent.neg1 <- scale(cent.neg1$OutDegree)
cent.neg2 <- scale(cent.neg2$OutDegree)
cent.neg3 <- scale(cent.neg3$OutDegree)
cent.neg4 <- scale(cent.neg4$OutDegree)
cent.neg5 <- scale(cent.neg5$OutDegree)
genre <- rownames(cor1)
cent.inf.neg <- cbind.data.frame(genre, cent.neg1, cent.neg2, cent.neg3, cent.neg4, cent.neg5)
cent.inf.neg <- melt(cent.inf.neg, by = "genre")
cent.inf.neg <- cent.inf.neg[, c("genre", "value")]

cent.inf.neg$classmem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
cent.inf.neg$classmem <- factor(cent.inf.neg$classmem, 
                                labels = c("Anything but Country", "Anything but Heavy Metal", 
                                           "Contempo-Trad", "Omnivore-Univore", "High-Low"))
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta")
lis.data <- read_dta("class-genre-numlis-lis.dta")
lis.data <- subset(lis.data, numgenrescat == 5)

require("ggplot2")
windows(height = 14, width = 14)
ggplot(cent.inf.neg, aes(classmem, genre)) +
  geom_tile(aes(fill = value), color = "white") +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("  ") +
  xlab(" ") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = " ")

colnames(cent.inf.pos[,6]) <- "id"
cent.inf.pos <- melt(cent.inf.pos, id = "id")
class.mem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
plot.bet <- data.frame(plot.bet[c(1:100), ], class.mem)

windows(height = 18, width = 14)
p <- ggplot(plot1, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class1.png", type = "png")
dev.off()

plot <- centralityTable(cor2)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class2.png", type = "png")
dev.off()

plot <- centralityTable(cor3)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class3.png", type = "png")
dev.off()

plot <- centralityTable(cor4)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class4.png", type = "png")
dev.off()

plot <- centralityTable(cor5)
windows(height = 18, width = 14)
p <- ggplot(plot, aes(x = factor(node), y = value, fill = factor(variable)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = " ") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + geom_hline(yintercept = 0)
p <- p + theme(axis.text.y = element_text(size = 18), legend.text = element_text(size = 14))
p
setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class5.png", type = "png")
dev.off()







