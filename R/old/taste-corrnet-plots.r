require("haven")
require("qgraph")

############ getting and setting up data ################################################
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

windows(height = 14, width = 14)
qgraph(cor1, groups = gen.groups)


#correlation plots for class 1
windows(height = 14, width = 14)
cg1 <- qgraph(cor1, labels = c(rownames(cor1)), layout = "spring", repulsion = 2,  
       graph = "cor", threshold = "sig", sampleSize = nrow(netcor.data1), alpha = 0.05,
       groups = gen.groups, palette = "colorblind", legend = FALSE,
       shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
       edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
       cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class1", type = "png")
dev.off()

pcor1 <- estimateNetwork(netcor.data1, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
windows(height = 14, width = 14)
pcg1 <- qgraph(pcor1$graph, labels = c(rownames(cor1)), layout = cg1$layout,  
               groups = gen.groups, palette = "colorblind", legend = FALSE,
               shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
               edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
               cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class1", type = "png")
dev.off()

#correlation plot for class 2
windows(height = 14, width = 14)
cg2 <- qgraph(cor2, labels = c(rownames(cor1)), layout = "spring", repulsion = 2,
       graph = "cor", threshold = "sig", sampleSize = nrow(netcor.data2), alpha = 0.05,
       groups = gen.groups, palette = "colorblind", legend = FALSE,
       shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
       edge.labels = FALSE, edge.label.cex = 0.5, esize = 8, posCol = "gray", fade = FALSE,
       cut = 0.05, minimum = 0, maximum = 1)

setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class2", type = "png")
dev.off()
#partial correlation plot for class 2
pcor2 <- estimateNetwork(netcor.data2, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
windows(height = 14, width = 14)
pcg2 <- qgraph(pcor2$graph, labels = c(rownames(cor1)), layout = cg2$layout,  
               groups = gen.groups, palette = "colorblind", legend = FALSE,
               shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
               edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
               cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class2", type = "png")
dev.off()

#correlation plots for class 3
windows(height = 14, width = 14)
cg3 <- qgraph(cor3, labels = c(rownames(cor1)), layout = "spring", repulsion = 5,
       graph = "cor", threshold = "sig", sampleSize = nrow(netcor.data3), alpha = 0.05,
       groups = gen.groups, palette = "colorblind", legend = FALSE,
       shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
       edge.labels = FALSE, edge.label.cex = 0.5, esize = 8, posCol = "gray", fade = FALSE,
       cut = 0.05, minimum = 0, maximum = 1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class3", type = "png")
dev.off()
#partial correlation plot for class 3
pcor3 <- estimateNetwork(netcor.data3, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
windows(height = 14, width = 14)
pcg3 <- qgraph(pcor2$graph, labels = c(rownames(cor1)), layout = cg3$layout,  
               groups = gen.groups, palette = "colorblind", legend = FALSE,
               shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
               edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
               cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class3", type = "png")
dev.off()
#correlation plots for class 4
windows(height = 14, width = 14)
cg4 <- qgraph(cor4, labels = c(rownames(cor1)), layout = "spring", repulsion = 0.5,
       graph = "cor", threshold = "sig", sampleSize = nrow(netcor.data4), alpha = 0.05,
       groups = gen.groups, palette = "colorblind", legend = FALSE,
       shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
       edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
       cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class4", type = "png")
dev.off()
#partial correlation plot for class 4
pcor4 <- estimateNetwork(netcor.data4, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
windows(height = 14, width = 14)
pcg4 <- qgraph(pcor4$graph, labels = c(rownames(cor1)), layout = cg4$layout,  
               groups = gen.groups, palette = "colorblind", legend = FALSE,
               shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
               edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
               cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class4", type = "png")
dev.off()
#correlation plots for class 5
windows(height = 14, width = 14)
cg5 <- qgraph(cor5, labels = c(rownames(cor1)), layout = "spring", repulsion = 3,
       graph = "cor", threshold = "sig", sampleSize = nrow(netcor.data5), alpha = 0.05,
       groups = gen.groups, palette = "colorblind", legend = FALSE,
       shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
       edge.labels = FALSE, edge.label.cex = 0.5, esize = 3, posCol = "olivedrab4", fade = FALSE,
       cut = 0.05, maximum = 1)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-corrnet-class5", type = "png")
dev.off()
#partial correlation plot for class 5
pcor5 <- estimateNetwork(netcor.data5, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.25)
windows(height = 14, width = 14)
pcg5 <- qgraph(pcor4$graph, labels = c(rownames(cor1)), layout = cg5$layout,  
               groups = gen.groups, palette = "colorblind", legend = FALSE,
               shape = "ellipse", vsize = 4, node.width = 3, node.height = 0.5, label.prop = 0.85,  borders = FALSE,
               edge.labels = FALSE, edge.label.cex = 0.5, esize = 10, posCol = "gray", fade = FALSE,
               cut = 0.05, minimum = 0, maximum = 1, details = TRUE)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "taste-pcorrnet-class4", type = "png")
dev.off()
############ genre centrality scores #########################

plot1 <- centralityTable(cor1)
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
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
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
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
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
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
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
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
savePlot(filename = "genre-centrality-corrnet-class5.png", type = "png")
dev.off()







