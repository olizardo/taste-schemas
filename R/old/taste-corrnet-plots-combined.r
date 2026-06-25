#load r objects
load("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/R/CCA.RData")

#load packages
require(qgraph)
require(corrplot)
require(PerformanceAnalytics)
 
f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)

gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")

par(mfrow = c(1, 1))
clust <- clustOnnela(cor1)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 12, width = 16)
qgraph(cor1,
	sampleSize = 504,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-class1.png", type = "png")	

clust <- clustOnnela(cor2)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 12, width = 16)
qgraph(cor2,
	sampleSize = 341,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-class2.png", type = "png")	

clust <- clustOnnela(cor3)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 12, width = 16)
p3 <- qgraph(cor3,
	sampleSize = 525,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-class3.png", type = "png")	

clust <- clustOnnela(cor4)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 12, width = 16)
p4 <- qgraph(cor4,
	sampleSize = 408,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-class4.png", type = "png")	
	
clust <- clustOnnela(cor5)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 12, width = 16)
p5 <- qgraph(cor5,
	sampleSize = 322,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-class5.png", type = "png")	

diag(cor1) <- 0
diag(cor2) <- 0
diag(cor3) <- 0
diag(cor4) <- 0
diag(cor5) <- 0
col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7","#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")) 

dev.off()
dev.new(height = 20, width = 20)
cortab1 <- corrplot(cor1, method = "square", order = "hclust", addrect = 3, col=col(50), tl.cex = 1, cl.lim = c(-0.5, 0.75))
cortab2 <- corrplot(cor2, method = "square", order = "hclust", addrect = 3, col=col(50), tl.cex = 1, cl.lim = c(-0.5, 0.75))
cortab3 <- corrplot(cor3, method = "square", order = "hclust", addrect = 3, col=col(50), tl.cex = 1, cl.lim = c(-0.5, 0.75))
cortab4 <- corrplot(cor4, method = "square", order = "hclust", addrect = 3, col=col(50), tl.cex = 1, cl.lim = c(-0.5, 0.75))
cortab5 <- corrplot(cor5, method = "square", order = "hclust", addrect = 3, col=col(50), tl.cex = 1, cl.lim = c(-0.5, 0.75))

dev.off()
dev.new(height = 18, width = 20)
require(gridExtra)
grid.arrange(p1, cortab1, nrow = 1)
