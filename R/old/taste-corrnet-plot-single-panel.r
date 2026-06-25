#load r objects
load("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/R/CCA.RData")

#load packages
require(qgraph)
 
clust <- clustOnnela(cor1)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 16, width = 16)
p1 <- qgraph(cor1,
	sampleSize = 504,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE, legend = FALSE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)

clust <- clustOnnela(cor2)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 16, width = 16)
p2 <- qgraph(cor2,
	sampleSize = 341,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE, legend = FALSE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)

clust <- clustOnnela(cor3)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 16, width = 16)
p3 <- qgraph(cor3,
	sampleSize = 525,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE, legend = FALSE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)

clust <- clustOnnela(cor4)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 16, width = 16)
p4 <- qgraph(cor4,
	sampleSize = 408,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE, legend = FALSE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)
	
clust <- clustOnnela(cor5)
node.size <- clust[, 1]
node.size <- node.size * 30
dev.off()
dev.new(height = 16, width = 16)
p5 <- qgraph(cor5,
	sampleSize = 322,
	graph = "cor",
	minimum = 0.15, 
	overlay = TRUE, 	groups = gen.groups, 
	nodeNames = genres.lab,
	layout = "spring", repulsion = 1.5,
	pastel = TRUE, legend = FALSE,
	vsize = node.size, borders = FALSE, shape = "circle", label.prop = 0.75, 
	curveAll = FALSE, edge.labels = TRUE, edge.label.cex = 0.45, esize = 8,
	title = " "
	)

library(gridExtra)

p1 <- grob(p1)
p2 <- grob(p2)
grid.arrange(p1, p2, ncol = 2)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "taste-corrnet-single-panel.png", type = "png")	
