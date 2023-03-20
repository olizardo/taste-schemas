require(Hmisc)
require(qgraph)
require(FactoMineR)
require(factoextra)
require(arm)
require(reshape)

#getting data for factor analysis of genres by listening choices
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/dta")
lis.data <- stata.get("lis.dta")
liscat.data <- stata.get("class-num-lis.dta")
cca.data <- stata.get("schematic-class.dta")
lis.data <- merge(lis.data, cca.data[, c(1, 22)], by = "id")
lis.data <- merge(lis.data, liscat.data[, c(1, 3)], by = "id")
demog.data <- stata.get("demog-corr.dta")
demog.data.full <- stata.get("demog-corr-full.dta")
demog.data.omni <- melt(demog.data[,c(1, 3, 5, 7, 9, 11)], id = "varlabs")
demog.data.uni <- melt(demog.data[,c(2, 4, 6, 8, 10, 11)], id = "varlabs")
demog.data.omni$variable <- factor(demog.data.omni$variable, 
labels = c("Anything (But Country)", "Anything (But Hvy Metal)", "Contempo-Trad", "Omnivore-Univore", "High-Low"))
demog.data.uni$variable <- factor(demog.data.uni$variable, 
labels = c("Anything (But Country)", "Anything (But Hvy Metal)", "Contempo-Trad", "Omnivore-Univore", "High-Low"))

fav.omni <- demog.data.full[,c("favgen", "omniclass1", "omniclass2", "omniclass3", "omniclass4", "omniclass5")]
fav.omni <- aggregate(fav.omni, by = list(fav.omni$favgen), FUN = "mean", na.rm = T)
fav.omni <- fav.omni[,c(1, 3, 4, 5, 6, 7)]
fav.omni <- melt(fav.omni, id = "Group.1")

fav.uni <- demog.data.full[,c("favgen", "uniclass1", "uniclass2", "uniclass3", "uniclass4", "uniclass5")]
fav.uni <- aggregate(fav.uni, by = list(fav.uni$favgen), FUN = "mean", na.rm = T)
fav.uni <- fav.uni[,c(1, 3, 4, 5, 6, 7)]
fav.uni <- melt(fav.uni, id = "Group.1")

dev.off()
dev.new(height = 12, width = 16)
p <- ggplot(fav.uni, aes(x = value, y = Group.1, group = variable, color = variable))
p <- p +  geom_point(size = 3.5)
p <- p + geom_segment(aes(x = value, y = Group.1, xend = 0, yend = Group.1), size = 1.5)
p <- p + facet_wrap( ~ variable, ncol = 5)
p <- p + labs(x = "", y = "") 
p <- p + theme_classic() + geom_vline(xintercept = 0)
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12), strip.text.x = element_text(size = 10))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "fav-uni.png", type = "png")	

dev.off()
dev.new(height = 12, width = 16)
p <- ggplot(fav.omni, aes(x = value, y = Group.1, group = variable, color = variable))
p <- p +  geom_point(size = 3.5)
p <- p + geom_segment(aes(x = value, y = Group.1, xend = 0, yend = Group.1), size = 1.5)
p <- p + facet_wrap( ~ variable, ncol = 5)
p <- p + labs(x = "", y = "") 
p <- p + theme_classic() + geom_vline(xintercept = 0)
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12), strip.text.x = element_text(size = 10))
p
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "fav-omni.png", type = "png")	


f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)

gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")

class1.lis <- subset(lis.data, classmem==1)
class2.lis <- subset(lis.data, classmem==2)
class3.lis <- subset(lis.data, classmem==3)
class4.lis <- subset(lis.data, classmem==4)
class5.lis <- subset(lis.data, classmem==5)

class1.liscat <- subset(mca.data, classmem==1)
class2.liscat <- subset(mca.data, classmem==2)
class3.liscat <- subset(mca.data, classmem==3)
class4.liscat <- subset(mca.data, classmem==4)
class5.liscat <- subset(mca.data, classmem==5)

#factor analysis of musical genres by behavioral choices
dev.off()
dev.new(height = 12, width = 16)
qgraph.efa(lis.data[,c(2:21)], 4, groups = gen.groups,
	rotation = "promax",
	minimum = 0.15, cut = 0.4,
	layout = "circle",
	residuals = FALSE, 
	edge.labels = T, edge.label.cex = 0.65, esize = 8,
	vsize = c(5, 8), borders = F, shape = c("circle", "rectangle"), label.prop = 0.75,
	vTrans = 120
	)
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "genre-behav-typology.png", type = "png")	

require(factoextra)
require(FactoMineR)

res.pca1 <- PCA(class1.lis[,c(2:21)], graph = FALSE)
res.pca2 <- PCA(class2.lis[,c(2:21)], graph = FALSE)
res.pca3 <- PCA(class3.lis[,c(2:21)], graph = FALSE)
res.pca4 <- PCA(class4.lis[,c(2:21)], graph = FALSE)
res.pca5 <- PCA(class5.lis[,c(2:21)], graph = FALSE)

dev.off()
dev.new(height = 14, width = 14)
fviz_pca_biplot(
	res.pca1, habillage = class1.lis$numgenrescat,
	addEllipses = T, ellipse.level = 0.75,
	axes = c(1,2), repel = T,
	pointsize = 1.5, labelsize = 3.25,
	col.var = "black", label = "var"
	) + 
	theme_minimal() + ggtitle("Anything (But Country)")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "consumption-space-PCA-biplot-class1-dim12.png", type = "png")

dev.off()
dev.new(height = 14, width = 14)
fviz_pca_biplot(
	res.pca2, habillage = class2.lis$numgenrescat,
	addEllipses = T, ellipse.level = 0.75,
	axes = c(1,2), repel = T,
	pointsize = 1.5, labelsize = 3.25,
	col.var = "black", label = "var"
	) + 
	theme_minimal() + ggtitle("Anything (But Heavy Metal)")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "consumption-space-PCA-biplot-class2-dim12.png", type = "png")

dev.off()
dev.new(height = 14, width = 14)
fviz_pca_biplot(
	res.pca3, habillage = class3.lis$numgenrescat,
	addEllipses = T, ellipse.level = 0.75,
	axes = c(1,2), repel = T,
	pointsize = 1.5, labelsize = 3.25,
	col.var = "black", label = "var"
	) + 
	theme_minimal() + ggtitle("Contempo-Trad")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "consumption-space-PCA-biplot-class3-dim12.png", type = "png")

dev.off()
dev.new(height = 14, width = 14)
fviz_pca_biplot(
	res.pca4, habillage = class4.lis$numgenrescat,
	addEllipses = T, ellipse.level = 0.75,
	axes = c(1,2), repel = T,
	pointsize = 1.5, labelsize = 3.25,
	col.var = "black", label = "var"
	) + 
	theme_minimal() + ggtitle("Omnivore-Univore")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "consumption-space-PCA-biplot-class4-dim12.png", type = "png")

dev.off()
dev.new(height = 14, width = 14)
fviz_pca_biplot(
	res.pca5, habillage = class5.lis$numgenrescat,
	addEllipses = T, ellipse.level = 0.75,
	axes = c(1,2), repel = T,
	pointsize = 1.5, labelsize = 3.25,
	col.var = "black", label = "var"
	) + 
	theme_minimal() + ggtitle("High-Low")
setwd("C:/Users/olizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-skiles-taste-schemas/png")
savePlot(filename = "consumption-space-PCA-biplot-class5-dim12.png", type = "png")