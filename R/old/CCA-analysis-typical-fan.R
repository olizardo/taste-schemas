require("igraph")
require("corclass")
require("Hmisc")
require("foreign")
require("qgraph")

setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/dta")
cca.data <- stata.get("typical-fan.dta")
cca.data <- na.omit(cca.data)

cca.res <- cca(cca.data[, c(32:46)], filter.value = 0.05, zero.action = "drop")

plot(cca.res, 1)

cca.data <- data.frame(cca.data, cca.res$membership)
colnames(cca.data)[22] <- "classmem"
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/dta")
write.dta(cca.data, "schematic-class-typical.dta")

setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
cor1 <- cca.res$modules[[1]]$cormat
cor2 <- cca.res$modules[[2]]$cormat
cor3 <- cca.res$modules[[3]]$cormat


diag(cor1) <- 0
diag(cor2) <- 0
diag(cor3) <- 0


require(qgraph)

labnames <- c("female", "male", "white", "black", "hispanic", "asian", "college", "high.school", "young", "middle.aged", "older", "lower", "working", "middle.class", "upper")

rownames(cor1) <- labnames
colnames(cor1) <- labnames
rownames(cor2) <- labnames
colnames(cor2) <- labnames
rownames(cor3) <- labnames
colnames(cor3) <- labnames

p1 <- qgraph(cor1, layout = "spring", minimum = 0.1, border = F, vsize = 8)
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "schematic-class1-corrnet.png", type = "png")	
p2 <- qgraph(cor2, layout = "spring", minimum = 0.1, border = F, vsize = 8)
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "schematic-class2-corrnet.png", type = "png")	
p3 <- qgraph(cor3, layout = "spring", minimum = 0.1, border = F, vsize = 8)
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "schematic-class3-corrnet.png", type = "png")	
p4 <- qgraph(cor4, layout = "spring", minimum = 0.2, border = F, vsize = 8)
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "schematic-class4-corrnet.png", type = "png")	
p5 <- qgraph(cor5, layout = "spring", minimum = 0.2, border = F, vsize = 8)
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "schematic-class5-corrnet.png", type = "png")	


cent1 <- centrality(cor1)
cent2 <- centrality(cor2)
cent3 <- centrality(cor3)
cent4 <- centrality(cor4)
cent5 <- centrality(cor5)

genre <- colnames(cor1)
genre.bet.dat <- data.frame(genre, cent1$Betweenness, cent2$Betweenness, cent3$Betweenness, cent4$Betweenness, cent5$Betweenness)
genre.clos.dat <- data.frame(genre, cent1$Closeness, cent2$Closeness, cent3$Closeness, cent4$Closeness, cent5$Closeness)

library("reshape")
plot.bet.dat <- melt(genre.bet.dat, id = "genre")
class.mem <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
plot.bet.dat <- data.frame(plot.bet.dat, class.mem)

p <- ggplot(plot.bet.dat, aes(x = factor(genre), y = value, fill = factor(class.mem)))
p <- p + geom_bar(stat = "identity", width = .5, position = "dodge")
p <- p + theme_classic() + theme(legend.position = "bottom")
p <- p + scale_fill_discrete(name = "Schematic Class") 
p <- p + labs(x = "", y = "")  + coord_flip()
p <- p + facet_wrap( ~ class.mem, ncol = 5)
p
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/png")
savePlot(filename = "genre-betweenness-by-schematic-class.png", type = "png")	




