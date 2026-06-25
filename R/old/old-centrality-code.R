#genre centrality plots
cent1 <- centrality(cor1)
cent2 <- centrality(cor2)
cent3 <- centrality(cor3)
cent4 <- centrality(cor4)
cent5 <- centrality(cor5)

require("reshape")
genre <- rownames(cor1)
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
savePlot(filename = "genre-betweenness-corrnet.png", type = "png")
dev.off()

windows(height = 14, width = 18)
p <- ggplot(plot.clos, aes(x = factor(genre), y = value, fill = factor(class.mem)))
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
savePlot(filename = "genre-closeness-corrnet.png", type = "png")
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
savePlot(filename = "genre-strength-corrnet.png", type = "png")
dev.off()