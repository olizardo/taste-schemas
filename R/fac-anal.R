```{r factor analysis plot, fig,width = 10}
f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6, 11)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10)
gen.groups <- list(f1, f2, f3, f4)
efa.res <- factanal(taste.dat.new, factors = 4, rotation="promax")
fa <- qgraph(efa.res$loadings, 
             node.width = c(rep(1, 20), rep(1.5, 4)),
             label.scale = FALSE, label.cex = 0.8,
             groups = gen.groups, borders = FALSE, 
             edge.labels = TRUE, minimum = 0.1, 
             edge.label.cex = 0.8, edge.width = 0.8, theme = "colorblind",
             labels = c("Classical", "Opera", "Jazz", "Broadway", "Easy", "Big Band", "Classic.Rock", "Country", "Blueg.", "Folk", "Gospel", "Latin", "Rap", "Blues", "Reggae", "Pop", "Cont.Rock", "Alt.Rock", "Dance", "Metal"))
```
