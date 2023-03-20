############ correlational class analysis ##############################
require("corclass")
require("foreign")

cca.data <- read.dta("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta/taste.dta")
cca.res <- cca(cca.data[, c(2:21)], filter.value = 0.01) #Five Class Solution
cca.data <- data.frame(cca.data, cca.res$membership)
names(cca.data)[ncol(cca.data)] <- c("classmem")
write.dta(cca.data, "schematic-class.dta")

require("haven")
require("qgraph")
############ getting and setting up data ##############################
netcor.data <- read_dta("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/dta/schematic-class.dta")

############ meta genres ##############################################
f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)
gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")
genre.names <- colnames(netcor.data[,c(2:21)])

############# correlation plots #######################################
for (i in 1:5) {
  x <- subset(netcor.data, classmem == i)[,c(2:21)]
  windows(height = 14, width = 14)
  qgraph(cor_auto(x), layout = "spring", 
         labels = genre.names, repulsion = 1.5,
         graph = "cor", sampleSize = nrow(x), 
         alpha = 0.01, minimum = 0.05, threshold = "sig",
         groups = gen.groups, palette = "colorblind", 
         legend = FALSE, esize = 15, 
         posCol = "darkgray", borders = FALSE)
  savePlot(filename = paste("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png/taste-corrnet-class", 
                            i, sep = ""), type = "png")
  dev.off()
}

############# partial correlation plots #######################################
for (i in 1:5) {
  x <- subset(netcor.data, classmem == i)[,c(2:21)]
  windows(height = 14, width = 14)
  qgraph(cor_auto(x), layout = "spring", 
         labels = genre.names,
         graph = "pcor", sampleSize = nrow(x), 
         alpha = 0.01, minimum = 0.05, threshold = "sig",
         groups = gen.groups, palette = "colorblind", 
         legend = FALSE, esize = 15, 
         posCol = "darkgray", borders = FALSE)
  setwd("C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png")
  savePlot(filename = paste("taste-pcorrnet-class", 
                            i, sep = ""), type = "png")
  dev.off()
}

