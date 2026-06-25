require("igraph")
require("qgraph")
require("Hmisc")
setwd("C:/Users/olizardo/Google Drive/lizardo-skiles-taste-schemas/dta")
corr.data <- stata.get("typical-fan-correlation-network.dta")
cca.data <- stata.get("taste.dta")

genre.groups <- list(c(1:15), c(16:30), c(31:45), c(46:60), c(61:75), c(76:90), c(91:105), c(106:120), c(121:135), c(136:150), c(151:165), c(166:180), c(181:195), c(196:210), c(211:225), c(226:240),c(241:255), c(256:270), c(271:285), c(286:300))

dem.groups <- list(c(seq(1, 300, by = 15)), c(seq(2, 300, by = 15)), c(seq(3, 300, by = 15)), c(seq(4, 300, by = 15)), c(seq(5, 300, by = 15)), c(seq(6, 300, by = 15)), c(seq(7, 300, by = 15)), c(seq(8, 300, by = 15)), c(seq(9, 300, by = 15)), c(seq(10, 300, by = 15)), c(seq(11, 300, by = 15)), c(seq(12, 300, by = 15)), c(seq(13, 300, by = 15)), c(seq(14, 300, by = 15)), c(seq(15, 300, by = 15)))

names(dem.groups) <- c("Woman", "Man", "White", "Black", "Hispanic", "Asian", "College", "High_School", "Young", "Middle_Aged", "Older", "Lower_Class", "Working_Class", "Middle_Class", "Upper_Class")

corr.data.class <- corr.data[, c(seq(12, 300, by = 15), seq(13, 300, by = 15), seq(14, 300, by = 15), seq(15, 300, by = 15))]
corr.data.race <- corr.data[, c(seq(3, 300, by = 15), seq(4, 300, by = 15), seq(5, 300, by = 15), seq(6, 300, by = 15))]
corr.data.gender <- corr.data[, c(seq(1, 300, by = 15), seq(2, 300, by = 15))]
corr.data.age <- corr.data[, c(seq(9, 300, by = 15), seq(10, 300, by = 15),  seq(11, 300, by = 15))]
corr.data.educ <- corr.data[, c(seq(7, 300, by = 15), seq(8, 300, by = 15))]

genre.cols <- c("Classical","Opera","Jazz","Musicals","Easy","Bigband","Oldies","Country","Bluegrass","Folk","Gospel","Latin","HipHop","Blues","Reggae","Pop","Rock","Alternative","Dance","Metal","Classical","Opera","Jazz","Musicals","Easy","Bigband","Oldies","Country","Bluegrass","Folk","Gospel","Latin","HipHop","Blues","Reggae","Pop","Rock","Alternative","Dance","Metal","Classical","Opera","Jazz","Musicals","Easy","Bigband","Oldies","Country","Bluegrass","Folk","Gospel","Latin","HipHop","Blues","Reggae","Pop","Rock","Alternative","Dance","Metal","Classical","Opera","Jazz","Musicals","Easy","Bigband","Oldies","Country","Bluegrass","Folk","Gospel","Latin","HipHop","Blues","Reggae","Pop","Rock","Alternative","Dance","Metal")

gender.cats <- list(c(1:20), c(21:40))
age.cats <- list(c(1:20), c(21:40), c(41:60))
race.cats <- list(c(1:20), c(21:40), c(41:60), c(61:80))
class.cats <- list(c(1:20), c(21:40), c(41:60), c(61:80))
educ.cats <- list(c(1:20), c(21:40))

colnames(corr.data.gender) <- genre.cols[1:40]
colnames(corr.data.race) <- genre.cols[1:80]
colnames(corr.data.educ) <- genre.cols[1:40]
colnames(corr.data.age) <- genre.cols[1:60]
colnames(corr.data.class) <- genre.cols[1:80]

names(educ.cats) <- c("College", "High_School")
names(age.cats) <- c("Young", "Middle_Aged", "Older")
names(race.cats) <- c("White", "Black", "Hispanic", "Asian")
names(gender.cats) <- c("Woman", "Man")
names(class.cats) <- c("Lower_Class", "Working_Class", "Middle_Class", "Upper_Class")

Q <- qgraph(cor(corr.data.race), groups = race.cats, minimum = 0.1, vsize = 4, layout = "spring", graph = "concentration", borders = F)
Q <- qgraph(cor(corr.data.gender), groups = gender.cats, minimum = 0.05, vsize = 4, layout = "spring", graph = "concentration", borders = F)
Q <- qgraph(cor(corr.data.age), groups = age.cats, minimum = 0.1, vsize = 4, layout = "spring", graph = "concentration", borders = F)
Q <- qgraph(cor(corr.data.class), groups = class.cats, minimum = 0.1, vsize = 4, layout = "spring", graph = "concentration", borders = F)
Q <- qgraph(cor(corr.data.educ), groups = educ.cats, minimum = 0.1, vsize = 4, layout = "spring", graph = "concentration", borders = F)



