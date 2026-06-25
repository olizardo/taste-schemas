```{r validation plot 2 data prep, message=FALSE, warning=FALSE}
lis.all <- lis.dat
lis.all <- lis.all %>%
  merge(taste.dat[, c(1, length(taste.dat))], by = "id") %>%
  select(-c("id", "omni")) %>%
  summarise_all(mean)
lis.all$dummyid <- 1
lis.all <- melt(lis.all, id.vars = "dummyid")
lis.all <- lis.all[,2:3]
lis.all
lis.omni <- filter(lis.dat, omni >= 8)
lis.omni <- lis.omni %>%
  merge(taste.dat[, c(1, length(taste.dat))], by = "id") %>%
  select(-c("id", "omni")) %>%
  group_by(classmem) %>%
  summarise_all(mean)
lis.omni
lis.omni <- melt(lis.omni, id.vars = "classmem")
lis.omni
lis.diff <- merge(lis.all, lis.omni, by = "variable")
names(lis.diff) <- c("genre", "lis.all", "classmem", "lis.omni")
lis.diff <- lis.diff %>%
  mutate(diff = lis.omni - lis.all) %>%
  mutate(classmem.f = factor(classmem, labels = 
                               c("Anything but Country", "Anything but Heavy Metal", 
                                 "Contemporary-Traditional", "Omnivore-Univore", "Contempo-Folksy-Easy"))) %>%
  select(-"classmem")
lis.diff$genre.r <- factor(lis.diff$genre, levels = unique(lis.diff$genre[order(lis.diff$lis.all)]), ordered = TRUE)
```

```{r validation plot 3 and 4 data prep}
dat <- merge(lis.diff[,c("genre", "diff","classmem.f")], corr.dat[,c("genre", "exp.inf","classmem.f")], by = c("genre", "classmem.f"))
dat.sd <- group_by(dat, genre)
dat.sd <- summarise(dat.sd, sd.genre = sd(diff))
dat <- merge(dat, dat.sd, by = "genre")
dat$sd.cut <- quantcut(dat$sd.genre, q = 4)
dat$sd.cut <- factor(dat$sd.cut, labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))
```

```{r schema validation plot 2 (dropline)}
windows(height = 12, width = 16)
p <- ggplot(lis.diff, aes(x = classmem.f, y = diff, color = classmem.f))
p <- p + geom_point(size = 3.25) + coord_flip() 
p <- p + geom_segment(aes(x = classmem.f, y = diff, xend = classmem.f, yend = 0))
p <- p + geom_hline(aes(yintercept = 0))
p <- p + facet_wrap( ~ genre.r, nrow = 5)
p <- p + theme_minimal() + labs(x = "", y = "", color = "")
p <- p + theme(legend.position = "none", axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
p
savePlot(filename = "C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png/schema-validation2.png"
         , type = "png")
```

```{r schema validation plot 3 (scatter)}
set.seed(25)
windows(height = 12, width = 7)
p <- ggplot(data = dat, mapping = aes(x = exp.inf, y = diff))
p <- p + geom_smooth(method = "lm", alpha = 0.15)
p <- p + geom_text_repel(aes(label = genre, color = classmem.f), size = 2, segment.alpha = 0)
p <- p + theme_classic() + geom_hline(yintercept = 0, color = "gray") + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(x = " ", y = "")
p.val3 <- p + theme(legend.position = "none")
p.val3
savePlot(filename = "C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png/schema-validation3.png"
         , type = "png")
```

```{r schema validation plot 4 (scatter)}
windows(height = 4, width = 10)
p <- ggplot(data = dat, mapping = aes(x = exp.inf, y = diff))
p <- p + geom_smooth(method = "lm", alpha = 0.15)
p <- p + geom_text_repel(aes(label = genre, color = classmem.f), size = 2, segment.alpha = 0)
p <- p + theme_classic() + geom_hline(yintercept = 0, color = "gray") + geom_vline(xintercept = 0, color = "gray")
p <- p + labs(x = " ", y = "")
p <- p + theme(legend.position = "none")
p.val4 <- p + facet_wrap( ~ sd.cut, nrow = 2)
p.val4
savePlot(filename = "C:/Users/Omar Lizardo/Google Drive/WORK/IN PROGRESS/CULTURE/lizardo-taste-schemas/png/schema-validation4.png"
         , type = "png")
```

