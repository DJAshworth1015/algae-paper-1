#Setup
library(carData)
library(car)
library(FSA)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(extrafont)
library(patchwork)

#Script intended to be used with the RProjects format
#Therefore no working directory is set

#Import
dth <- read.csv("2024FTPhenologyAndNDVI.csv")
dth$Plot <- as.factor(dth$Plot)
dth$Trt <- as.factor(dth$Trt)
dth$Block <- as.factor(dth$Block)
str(dth)

## DTFL ##
dtfl.aov <- aov(DTFL ~ Trt + Block + Trt:Block, data=dth)
summary(dtfl.aov) #Int NS
dtfl.aov1 <- update(dtfl.aov,~.-Trt:Block)
summary(dtfl.aov1) #Block NS
dtfl.aov2 <- update(dtfl.aov1,~.-Block)
summary(dtfl.aov2) #Trt Sig

par(mfrow=c(2,2))
plot(dtfl.aov2) #Not suitable

dtfl.glm <- glm(DTFL ~ Trt + Block + Trt:Block, family = poisson(link="log"), data = dth)
summary(dtfl.glm) #Ints NS
dtfl.glm1 <- update(dtfl.glm,~.-Trt:Block)
summary(dtfl.glm1) #Blocks NS
dtfl.glm2 <- update(dtfl.glm1,~.-Block)
summary(dtfl.glm2) #Trts NS
plot(dtfl.glm2) #Still not suitable

kruskal.test(DTFL ~ Trt, data = dth) #Sig
dunnTest(DTFL ~ Trt, data=dth, method = "bh")

dtfl.df <- data.frame(y.position=c(63.4,63.7,64.2),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("9.90e-3**","0.0557","0.393"))


dtfl.plot <- ggboxplot(dth, x = "Trt", y = "DTFL", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(dtfl.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Days to Flag Leaf") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
dtfl.plot



## DTH ##
dth.glm <- glm(DTH ~ Trt + Block + Trt:Block, family = poisson(link="log"), data = dth)
summary(dth.glm) #Ints NS
dth.glm1 <- update(dth.glm,~.-Trt:Block)
summary(dth.glm1) #Blocks NS
dth.glm2 <- update(dth.glm1,~.-Block)
summary(dth.glm2) #Trts NS
par(mfrow=c(2,2))
plot(dth.glm2) #Unsuitable

kruskal.test(DTH ~ Trt, data = dth) #Sig
dunnTest(DTH ~ Trt, data=dth, method = "bh")

dth.df <- data.frame(y.position=c(65.2,65.35,65.6),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("0.0161*","4.56e-5***","0.0758"))

dth.plot <- ggboxplot(dth, x = "Trt", y = "DTH", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(dth.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="Days to Heading") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
dth.plot

## Figure Creation ##
figS6 <- (dtfl.plot | dth.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
figS6
jpeg("figureS6.jpg", width = 180, height = 50, units = "mm", res = 300)
figS6
dev.off()


