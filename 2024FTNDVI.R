#Setup
library(carData)
library(car)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(lme4)
library(afex)
library(extrafont)
library(patchwork)

#Import
ndvi <- read.csv("2024FTPhenologyAndNDVI.csv")
ndvi$Trt <- as.factor(ndvi$Trt)
ndvi$Block <- as.factor(ndvi$Block)
str(ndvi)

## 26DAS ##
ndvipp.aov <- aov(X26DAS ~ Trt + Block + Trt:Block, data = ndvi)
Anova(ndvipp.aov, type="III") #Interaction NS
ndvipp.aov1 <- update(ndvipp.aov,~.-Trt:Block)
Anova(ndvipp.aov1, type="II") #Block NS
ndvipp.aov2 <- update(ndvipp.aov1,~.-Block)
Anova(ndvipp.aov2, type="II") #Trt is significant

par(mfrow=c(2,2))
plot(ndvipp.aov2) #Good with small tail

TukeyHSD(ndvipp.aov2) #NoAdd diff from Min and Alg, which are not different from each other

pp26.df <- data.frame(y.position=c(0.67,0.677,0.69),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("0.0189*","2.98e-3**","0.720"))

ndvi.pp26.plot <- ggboxplot(ndvi, x = "Trt", y = "X26DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(pp26.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="NDVI at 26DAS") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
ndvi.pp26.plot



## 35DAS ##
ndvipp.aov <- aov(X35DAS ~ Trt + Block + Trt:Block, data = ndvi)
Anova(ndvipp.aov, type="III") #Interaction NS
ndvipp.aov1 <- update(ndvipp.aov,~.-Trt:Block)
Anova(ndvipp.aov1, type="II") #Block NS
ndvipp.aov2 <- update(ndvipp.aov1,~.-Block)
Anova(ndvipp.aov2, type="II") #Trt is significant

par(mfrow=c(2,2))
plot(ndvipp.aov2) #Fine

TukeyHSD(ndvipp.aov2) #NoAdd diff from Min and Alg, which are not different from each other

pp35.df <- data.frame(y.position=c(0.682,0.689,0.7),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("1e-7***","3e-7***","0.868"))

ndvi.pp35.plot <- ggboxplot(ndvi, x = "Trt", y = "X35DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(pp35.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="NDVI at 35DAS") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
ndvi.pp35.plot



## 40DAS ##
ndvipp.aov <- aov(X40DAS ~ Trt + Block + Trt:Block, data = ndvi)
Anova(ndvipp.aov, type="III") #Interaction NS
ndvipp.aov1 <- update(ndvipp.aov,~.-Trt:Block)
Anova(ndvipp.aov1, type="II") #Block NS
ndvipp.aov2 <- update(ndvipp.aov1,~.-Block)
Anova(ndvipp.aov2, type="II") #Trt is significant

par(mfrow=c(2,2))
plot(ndvipp.aov2) #Fine

TukeyHSD(ndvipp.aov2)

pp40.df <- data.frame(y.position=c(0.645,0.6525,0.665),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("8e-7***","1e-7***","0.401"))

ndvi.pp40.plot <- ggboxplot(ndvi, x = "Trt", y = "X40DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(pp40.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="NDVI at 40DAS") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
ndvi.pp40.plot



## 56DAS ##
ndvipp56.aov <- aov(X56DAS ~ Trt + Block + Trt:Block, data = ndvi)
Anova(ndvipp56.aov, type="III") #Interaction NS
ndvipp56.aov1 <- update(ndvipp56.aov,~.-Trt:Block)
Anova(ndvipp56.aov1,type="II") #Trt and block sig, use mixed model

ndvipp56.mixed <- mixed(X56DAS ~ Trt + (1|Block), data = ndvi)
ndvipp56.mixed #Sig
ndvipp56.lme1 <- lmer(X56DAS ~ Trt + (1|Block), data = ndvi)
summary(ndvipp56.lme1) #Alg as contrast
ndvipp56.lme2 <- lmer(X56DAS ~ Trt + (1|Block), data = ndvi, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(ndvipp56.lme2) #Min as contrast

plot(ndvipp56.lme1) #Check resid vs fitted
qqnorm(resid(ndvipp56.lme1))
qqline(resid(ndvipp56.lme1)) #Check QQs
##All good

pp56.df <- data.frame(y.position=c(0.637,0.642,0.649),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("3.51e-5***","1.37e-8***","1.78e-3**"))


ndvi.pp56.plot <- ggboxplot(ndvi, x = "Trt", y = "X56DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(pp56.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="NDVI at 56DAS") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
ndvi.pp56.plot

## Figure Creation ##
figS7 <- (ndvi.pp26.plot | ndvi.pp35.plot ) / (ndvi.pp40.plot | ndvi.pp56.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
figS7
jpeg("figureS7.jpg", width = 180, height = 100, units = "mm", res = 300)
figS7
dev.off()



