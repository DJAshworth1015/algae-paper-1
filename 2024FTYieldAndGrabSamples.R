#Setup
library(carData)
library(car)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(lme4)
library(afex)
library(extrafont)
library(patchwork)

#Input
yield <- read.csv("2024FTYieldandGrainN.csv")
yield$Trt <- as.factor(yield$Trt)
yield$Block <- as.factor(yield$Block)
str(yield)

tn <- read.csv("2024FTGrabSampleTillerNumber.csv")
tn$Plot <- as.factor(tn$Plot)
tn$Trt <- as.factor(tn$Trt)
tn$Block <- as.factor(tn$Block)
tn$Width <- as.factor(tn$Width)
tn$Depth <- as.factor(tn$Depth)
str(tn)
tn_long <- gather(tn, Plant, TN, P1:P57, factor_key=TRUE)
str(tn_long)

gpe <- read.csv("2024FTGrabSampleGPE.csv")
gpe$Plot <- as.factor(gpe$Plot)
gpe$Trt <- as.factor(gpe$Trt)
gpe$Block <- as.factor(gpe$Block)
gpe$Width <- as.factor(gpe$Width)
gpe$Depth <- as.factor(gpe$Depth)
str(gpe)
gpe_long <- gather(gpe, Ear, GPE, E1:E3, factor_key=TRUE)
str(gpe_long)

el <- read.csv("2024FTGrabSampleEarLength.csv")
el$Plot <- as.factor(el$Plot)
el$Trt <- as.factor(el$Trt)
el$Block <- as.factor(el$Block)
el$Width <- as.factor(el$Width)
el$Depth <- as.factor(el$Depth)
str(el)
el_long <- gather(el, Ear, EL, E1:E3, factor_key=TRUE)
str(el_long)

##### Yield #####
yield.aov <- aov(FreshYield ~ Trt + Block + Trt:Block, data=yield)
Anova(yield.aov, type ="III") #Int NS
yield.aov1 <- update(yield.aov,~.-Trt:Block)
Anova(yield.aov1,type="II") #Block NS
yield.aov2 <- update(yield.aov1,~.-Block)
Anova(yield.aov2, type="II") #Trt sig

par(mfrow=c(2,2))
plot(yield.aov2) #Good

TukeyHSD(yield.aov2) #Min and alg not diff

dy.df <- data.frame(y.position=c(8.35,8.7,9.2),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj.sig=c("1.57e-4***","1.1e-6***","0.125"))

dy.plot <- ggboxplot(yield, x = "Trt", y = "DryYield", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(dy.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Grain Yield, t/ha") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
dy.plot

ggsave("ftyield.eps", plot = dy.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### TN #####
tn.glm <- glm(TN ~ Trt + Block, data = tn_long, family = poisson)
tn.glm <- glm(TN ~ Trt + Block, data = tn_long, family = poisson,contrasts=list(Block = contr.treatment(n=3,base=2)))
summary(tn.glm) #Blocks NS, proceed with normal glm

tn.glm1 <- glm(TN ~ Trt, data = tn_long, family = poisson)
summary(tn.glm1) #Alg diff NoAdd
tn.glm2 <- glm(TN ~ Trt, data = tn_long, family = poisson,contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(tn.glm2) #Min diff NoAdd
tn.glm3 <- glm(TN ~ Trt, data = tn_long, family = poisson,contrasts=list(Trt = contr.treatment(n=3,base=3)))
summary(tn.glm3) #Confirms NoAdd diff both

par(mfrow=c(2,2))
plot(tn.glm1) #Acceptable for poisson

tn.df <- data.frame(y.position=c(6.3,6.6,7.1),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj.sig=c("1.15e-8***","2.29e-5***","0.0980"))

## Plot ##
tn.viol <- ggviolin(tn_long, x = "Trt", y = "TN", width = 0.1, trim = TRUE, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) +
  geom_violin(bw=0.38, aes(col=Trt, fill=Trt), show.legend=FALSE) +
  stat_summary(fun = mean, geom = "point", size=1, aes(color = Trt), show.legend = FALSE)+
  scale_color_manual(values = c("Alg"="#67A030", "NoAdd"="#BBBBBB", "Min"="#2250A2")) +
  stat_pvalue_manual(tn.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  labs(x=NULL, y="Tiller Number") + scale_y_continuous(breaks=1:6) +
  scale_x_discrete(limits = c("NoAdd","Alg","Min"), labels = c("No Additions","Algae Pellets", "NPK Granules")) +
  theme_clean() + theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                                        axis.title=element_text(size=rel(0.6)),
                                        axis.line.x = element_line(linewidth = rel(0.6)),
                                        axis.line.y = element_line(linewidth = rel(0.6)),
                                        text = element_text(family = "Times New Roman"),
                                        panel.border=element_blank(),
                                        plot.background = element_blank())
tn.viol

ggsave("fttillers.eps", plot = tn.viol,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


##### GPE #####
## Model ##
gpe.aov <- aov(GPE ~ Trt + Block, data = gpe_long)
Anova(gpe.aov, type="II") #Block NS, proceed with regular Anova
gpe.aov1 <- update(gpe.aov,~.-Block)
Anova(gpe.aov1,type="II") #Trt sig

par(mfrow=c(2,2))
plot(gpe.aov1) #Fine

TukeyHSD(gpe.aov1) #Alg diff from min

gpe.df <- data.frame(y.position=c(25.5,26.1,27),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj.sig=c("0.0924","0.847","0.0248*"))


## Plot ##
gpe.plot <- ggboxplot(gpe_long, x = "Trt", y = "GPE", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(gpe.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Grain Per Ear") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
gpe.plot

ggsave("ftgpe.eps", plot = gpe.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### EL #####
## Model ##
el.aov <- aov(EL ~ Trt + Block, data = el_long)
Anova(el.aov, type="II") #Block sig, use lme
par(mfrow=c(2,2))
plot(el.aov) #Good

el.mix <- mixed(EL ~ Trt + (1|Block), data = el_long)
el.mix #Trt sig
el.lme <- lmer(EL ~ Trt + (1|Block), data = el_long)
summary(el.lme) #Alg vs M and N
el.lme2 <- lmer(EL ~ Trt + (1|Block), data = el_long, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(el.lme2) #Min vs A and N
el.lme3 <- lmer(EL ~ Trt + (1|Block), data = el_long, contrasts=list(Trt = contr.treatment(n=3,base=3)))
summary(el.lme3) #NoAdd vs A and M

el.df <- data.frame(y.position=c(77,79,82),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj=c(0.000394,0.1776,0.0215),
                    p.adj.sig=c("3.94e-4***","0.178","0.0215*"))

## Plot ##
el.plot <- ggboxplot(el_long, x = "Trt", y = "EL", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(el.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="Ear Length, mm") + scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
el.plot

ggsave("ftel.eps", plot = el.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


## Figure Creation ##
fig3 <- (dy.plot | tn.viol) / (gpe.plot | el.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig3
jpeg("figure3.jpg", width = 180, height = 100, units = "mm", res = 300)
fig3
dev.off()

