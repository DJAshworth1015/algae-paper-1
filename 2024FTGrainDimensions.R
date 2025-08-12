#Setup
library(carData)
library(car)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(lme4)
library(afex)
library(FSA)
library(extrafont)
library(patchwork)

#Script intended to be used with the RProjects format
#Therefore no working directory is set

#Import
marv <- read.csv("2024FTGrainDimensions.csv")
marv$Trt <- as.factor(marv$Trt)
marv$Block <- as.factor(marv$Block)
marv$Width <- as.factor(marv$Width)
marv$Depth <- as.factor(marv$Depth)
str(marv)

##### TGW #####
## Model ##
tgw.aov <- aov(TGW ~ Trt + Trt:Block, data = marv)
Anova(tgw.aov, type ="III") #Int NS
tgw.aov1 <- update(tgw.aov,~.-Trt:Block)
Anova(tgw.aov1, type="II") #Block NS
tgw.aov2 <- update(tgw.aov1,~.-Block)
Anova(tgw.aov2, type="II") #Trt sig

par(mfrow=c(2,2))
plot(tgw.aov2) #Acceptable

TukeyHSD(tgw.aov2)

tgw.df <- data.frame(y.position=c(52.8,53.25,53.9),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("4.59e-3**","4.21e-4***","0.608"))

## Plot ##
tgw.plot <- ggboxplot(marv, x = "Trt", y = "TGW", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(tgw.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="TGW, g") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
tgw.plot

ggsave("fttgw.eps", plot = tgw.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Gwid #####
## Model ##
gwid.aov <- aov(Gwid ~ Trt + Block + Trt:Block, data = marv)
Anova(gwid.aov, type = "III") #Int NS
gwid.aov1 <- update(gwid.aov,~.-Trt:Block)
Anova(gwid.aov1, type="II") #Trt NS
gwid.aov2 <- update(gwid.aov1,~.-Trt)
Anova(gwid.aov2, type="II") #Block NS

par(mfrow=c(2,2))
plot(gwid.aov2) #Anova unsuitable, use non-parametric

kruskal.test(Gwid ~ Trt, data = marv) #NS

## Plot ##
gwid.plot <- ggboxplot(marv, x = "Trt", y = "Gwid", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="Mean Grain Width, mm") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
gwid.plot

ggsave("ftwid.eps", plot = gwid.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


##### Glen #####
## Model ##
glen.aov <- aov(Glen ~ Trt + Block + Trt:Block, data = marv)
Anova(glen.aov, type="III") #Int NS
glen.aov1 <- update(glen.aov,~.-Trt:Block)
Anova(glen.aov1, type="II") #Block NS
glen.aov2 <- update(glen.aov1,~.-Block)
Anova(glen.aov2, type="II") #Trt sig

par(mfrow=c(2,2))
plot(glen.aov2) #Anova Unsuitable, use non-parametric

kruskal.test(Glen ~ Trt, data = marv) #Sig
dunnTest(Glen ~ Trt, data = marv)

glen.df <- data.frame(y.position=c(8.62,8.65,8.69),
                     group1=c("NoAdd","NoAdd","Alg"),
                     group2=c("Alg","Min","Min"),
                     p.adj.sig=c("0.0964","7.92e-4***","0.0941"))

## Plot ##
glen.plot <- ggboxplot(marv, x = "Trt", y = "Glen", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(glen.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Grain Length, mm") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
glen.plot 

ggsave("ftlength.eps", plot = glen.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Gare #####
## Model ##
gare.aov <- aov(Gare ~ Trt + Block + Trt:Block, data = marv)
Anova(gare.aov, type="III") #Int ns
gare.aov1 <- update(gare.aov,~.-Trt:Block)
Anova(gare.aov1, type="II") #Block ns
gare.aov2 <- update(gare.aov1,~.-Block)
Anova(gare.aov2, type="II") #Trt sig

par(mfrow=c(2,2))
plot(gare.aov2) #Acceptable

TukeyHSD(gare.aov2)

gare.df <- data.frame(y.position=c(24.3,24.4,24.55),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("0.117","5.06e-3**","0.344"))

## Plot ##
gare.plot <- ggboxplot(marv, x = "Trt", y = "Gare", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(gare.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Grain Area, square mm") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
gare.plot

ggsave("ftarea.eps", plot = gare.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

## Figure Creation ##
fig4 <- (tgw.plot | gwid.plot) / (glen.plot | gare.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig4
jpeg("figure4.jpg", width = 180, height = 100, units = "mm", res = 300)
fig4
dev.off()
