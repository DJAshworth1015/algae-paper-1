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

#Import
yield <- read.csv("2024FTYieldandGrainN.csv")
yield$Trt <- as.factor(yield$Trt)
yield$Block <- as.factor(yield$Block)
str(yield)

mlt <- read.csv("2024FTMaltingData.csv")
mlt$Plot <- as.factor(mlt$Plot)
mlt$TechRep <- as.factor(mlt$TechRep)
mlt$Trt <- as.factor(mlt$Trt)
mlt$Block <- as.factor(mlt$Block)
mlt$Run <- as.factor(mlt$Run)
mlt$MaltTank <- as.factor(mlt$MaltTank)
mlt$Segment <- as.factor(mlt$Segment)
mlt$Kiln <- as.factor(mlt$Kiln)
mlt$Mash <- as.factor(mlt$Mash)
str(mlt)

##### %N #####
PctN.aov <- aov(PctN ~ Trt + Block + Trt:Block, data = yield)
Anova(PctN.aov, type="III") #Int NS
pctn.aov1 <- update(PctN.aov,~.-Trt:Block)
Anova(pctn.aov1,type="II") #Block sig
TukeyHSD(pctn.aov1) #NoAdd-min diff, block 1-3 diff, need lme

pctn.mix <- mixed(PctN ~ Trt + (1|Block), data = yield)
pctn.mix #Trt overall is sig
pctn.lme1 <- lmer(PctN ~ Trt + (1|Block), data = yield)
summary(pctn.lme1) #Uses alg as contrast
pctn.lme2 <- lmer(PctN ~ Trt + (1|Block), data = yield, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(pctn.lme2) #Uses min as contrast

plot(pctn.lme1) #Check resid vs fitted
qqnorm(resid(pctn.lme1))
qqline(resid(pctn.lme1)) #Check QQs
##All good

pctn.df <- data.frame(y.position=c(1.51,1.53,1.555),
                      group1=c("NoAdd","NoAdd","Alg"),
                      group2=c("Alg","Min","Min"),
                      p.adj.sig=c("0.176","9.98e-4***","0.0257*"))


PctN.plot <- ggboxplot(yield, x = "Trt", y = "PctN", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(pctn.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Grain %N, dry") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
PctN.plot

ggsave("ftgrainN.eps", plot = PctN.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


##### Wet Extract #####
wetext.aov <- aov(WetExt ~ Trt + Segment + Block, data = mlt)
Anova(wetext.aov, type="II") #Block NS
wetext.aov1 <- update(wetext.aov,~.-Block)
Anova(wetext.aov1, type="II") #Segment very significant, use mixed model

par(mfrow=c(2,2))
plot(wetext.aov1)

wetext.mix <- mixed(WetExt ~ Trt + (1|Segment), data = mlt)
wetext.mix #Trt sig <0.001
wetext.lme <- lmer(WetExt ~ Trt + (1|Segment), data = mlt)
summary(wetext.lme) #A v M,N
wetext.lme2 <- lmer(WetExt ~ Trt + (1|Segment), data = mlt, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(wetext.lme2) #M v A,N
wetext.lme3 <- lmer(WetExt ~ Trt + (1|Segment), data = mlt, contrasts=list(Trt = contr.treatment(n=3,base=3)))
summary(wetext.lme3) #N v A,M

plot(wetext.lme) #Check resid vs fitted
qqnorm(resid(wetext.lme))
qqline(resid(wetext.lme)) #Check QQs
#All good

wetext.df <- data.frame(y.position=c(292.8,293.7,295),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj.sig=c("0.0289*","2.03e-4***","0.114"))


wetext.plot <- ggboxplot(mlt, x = "Trt", y = "WetExt", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(wetext.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Hot Water Extract (as is), Litre degrees/kg") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
wetext.plot

ggsave("wetext.eps", plot = wetext.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Dry Extract #####
dryext.aov <- aov(DryExt ~ Trt + Segment + Block, data = mlt)
Anova(dryext.aov, type="II") #Segment NS
dryext.aov1 <- update(dryext.aov,~.-Segment)
Anova(dryext.aov1, type="II") #Block NS
dryext.aov2 <- update(dryext.aov1,~.-Block)
Anova(dryext.aov2) #Trt sig

par(mfrow=c(2,2))
plot(dryext.aov2) #Point 1 is a massive outlier
mlt <- mlt[-1,] #Remove extremely outlaying point, different from its other 3 technical reps

dryext.aov3 <- aov(DryExt ~ Trt + Segment + Block, data = mlt)
Anova(dryext.aov3, type="II") #Segmenet NS
dryext.aov4 <- update(dryext.aov3,~.-Segment)
Anova(dryext.aov4, type="II") #Block NS
dryext.aov5 <- update(dryext.aov4,~.-Block)
Anova(dryext.aov5) #Trt sig

plot(dryext.aov5) #Good

TukeyHSD(dryext.aov5)

dryext.df <- data.frame(y.position=c(313,313.8,315),
                        group1=c("NoAdd","NoAdd","Alg"),
                        group2=c("Alg","Min","Min"),
                        p.adj.sig=c("3.55e-3**","7.0e-6***","0.271"))

dryext.plot <- ggboxplot(mlt, x = "Trt", y = "DryExt", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(dryext.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Hot Water Extract (dry), Litre degrees/kg") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
dryext.plot

ggsave("dryext.eps", plot = dryext.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Total Malt Nitrogen #####
#Remember to restore row 1 after removing it for the above analysis
tn.aov <- aov(TN ~ Trt + Segment + Block, data = mlt)
Anova(tn.aov, type="II") #Segment NS
tn.aov1 <- update(tn.aov,~.-Segment)
Anova(tn.aov1, type="II") #Block and Trt sig, use lmer

tn.mix <- mixed(TN ~ Trt + (1|Block), data = mlt, method="KR")
tn.mix #Trt sig <0.001
tn.lme <- lmer(TN ~ Trt + (1|Block), data = mlt)
summary(tn.lme) #A v M,N
tn.lme2 <- lmer(TN ~ Trt + (1|Block), data = mlt, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(tn.lme2) #M v A,N
tn.lme3 <- lmer(TN ~ Trt + (1|Block), data = mlt, contrasts=list(Trt = contr.treatment(n=3,base=3)))
summary(tn.lme3) #N v A,M

plot(tn.lme) #Check resid vs fitted
qqnorm(resid(tn.lme))
qqline(resid(tn.lme)) #Check QQs
#Good

tn.df <- data.frame(y.position=c(1.176,1.195,1.22),
                        group1=c("NoAdd","NoAdd","Alg"),
                        group2=c("Alg","Min","Min"),
                        p.adj.sig=c("3.68e-3**","4.34e-10***","2.09e-4***"))

tn.plot <- ggboxplot(mlt, x = "Trt", y = "TN", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(tn.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="Malt Total Nitrogen, % by dry mass") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
tn.plot

ggsave("maltN.eps", plot = tn.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Soluble Nitrogen ######
tsn.aov <- aov(TSN ~ Trt + Segment + Block, data=mlt)
Anova(tsn.aov, type="II") #Segment NS
tsn.aov1 <- update(tsn.aov,~.-Segment)
Anova(tsn.aov1, type="II") #Block and Trt sig, use lmer

tsn.mix <- mixed(TSN ~ Trt + (1|Block), data=mlt)
tsn.mix #Trt sig 0.018
tsn.lme <- lmer(TSN ~ Trt + (1|Block), data=mlt)
summary(tsn.lme) #A v M,N
tsn.lme2 <- lmer(TSN ~ Trt + (1|Block), data=mlt, contrasts=list(Trt = contr.treatment(n=3,base=2)))
summary(tsn.lme2) #M v A,N
tsn.lme3 <- lmer(TSN ~ Trt + (1|Block), data=mlt, contrasts=list(Trt = contr.treatment(n=3,base=3)))
summary(tsn.lme3) #N v A,M

plot(tsn.lme) #Check resid vs fitted
qqnorm(resid(tsn.lme))
qqline(resid(tsn.lme)) #Check QQs
#Good

tsn.df <- data.frame(y.position=c(0.45,0.468,0.495),
                    group1=c("NoAdd","NoAdd","Alg"),
                    group2=c("Alg","Min","Min"),
                    p.adj.sig=c("0.016*","0.013*","0.924"))

tsn.plot <- ggboxplot(mlt, x = "Trt", y = "TSN", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(tsn.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Soluble Nitrogen, % by dry mass") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
tsn.plot

ggsave("solubleN.eps", plot = tsn.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### SNR #####
snr.aov <- aov(SNR ~ Trt + Segment + Block, data = mlt)
Anova(snr.aov, type="II") #Seg NS
snr.aov1 <- update(snr.aov,~.-Segment)
Anova(snr.aov1) #Block NS
snr.aov2 <- update(snr.aov1,~.-Block)
Anova(snr.aov2, type="II") #Trt NS

par(mfrow=c(2,2))
plot(snr.aov2) #Good with an outlier

snr.plot <- ggboxplot(mlt, x = "Trt", y = "SNR", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Soluble N:Total N, %") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
snr.plot

ggsave("SNR.eps", plot = snr.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Wet PSY #####
wetpsy.aov <- aov(WetPSY ~ Trt + Segment + Block, data = mlt)
Anova(wetpsy.aov, type="II") #Block NS
wetpsy.aov1 <- update(wetpsy.aov,~.-Block)
Anova(wetpsy.aov1, type ="II") #Segment sig

wetpsy.mix <- mixed(WetPSY ~ Trt + (1|Segment), data = mlt)
wetpsy.mix #NS
wetpsy.lme <- lmer(WetPSY ~ Trt + (1|Segment), data = mlt)

plot(wetpsy.lme) #Check resid vs fitted
qqnorm(resid(wetpsy.lme))
qqline(resid(wetpsy.lme)) #Check QQs
#Good

wetpsy.plot <- ggboxplot(mlt, x = "Trt", y = "WetPSY", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="PSY (as is), OLA/t") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
wetpsy.plot

ggsave("wetPSY.eps", plot = wetpsy.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Dry PSY #####
drypsy.aov <- aov(DryPSY ~ Trt + Segment + Block, data = mlt)
Anova(drypsy.aov, type="II") #Block NS
drypsy.aov1 <- update(drypsy.aov,~.-Block)
Anova(drypsy.aov1, type="II") #Segment NS
drypsy.aov2 <- update(drypsy.aov1,~.-Segment)
Anova(drypsy.aov2, type="II") #Trt sig, no need for lmer

par(mfrow=c(2,2))
plot(drypsy.aov2) #Point 1 extreme outlier
mlt <- mlt[-1,] #Remove extremely outlaying point, different from its other 3 technical reps

drypsy.aov3 <- aov(DryPSY ~ Trt + Segment + Block, data = mlt)
Anova(drypsy.aov3, type="II") #BlockNS
drypsy.aov4 <- update(drypsy.aov3,~.-Block)
Anova(drypsy.aov4, type="II") #SegmentNS
drypsy.aov5 <- update(drypsy.aov4,~.-Segment)
Anova(drypsy.aov5) #Trt sig, no need for lmer
par(mfrow=c(2,2))
plot(drypsy.aov5) #Good with slight QQ tail

TukeyHSD(drypsy.aov5)

drypsy.df <- data.frame(y.position=c(433.5,434.8,436.5),
                     group1=c("NoAdd","NoAdd","Alg"),
                     group2=c("Alg","Min","Min"),
                     p.adj.sig=c("0.0232*","2.93e-3**","0.814"))

drypsy.plot <- ggboxplot(mlt, x = "Trt", y = "DryPSY", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#4472C4"), order = c("NoAdd","Alg","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(drypsy.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="PSY (dry), OLA/t") +
  scale_x_discrete(labels = c("No Additions","Algae Pellets", "NPK Granules"))
drypsy.plot

ggsave("dryPSY.eps", plot = drypsy.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

## Figure Creation ##
fig5 <- (PctN.plot | tn.plot) / (tsn.plot | snr.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig5
jpeg("figure5.jpg", width = 180, height = 100, units = "mm", res = 300)
fig5
dev.off()

fig6 <- (wetext.plot | dryext.plot) / (wetpsy.plot | drypsy.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig6
jpeg("figure6.jpg", width = 180, height = 100, units = "mm", res = 300)
fig6
dev.off()
