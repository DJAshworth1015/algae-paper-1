#Setup
library(carData)
library(car)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggbeeswarm)
library(lme4)
library(afex)
library(extrafont)
library(patchwork)

#Script intended to be used with the RProjects format
#Therefore no working directory is set

#Import
comb <- read.csv("3ExpsCombinedMaturity.csv")
comb$Trt <- as.factor(comb$Trt)
comb$Experiment <- as.factor(comb$Experiment)
comb$Block <- as.factor(comb$Block)
str(comb)


##### Mass #####
mas.mix <- mixed(Mass ~ Trt + (1|Experiment/Block), data = comb, method="KR")
mas.mix #Sig <0.001
mas.lme1 <- lmer(Mass ~ Trt + (1|Experiment/Block), data = comb)
summary(mas.lme1) #AO
mas.lme2 <- lmer(Mass ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(mas.lme2) #AS
mas.lme3 <- lmer(Mass ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(mas.lme3) #Min
mas.lme4 <- lmer(Mass ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(mas.lme4) #NoAdd
plot(mas.lme1) #Check resid vs fitted
qqnorm(resid(mas.lme1))
qqline(resid(mas.lme1)) #Check QQs
#All good

mas.df <- data.frame(y.position=c(10.3,10.9,11.5,12.1,12.7,13.55),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.0000188, 0.00109, 0.0000038, 0.2529, 0.6967, 0.1253),
                     p.adj.sig=c("1.88e-5***","1.09e-3**","3.08e-6***","0.253","0.697","0.125"))


mas.plot <- ggboxplot(comb, x = "Trt", y = "Mass", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) + 
  stat_pvalue_manual(mas.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) + 
  labs(x=NULL, y="Earless Dry Mass, g") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
mas.plot

ggsave("aggmass.eps", plot = mas.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


##### Tillers #####
til.mix <- mixed(Tillers ~ Trt + (1|Experiment/Block), data = comb, method="KR")
til.mix #Sig 0.018
til.lme1 <- lmer(Tillers ~ Trt + (1|Experiment/Block), data = comb)
summary(til.lme1)

plot(til.lme1) #Check resid vs fitted
qqnorm(resid(til.lme1))
qqline(resid(til.lme1)) #Check QQs
# Resid vs fitted increases variance with increasing value
# Normal QQ shows deviation at tails
# Tillers are count data so try poisson glmer


til.mix.glm <- mixed(Tillers ~ Trt + (1|Experiment/Block), data = comb, method="LRT", family=poisson)
til.mix.glm #NS 0.214
til.glme1 <- glmer(Tillers ~ Trt + (1|Experiment/Block), data = comb, family=poisson)
summary(til.glme1)
plot(til.glme1) #Check resid vs fitted
qqnorm(resid(til.glme1))
qqline(resid(til.glme1)) #Check QQs
# Residuals better
# Normal QQs no better, tails deviate
# Test for poisson distribution
mean(comb$Tillers) #5.87
var(comb$Tillers) #12.84
# Data overdispersed, poisson not valid, try log to tame higher values


til.mix.log <- mixed(log(Tillers) ~ Trt + (1|Experiment/Block), data = comb, method="KR")
til.mix.log #Sig <0.001
til.lme.log1 <- lmer(log(Tillers) ~ Trt + (1|Experiment/Block), data = comb)
summary(til.lme.log1)

plot(til.lme.log1) #Check resid vs fitted
qqnorm(resid(til.lme.log1))
qqline(resid(til.lme.log1)) #Check QQs
# Residuals and QQs good, use log transformed data

til.lme.log2 <- lmer(log(Tillers) ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(til.lme.log2)
til.lme.log3 <- lmer(log(Tillers) ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(til.lme.log3)
til.lme.log4 <- lmer(log(Tillers) ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(til.lme.log4)

til.df <- data.frame(y.position=c(16,17,18,19,20.3,22),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.00829, 0.000637, 4.71e-5, 0.36797, 0.1094, 0.4958),
                     p.adj.sig=c("8.29e-3**","6.37e-4***","4.71e-5***","0.368","0.109","0.496"))

til.plot <- ggboxplot(comb, x = "Trt", y = "Tillers", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(til.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  scale_y_continuous(breaks = c(2,6,10,14,18),sec.axis = sec_axis(~ log(.), breaks = c(1,1.5,2,2.25,2.5,2.75) ,name = "Ln(Tillers)")) +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Tillers") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
til.plot

ggsave("aggtill.eps", plot = til.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### TFG #####
tfg.mix <- mixed(TFG ~ Trt + (1|Experiment/Block), data = comb, method="KR") #Sig
tfg.mix #Sig 0.007
tfg.lme1 <- lmer(TFG ~ Trt + (1|Experiment/Block), data = comb)
summary(tfg.lme1)
tfg.lme2 <- lmer(TFG ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(tfg.lme2)
tfg.lme3 <- lmer(TFG ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(tfg.lme3)
tfg.lme4 <- lmer(TFG ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(tfg.lme4)
plot(tfg.lme1) #Check resid vs fitted
qqnorm(resid(tfg.lme1))
qqline(resid(tfg.lme1)) #Check QQs
#All good

tfg.df <- data.frame(y.position=c(260,274,288,302,316,338),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.02295, 0.01829, 0.000763, 0.9027, 0.2407, 0.3004),
                     p.adj.sig=c("0.0230*","0.0183*","7.63e-4***","0.903","0.241","0.300"))

tfg.plot <- ggboxplot(comb, x = "Trt", y = "TFG", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(tfg.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Total Filled Grain") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
tfg.plot

ggsave("aggtfg.eps", plot = tfg.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### FGPE #####
fgp.mix <- mixed(FGPE ~ Trt + (1|Experiment/Block), data = comb, method="KR")
fgp.mix #NS 0.969
fgp.lme <- lmer(FGPE ~ Trt + (1|Experiment/Block), data = comb)
plot(fgp.lme) #Check resid vs fitted
qqnorm(resid(fgp.lme))
qqline(resid(fgp.lme)) #Check QQs
#Fine

fgp.plot <- ggboxplot(comb, x = "Trt", y = "FGPE", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Filled Grain Per Ear") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
fgp.plot

ggsave("aggfgpe.eps", plot = fgp.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### TGW #####
tgw.mix <- mixed(TGW ~ Trt + (1|Experiment/Block), data = comb, method="KR")
tgw.mix #Sig 0.008
tgw.lme1 <- lmer(TGW ~ Trt + (1|Experiment/Block), data = comb)
summary(tgw.lme1)
tgw.lme2 <- lmer(TGW ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(tgw.lme2)
tgw.lme3 <- lmer(TGW ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(tgw.lme3)
tgw.lme4 <- lmer(TGW ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(tgw.lme4)
plot(tgw.lme1) #Check resid vs fitted
qqnorm(resid(tgw.lme1))
qqline(resid(tgw.lme1)) #Check QQs
#All good

tgw.df <- data.frame(y.position=c(65,67.5,70,72.5,75,78.3),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.000763, 0.0315, 0.0336, 0.197, 0.158, 0.932),
                     p.adj.sig=c("7.63e-4***","0.0315*","0.0336*","0.197","0.158","0.932"))

tgw.plot <- ggboxplot(comb, x = "Trt", y = "TGW", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) + 
  geom_boxplot(aes(col=Trt),  outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(tgw.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="TGW, g") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
tgw.plot

ggsave("aggtgw.eps", plot = tgw.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Grain Width #####
wid.mix <- mixed(Width ~ Trt + (1|Experiment/Block), data = comb, method="KR")
wid.mix #Sig 0.029
wid.lme1 <- lmer(Width ~ Trt + (1|Experiment/Block), data = comb)
summary(wid.lme1)
wid.lme2 <- lmer(Width ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(wid.lme2)
wid.lme3 <- lmer(Width ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(wid.lme3)
wid.lme4 <- lmer(Width ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(wid.lme4)
plot(wid.lme1) #Check resid vs fitted
qqnorm(resid(wid.lme1))
qqline(resid(wid.lme1)) #Check QQs
#QQ has tails but that's common in biological data

wid.df <- data.frame(y.position=c(4.08,4.16,4.24,4.32,4.4,4.52),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.00539, 0.05694, 0.01733, 0.3688, 0.6363, 0.6556),
                     p.adj.sig=c("5.39e-3**","0.0569","0.0173*","0.369","0.636","0.656"))

wid.plot <- ggboxplot(comb, x = "Trt", y = "Width", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(wid.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Grain Width, mm") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
wid.plot

ggsave("aggwid.eps", plot = wid.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white")

##### Grain Length #####
len.mix <- mixed(Length ~ Trt + (1|Experiment/Block), data = comb, method="KR")
len.mix #NS 0.197
len.lme <- lmer(Length ~ Trt + (1|Experiment/Block), data = comb)
plot(len.lme) #Check resid vs fitted
qqnorm(resid(len.lme))
qqline(resid(len.lme)) #Check QQs
#QQ has a tail

len.plot <- ggboxplot(comb, x = "Trt", y = "Length", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Grain Length, mm") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
len.plot

ggsave("agglen.eps", plot = len.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used

##### Grain Area #####
are.mix <- mixed(Area ~ Trt + (1|Experiment/Block), data = comb, method="KR") #Sig
are.mix# Sig 0.018
are.lme1 <- lmer(Area~ Trt + (1|Experiment/Block), data = comb)
summary(are.lme1)
are.lme2 <- lmer(Area ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=2)))
summary(are.lme2)
are.lme3 <- lmer(Area ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=3)))
summary(are.lme3)
are.lme4 <- lmer(Area ~ Trt + (1|Experiment/Block), data = comb, contrasts=list(Trt = contr.treatment(n=4,base=4)))
summary(are.lme4)
plot(are.lme1) #Check resid vs fitted
qqnorm(resid(are.lme1))
qqline(resid(are.lme1)) #Check QQs
#All good

are.df <- data.frame(y.position=c(30,30.8,31.6,32.4,33.2,34.5),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj=c(0.00165, 0.0952, 0.1336, 0.1257, 0.0736, 0.8265),
                     p.adj.sig=c("1.65e-3**","0.0952","0.134","0.126","0.0736","0.827"))

are.plot <- ggboxplot(comb, x = "Trt", y = "Area", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(are.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Mean Grain Area, square mm") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
are.plot

ggsave("aggarea.eps", plot = are.plot,
       device = cairo_ps,
       family = "Times New Roman",
       fallback_resolution = 300,
       width = 6, height = 4,
       bg = "white") #Not currently used


#Fig Assembly
fig1 <- (mas.plot | til.plot) / (tfg.plot | fgp.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig1
jpeg("figure1.jpg", width = 180, height = 100, units = "mm", res = 300)
fig1
dev.off()

fig2 <- (tgw.plot | wid.plot) / (len.plot | are.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
fig2
jpeg("figure2.jpg", width = 180, height = 100, units = "mm", res = 300)
fig2
dev.off()
