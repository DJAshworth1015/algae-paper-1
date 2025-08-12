#Setup
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
exp1 <- read.csv("Exp1ZadokData.csv")
exp1$Trt <- as.factor(exp1$Trt)
exp1$Block <- as.factor(exp1$Block)
str(exp1)

##### 1st Exp #####
## 26DAS ##
kruskal.test(X26DAS~Trt, data=exp1)
dunnTest(X26DAS~Trt, data=exp1, method = "bh")

exp1.26.df <- data.frame(y.position=c(23,24,25,26,27,28.5),
                         group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                         group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                         p.adj.sig=c("0.198","0.183","0.434","8.37e-3**","0.440","0.0546"))

exp1.26.plot <- ggboxplot(exp1, x = "Trt", y = "X26DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.26.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 26DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.26.plot

## 38DAS ##
kruskal.test(X38DAS~Trt, data=exp1)
dunnTest(X38DAS~Trt, data=exp1, method = "bh")

exp1.38.df <- data.frame(y.position=c(25.5,26,26.5,27,27.5,28.2),
                         group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                         group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                         p.adj.sig=c("0.0471*","0.0629","0.574","0.726","0.0895","0.149"))

exp1.38.plot <- ggboxplot(exp1, x = "Trt", y = "X38DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + 
  geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.38.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 38DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.38.plot

## Figure S3 ##
figS3 <- (exp1.26.plot | exp1.38.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
figS3
jpeg("figureS3.jpg", width = 180, height = 50, units = "mm", res = 300)
figS3
dev.off()

## 42DAS ##
kruskal.test(X42DAS~Trt, data=exp1)
dunnTest(X42DAS~Trt, data=exp1, method = "bh")

exp1.42.df <- data.frame(y.position=c(32.8,33.7,34.6,35.5,36.4,37.6),
                         group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                         group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                         p.adj.sig=c("0.228","0.0174*","0.335","0.244","0.679","0.128"))

exp1.42.plot <- ggboxplot(exp1, x = "Trt", y = "X42DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + 
  geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.42.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 42DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.42.plot

## 45DAS ##
kruskal.test(X45DAS~Trt, data=exp1)
dunnTest(X45DAS~Trt, data=exp1, method = "bh")

exp1.45.df <- data.frame(y.position=c(32.8,33.65,34.5,35.35,36.2,37.4),
                         group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                         group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                         p.adj.sig=c("0.113","0.0475*","0.258","0.564","0.506","0.319"))

exp1.45.plot <- ggboxplot(exp1, x = "Trt", y = "X45DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + 
  geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.45.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 45DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.45.plot

## 46DAS ##
kruskal.test(X46DAS~Trt, data=exp1)
dunnTest(X46DAS~Trt, data=exp1, method = "bh")

exp1.46.df <- data.frame(y.position=c(33,33.2,33.4,33.6,33.8,34.1),
                         group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                         group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                         p.adj.sig=c("0.0611","0.0222*","0.707","0.551","0.0897","0.0238*"))

exp1.46.plot <- ggboxplot(exp1, x = "Trt", y = "X46DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + 
  geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.46.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 46DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.46.plot

## 66DAS ##
kruskal.test(X66DAS~Trt, data=exp1)
dunnTest(X66DAS~Trt, data=exp1, method = "bh")

exp1.66.df <- data.frame(y.position=c(51.5,52.1,52.7,53.3,53.8,54.6),
                     group1=c("NoAdd","NoAdd","NoAdd","AlgOnly","AlgOnly","AlgSupp"),
                     group2=c("AlgOnly","AlgSupp","Min","AlgSupp","Min","Min"),
                     p.adj.sig=c("0.420","0.0202*","0.369","0.128","0.914","0.124"))

exp1.66.plot <- ggboxplot(exp1, x = "Trt", y = "X66DAS", size = 0.25, outlier.shape = NA, palette=c("#000000","#92D050","#00B050","#4472C4"), order = c("NoAdd","AlgOnly","AlgSupp","Min"), show.legend = FALSE) +
  geom_boxplot(aes(col=Trt), outlier.shape = NA, size = 0.3, show.legend = FALSE) + geom_beeswarm(size = 1, cex = 2, aes(col=Trt), show.legend = FALSE) +
  stat_pvalue_manual(exp1.66.df, label= "p.adj.sig", label.size = 1.5, family = "Times New Roman") + 
  theme_clean() + theme(axis.text=element_text(size=rel(0.6)),
                        axis.title=element_text(size=rel(0.6)),
                        axis.line.x = element_line(linewidth = rel(0.6)),
                        axis.line.y = element_line(linewidth = rel(0.6)),
                        text = element_text(family = "Times New Roman"),
                        panel.border=element_blank(),
                        plot.background = element_blank()) +
  labs(x=NULL, y="Zadok Score at 66DAS") + scale_x_discrete(labels = c("No Additions", "Algae Only", "Algae Supplement", "Mineral"))
exp1.66.plot

## Figure S4 ##
figS4 <- (exp1.42.plot | exp1.45.plot) / (exp1.46.plot | exp1.66.plot) + 
  plot_annotation(tag_prefix ="(", tag_levels = 'A',tag_suffix = ")") & theme(plot.tag = element_text(size=rel(0.5)))
figS4
jpeg("figureS3.jpg", width = 180, height = 100, units = "mm", res = 300)
figS4
dev.off()