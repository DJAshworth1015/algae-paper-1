#Setup
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggbeeswarm)
library(fmsb)

#Script intended to be used with the RProjects format
#Therefore no working directory is set

#Import
alg <- read.csv("AlgaeBatchComposition.csv")
str(alg)

npkvp <- read.csv("AlgaeFertiliserComposition.csv")
str(npkvp)

### Make data starplot ready ###
#Algae
str(alg)

alg.df <- data.frame(N = alg$N,
                     C = alg$C,
                     B = alg$B,
                     Ca = alg$Ca,
                     Cr = alg$Cr,
                     Cu = alg$Cu,
                     Fe = alg$Fe,
                     K = alg$K,
                     Mg = alg$Mg,
                     Mn = alg$Mn,
                     Mo = alg$Mo,
                     Na = alg$Na,
                     Ni = alg$Ni,
                     P = alg$P,
                     S = alg$S,
                     Sr = alg$Sr,
                     Zn = alg$Zn
)
rownames(alg.df) <- c("RL1","RL2","AP")
str(alg.df)

max_min.a <- data.frame(N = c(1,0),
                      C = c(1,0),
                      B = c(1,0),
                      Ca = c(1,0),
                      Cr = c(1,0),
                      Cu = c(1,0),
                      Fe = c(1,0),
                      K = c(1,0),
                      Mg = c(1,0),
                      Mn = c(1,0),
                      Mo = c(1,0),
                      Na = c(1,0),
                      Ni = c(1,0),
                      P = c(1,0),
                      S = c(1,0),
                      Sr = c(1,0),
                      Zn = c(1,0)
)
rownames(max_min.a) <- c("Max","Min")
alg.b <- rbind(max_min.a, alg.df)

#NPK v Pellets
str(npkvp)

npkvp.df <- data.frame(N = npkvp$N,
                     C = npkvp$C,
                     B = npkvp$B,
                     Ca = npkvp$Ca,
                     Co = npkvp$Co,
                     Cr = npkvp$Cr,
                     Cu = npkvp$Cu,
                     Fe = npkvp$Fe,
                     K = npkvp$K,
                     Mg = npkvp$Mg,
                     Mn = npkvp$Mn,
                     Mo = npkvp$Mo,
                     Na = npkvp$Na,
                     Ni = npkvp$Ni,
                     P = npkvp$P,
                     S = npkvp$S,
                     Sr = npkvp$Sr,
                     Ti = npkvp$Ti,
                     Zn = npkvp$Zn
)
rownames(npkvp.df) <- c("AP","NPK")
str(npkvp.df)

max_min.npk <- data.frame(N = c(1,0),
                        C = c(1,0),
                        B = c(1,0),
                        Ca = c(1,0),
                        Co = c(1,0),
                        Cr = c(1,0),
                        Cu = c(1,0),
                        Fe = c(1,0),
                        K = c(1,0),
                        Mg = c(1,0),
                        Mn = c(1,0),
                        Mo = c(1,0),
                        Na = c(1,0),
                        Ni = c(1,0),
                        P = c(1,0),
                        S = c(1,0),
                        Sr = c(1,0),
                        Ti = c(1,0),
                        Zn = c(1,0)
)
rownames(max_min.npk) <- c("Max","Min")
npkvp.b <- rbind(max_min.npk, npkvp.df)


### Star plots ###
#Algae
jpeg("figureS1.jpg", width = 185, height = 100, units = "mm", res = 300)
pal = c("#009E73", "#56B4E9", "#F0E442") #Picked from Okabe-Ito
par(mar = c(0,2,0,0))
radarchart(alg.b, 
           pcol = pal,
           pfcol = scales::alpha(pal,0.2),
           plwd = 1, plty = 1,
           cglcol = "grey", cglty = 1, cglwd = 0.4,
           caxislabels = c("0","0.25","0.5","0.75","1"),
           axislabcol = "#000000",
           axistype = 1
)
legend(x = "left", legend=c("Batch 1","Batch 2", "Pellets"), 
       bty="n", col = pal, pch=20, pt.cex = 1
)
dev.off()


#NPKvP
jpeg("figureS2.jpg", width = 185, height = 100, units = "mm", res = 300)
pal = c("#92D050","#4472C4")
par(mar = c(0,2,0,0))
radarchart(npkvp.b, 
           pcol = pal,
           pfcol = scales::alpha(pal,0.2),
           plwd = 1, plty = 1,
           cglcol = "grey", cglty = 1, cglwd = 0.4,
           caxislabels = c("0","0.25","0.5","0.75","1"),
           axislabcol = "#000000",
           axistype = 1
)
legend(x = "left", legend=c("Algae Pellets","NPK Granules"), 
       bty="n", col = pal, pch=20, pt.cex = 1
)
dev.off()