## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(warning = F, message = F, out.width = "60%")
library(dplyr)

## ----eval=F-------------------------------------------------------------------
# install.packages("geneticae")

## ----eval=F-------------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("jangelini/geneticae")

## -----------------------------------------------------------------------------
library(geneticae)

## -----------------------------------------------------------------------------
library(agridat)
data(yan.winterwheat)
head(yan.winterwheat)

## -----------------------------------------------------------------------------
data(plrv)
head(plrv)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 1: GE biplot based on yield data of 1993 Ontario winter wheat performance trials.  The 71.66% of GE variability is explained by the first two multiplicative terms. Cultivars are shown in lowercase and environments in uppercase.'----

rAMMI_clasic <- rAMMIModel(yan.winterwheat, genotype = "gen", environment = "env", 
      response = "yield", type = "AMMI")

rAMMIPlot(rAMMI_clasic, titles = TRUE, footnote = TRUE)


## -----------------------------------------------------------------------------
GGE1 <- rSREGModel(yan.winterwheat, genotype = "gen", environment = "env", 
                 response = "yield")

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 2: GGE biplot based on yield data of 1993 Ontario winter wheat performance trials. The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms. Cultivars are shown in lowercase and environments in uppercase.'----
rSREGPlot(GGE1, type = "Biplot", footnote = F, titles = F)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 3: comparison of cultivar performance in a selected environment (OA93). The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms.'----
rSREGPlot(GGE1, type = "Selected Environment", selectedE = "OA93", 
        footnote = F, titles = F)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 4: comparison of the performance of cultivar Luc in different environments. The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms. '----
rSREGPlot(GGE1, type = "Selected Genotype", selectedG = "Kat", 
        footnote = F, titles = F)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 5: comparison of the cultivars _Kat_ and _Cas_. The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms. Cultivars are shown in lowercase and environments in uppercase.'----
rSREGPlot(GGE1, type = "Comparison of Genotype", 
        selectedG1 = "Kat", selectedG2 = "Cas", 
        footnote = F, titles = F, axis_expand = 1.5)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 6: polygon view of the GGE biplot, showing which cultivars presented highest yield in each environment. The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms. Cultivars are shown in lowercase and environments in uppercase.'----
rSREGPlot(GGE1, type = "Which Won Where/What", footnote = F,
        titles = F, axis_expand = 1.5)


## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 7: average environment view of the GGE biplot based on genotype-focused scaling, showing mean yield and stability of genotypes. '----
data <- yan.winterwheat[yan.winterwheat$env %in% c("BH93", "EA93","HW93", "ID93",
                                                   "NN93", "RN93", "WP93"), ]
data <- droplevels(data)
GGE2 <- rSREGModel(data, genotype = "gen", environment = "env", 
                 response = "yield", SVP = "row")

rSREGPlot(GGE2, type = "Mean vs. Stability", footnote = F, titles = F, sizeEnv = 0)


## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 8: Classification of genotypes with respect to the ideal genotype. Genotype-focused scaling is used.', warning=FALSE----
rSREGPlot(GGE2, type = "Ranking Genotypes", footnote = F, titles = F, sizeEnv = 0)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 9: Relationship between environments. Environment-focused scaling is used.'----

GGE3 <- rSREGModel(data, genotype = "gen", environment = "env", 
                 response = "yield", SVP = "column")
rSREGPlot(GGE3, type = "Relationship Among Environments", footnote = F, titles = F)

## ----fig.width=6.5, fig.height=6, fig.align='center', dpi=300, out.width="60%", fig.cap='Figure 10: classification of environments with respect to the ideal environment. Environment-focused scaling is used.'----
rSREGPlot(GGE3, type = "Ranking Environments", footnote = F, titles = F)

## ----fig.width=12, fig.height=10, fig.align='center', dpi=300, out.width="100%", fig.cap='Figure 11: GGE biplot based on the height of 25 progenies of Eucalyptus grandis from trials carried out in seven environments in the southern and southeastern regions of Brazil. The scaling method used is symmetrical singular value partitioning. SREG (A), hSREG (B), CovSREG (C), and ppSREG (D).'----

library(patchwork)
library(ggplot2)
data("lavoranti.eucalyptus")

# Bellthorpe
lavoranti_Bellthorpe <- droplevels(subset(lavoranti.eucalyptus, origin=="Bellthorpe"))

Nenv <-length(levels(lavoranti_Bellthorpe$loc))
Ngen <- length(levels(lavoranti_Bellthorpe$gen))

lavoranti_Bellthorpe$genot <- rep(1:Ngen, Nenv)
lavoranti_Bellthorpe$genot <- as.factor(lavoranti_Bellthorpe$genot)


# SREG Classic
SREG_classic <- rSREGModel(lavoranti_Bellthorpe, genotype = "genot", 
                          environment = "loc", response = "height", 
                          model = "SREG")

# hSREG
hSREG <- rSREGModel(lavoranti_Bellthorpe, genotype = "genot", 
                    environment = "loc", response = "height", 
                    model = "hSREG")

# CovSREG
CovSREG <- rSREGModel(lavoranti_Bellthorpe, genotype = "genot", 
                    environment = "loc", response = "height", 
                    model = "hSREG")



# ppSREG
ppSREG <- rSREGModel(lavoranti_Bellthorpe, genotype = "genot", 
                    environment = "loc", response = "height", 
                    model = "ppSREG")

# Graphs
p1 <- rSREGPlot(SREG_classic, footnote = F, titles = T)
p2 <- hSREG_plot  <- rSREGPlot(hSREG, footnote = F, titles = T)
p3 <- CovSREG_plot <- rSREGPlot(CovSREG, footnote = F, titles = T)
p4 <- ppSREG_plot  <- rSREGPlot(ppSREG, footnote = F, titles = T)

grafico_robusto_SREG <- (p1 + p2) / (p3 + p4) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18, face = "bold"))

grafico_robusto_SREG

## -----------------------------------------------------------------------------
# Generating missing data
yan.winterwheat[1,3] <- NA
yan.winterwheat[3,3] <- NA
yan.winterwheat[2,3] <- NA

## -----------------------------------------------------------------------------
imputation(yan.winterwheat, genotype = "gen", environment = "env", 
           response = "yield", type = "EM-SREG")


imputation(yan.winterwheat, genotype = "gen", environment = "env", 
           response = "yield", type = "EM-bSREG")

## ----echo = FALSE-------------------------------------------------------------
sessionInfo()

