## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(warning = F, collapse = TRUE, message = F, out.width = "60%")

## ---- echo=F, fig.align='center', fig.cap='Figure: Polygon view of the GGE biplot, showing which cultivars presented highest yield in each environment. The scaling method used is symmetrical singular value partitioning (by default). The 78% of G + GE variability is explained by the first two multiplicative terms. Cultivars are shown in lowercase and environments in uppercase.'----
library(geneticae)
library(agridat)
data(yan.winterwheat)
GGE1 <- GGEmodel(yan.winterwheat, genotype = "gen", environment = "env", 
                 response = "yield")

GGEPlot(GGE1, type = "Which Won Where/What", footnote = F,
        titles = F, axis_expand = 1.5)

