source("https://bioconductor.org/biocLite.R")
biocLite(pkgs = "tissuesGeneExpression")

library(tissuesGeneExpression)
data(tissuesGeneExpression)
head(e)
head(tissue)

library(GSE5859Subset)
library(Biobase)
library(GSE5859Subset)
data(GSE5859Subset)
