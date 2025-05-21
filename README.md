# Introduciton
We presents a novel approach to enhancing nonparametric multivariate control charts by integrating depth functions with the exceedance probability criterion (EPC ) in Phase I analysi.
R programs are provided to facilitate implementation and adaptation.
# Install and load required packages
```{r}
packages <- c("dplyr", "MASS", "rgl", "geometry", "stats", "lmomco", "MDBED", "copula", "mrfDepth")
for (pkg in packages) {
  if (!(pkg %in% rownames(installed.packages()))) install.packages(pkg)
}
lapply(packages, require, character.only = TRUE)

library(MASS)
library(ddalpha)
library(geometry)
library(copula)
library(dplyr)
```{r}

# Example
