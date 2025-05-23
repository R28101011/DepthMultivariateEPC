# Introduction
We present a novel approach to enhancing nonparametric multivariate control charts by integrating depth functions with the exceedance probability criterion in Phase I analysis. An R program is provided to calculate the upper control limit.

# An example: semiconductor manufacturing process (uci-secom.csv)

## Step 1: Install and load required packages  
```{R}
packages <- c("ddalpha","dplyr", "MASS", "rgl", "geometry", "stats", "lmomco", "MDBED", "copula", "mrfDepth")
for (pkg in packages) {
  if (!(pkg %in% rownames(installed.packages()))) install.packages(pkg)
}
lapply(packages, require, character.only = TRUE)
library(MASS)
library(ddalpha)
library(geometry)
library(copula)
library(dplyr)
```

## Step 2: Load the cleaned dataset  
```{R}
cleaned_data <- read.csv("final_cleaned_dataset.csv", stringsAsFactors = FALSE)
# Selection for the Phase I dataset
dt0 <- cleaned_data %>% filter(Group == -1)
# Selection for the numerical variables
dt <- dt0[, !(names(dt0) %in% c("Time", "Group"))]
```

## Step 3: Calculate the spatial depth and the corresponding outlyingness  
```{R}
SpatialDepth <- depth.spatial(dt, dt)
threshold <- 1e-4
Ospa <- ifelse(SpatialDepth > threshold, 1 / SpatialDepth - 1, 1 / threshold-1 )
```
## Step 4: Calculate the upper control limit and the middle line
In code.R, the OspaEPC function is used to calculate the upper control limit and the middle line. The arguments `Ospa` and `pn` represent the outlyingness and the nominal coverage probability, respectively. For example, we would like to calculate the upper control limit and middle line based on a given outlyingness `Ospa`, with a nominal coverage probability of 0.95.  
```{R}
UCL <- OspaEPC(Ospa = Ospa, pn=0.95)$UCL
ML <- OspaEPC(Ospa = Ospa)$ML
```

## Step 5: Construct the control chart  
```{R}
plot(Ospa, 
     xlab = "Sample Sequence", 
     ylab = "Outlyingness", 
     main = paste("EPC-based control chart with Pn=", pn, ": uci-secom.csv"),
     pch = 20, 
     lty = 1, 
     ylim=c(-0.1, 18),
     cex.lab=1.5, cex.main=1.5,
     cex.main = 1.5)
lines(Ospa, col = "black")
# Add control limit lines
abline(h= UCL, col = "red", lty = 2, lwd=2)
text(x = mean(par("usr")[1:4]), y = UCL + 1, 
     labels = sprintf("%.5f", UCL), col = "black", cex = 1)
abline(h = ML, col = "black", lty = 3, lwd=2)
abline(h = 0, col = "black", lty = 1, lwd = 1)
```
