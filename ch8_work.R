# ch 8 work
setwd("~/stats/ALR/")

lowbirth <- read.table("data/lowbwt.dat")
lbNames <- readLines("data/lowbwt.txt")
lbNames <- lbNames[grepl("^[0-9]",lbNames)]
tmp <- strsplit(lbNames,split = "\t")
names(lowbirth) <- sapply(tmp,function(x)x[length(x)])
# create BWT4
lowbirth$BWT4 <- cut(lowbirth$BWT, breaks = c(-Inf,2500,3000,3500,Inf), labels=3:0)
# make levels = 0,1,2,3
lowbirth$BWT4 <- factor(lowbirth$BWT4,levels = 0:3)
levels(lowbirth$BWT4)

# Table 8.16
tab8.16 <- with(lowbirth, table(BWT4,SMOKE))
addmargins(tab8.16)

library(MASS)
library(nnet)
(bwt.mu <- multinom(BWT4 ~ SMOKE, lowbirth))
# odds ratios for the multinomial or baseline logit model
exp(coef(bwt.mu)[,2])
# The increase in the odds ratio demonstrates an increase in odds of a
# progressively lower weight baby among women who smoke during pregnancy.

# create BWT4
lowbirth$BWT4N <- cut(lowbirth$BWT, breaks = c(-Inf,2500,3000,3500,Inf), labels=0:3)

# Table 8.20
(bwt.po <- polr(BWT4N ~ LWT, lowbirth))
