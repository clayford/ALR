setwd("C:/Users/jcf2d/Box Sync/_statistics/ALR/")
# Table 1.5
icu <- read.table("icu.dat")
icuNames <- readLines("icu.txt")
icuNames <- icuNames[grepl("^[0-9]",icuNames)]
tmp <- strsplit(icuNames,split = "\t")
names(icu) <- sapply(tmp,function(x)x[length(x)])
# Table 1.6
lowbirth <- read.table("lowbwt.dat")
lbNames <- readLines("lowbwt.txt")
lbNames <- lbNames[grepl("^[0-9]",lbNames)]
tmp <- strsplit(lbNames,split = "\t")
names(lowbirth) <- sapply(tmp,function(x)x[length(x)])

# Table 2.2
mod1 <- glm(LOW ~ AGE + LWT + factor(RACE) + FTV, data=lowbirth, family=binomial)
summary(mod1)
logLik(mod1) # log likelihood
-2*log(exp(logLik(mod1))) # deviance


mod0 <- glm(LOW ~ 1, data=lowbirth, family=binomial)
summary(mod0)

anova(mod0,mod1) # LRT

# Table 2.3
mod2 <- glm(LOW ~ LWT + factor(RACE), data=lowbirth, family=binomial)
summary(mod2)
logLik(mod2)
# compare models in table 2.2 and table 2.3
anova(mod2, mod1)

# Wald test 
coef(mod1)
sqrt(diag(summary(mod1)$cov.unscaled)) # standard errors
summary(mod1)
# Wald test calculation
t(coef(mod1))%*%solve(summary(mod1)$cov.unscaled)%*%coef(mod1)






# 2.5 confidence intervals
predict(mod2, newdata=data.frame(RACE=1,LWT=150))
predict(mod2, newdata=data.frame(RACE=1,LWT=150), type="response")

# estimator of the variance of the estimator of the logit (2.9)
# manually
estvar <- t(c(1,150,0,0)) %*% summary(mod2)$cov.unscaled %*% c(1,150,0,0)
sqrt(estvar)
# using predict()
pout <- predict(mod2, newdata=data.frame(RACE=1,LWT=150), se = TRUE)
pout$se.fit # same as sqrt(estvar)

# confidence interval
pout$fit + c(-1,1)*qnorm(0.975)*pout$se.fit

# CI for coefficients
library(MASS)
confint(mod2)

