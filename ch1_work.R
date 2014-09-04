# applied logistic regression
setwd("~/_statistics/ALR/")

# chapter 1 work
chd <- read.table("chdage.dat")
names(chd) <- c("id","age","chd")
str(chd)

# fig 1
plot(chd ~ jitter(age), data=chd, ylim=c(0,1))

# add age group variable
chd$agrp <- cut(x = chd$age, breaks = c(20, 29, 34, 39, 44, 49, 54, 59, 69), include.lowest = TRUE)
table(chd$agrp)

# Table 1.2
library(plyr)
table2.1 <- ddply(chd, .(agrp), summarize, n = length(chd), 
      absent = sum(chd==0), 
      present = sum(chd==1),
      proportion = round(present/n,2))
mod1 <- glm(chd ~ age, data=chd, family=binomial)
summary(mod1)
predict(mod1)
predict(mod1, type="response")

# extract the log-likelihood
logLik(mod1)

mod0 <- glm(chd ~ 1, data=chd, family=binomial)
summary(mod0)
logLik(mod0)

# evaluation of G
anova(mod0, mod1)

# manually
G <- -2*log(exp(logLik(mod0))/exp(logLik(mod1)))
# or this way
G <- 2*(logLik(mod1) - logLik(mod0))

# confidence intervals
# Table 1.4
summary(mod1)$cov.unscaled

confint(mod1)
predict(object = mod1, newdata = data.frame(age=50), type="response", se.fit = TRUE)$se.fit


## example from Venables and Ripley (2002, pp. 190-2.)
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
summary(budworm.lg)

plot(c(1,32), c(0,1), type = "n", xlab = "dose",
     ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
                                           sex = factor(rep("M", length(ld)), levels = levels(sex))),
                    type = "response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
                                           sex = factor(rep("F", length(ld)), levels = levels(sex))),
                    type = "response"))


hts <- function(df){
  if(!is.data.frame(df)) stop("not a data frame")
  list(dimensions=dim(df),
       head=head(df,n=3), 
       tail=tail(df,n=3), 
       structure=ls.str(df),
       summary=summary(df))
}
hts(df=chd)

