library(epitools)
library(aod)
sum(breast=='?')     # this will tell you how many missing data there are
breast[breast=='?'] <- NA      # change the '?' to NA
newbreast <-na.omit(breast)  # removes all rows with missing data NA
#breast$degmalig <- factor(breast$degmalig, level=c("1", "2", "3"))
modd<- glm(Y~tsize+degmalig, family=binomial, data=breast)
summary(modd)
exp(coefficients(modd))
predict(modd, newdata=breast, tsize=12, degmalig=2, type="response")
x<-jitter(breast$tsize)
plot(breast$Y~x,col=c("blue","green","red")[breast$degmalig],xlab="tsize",main="Recurrence vs Deg.Malig")
#add fitted values (colored by deg.malig)
points(breast$tsize,model$fitted,col=c("blue","green","red")[breast$degmalig],pch=22)
#add fitted values curve
lines(lowess(breast$tsize,model$fitted),col="black",lwd=2)
#look at a threshold of .5
abline(h=.5,col='grey')
predict(modd, data.frame(tsize=(30), degmalig=3),type="response")
diffdev<-summary(modd)$null.deviance -summary(modd)$deviance
diffdf<-summary(modd)$df.null -summary(modd)$df.residual
cbind('Diff in deviances'=diffdev,' Diff in degrees of freedom'=diffdf,' p-value'=1-pchisq(diffdev,diffdf))
library(aod)

#perform Wald Test to determine if 3rd and 4th predictor variables are both zero
wald.test(Sigma = vcov(modd), b = coef(modd), Terms = 1:2)
ORtable<-matrix(c(12, 2),nrow = 2, ncol = 2)
wald.test(ORtable)
library(oddsratio)
or_glm(data = breast, model = modd, incr = list(tsize=12, degmalig=2))
#perform Wald Test to determine if 3rd and 4th predictor variables are both zero
wald.test(Sigma = vcov(modd), b = coef(modd), Terms = 1:3)

