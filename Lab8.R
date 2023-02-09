install.packages("lmtest")


data<-View(water)
water.lm<-lm(water$USAGE~water$TEMP+water$PROD+water$DAYS+water$PAYR+water$HOUR, data=water)
summary(water.lm)
water.lm1<-lm(water$USAGE~water$PROD+water$PAYR, data=water)
summary(water.lm1)

anova(water.lm,water.lm1)
plot(water.lm1)
shapiro.test(water$residuals)
shapiro.test(residuals(water.lm1))
cor(water$TEMP, water$USAGE)
cor(water$PROD, water$USAGE)
cor(water$DAYS, water$USAGE)
cor(water$PAYR, water$USAGE)
cor(water$HOUR, water$USAGE)
library(lmtest)

bptest(water.lm1)
