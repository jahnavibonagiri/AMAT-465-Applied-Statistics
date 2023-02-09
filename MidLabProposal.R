cancer<-cancerdataset1
#Categorical Variables
str(cancer)
table(cancer$geography)
source("pairs.r")  
# fill the empty space with the path of the place where you have pairs.r in your computer
pairs(insurance[c(7, 1,3)],panel=panel.smooth,diag.panel=panel.hist,lower.panel=panel.cor) 
mod1<- plot(lm(log(cancer$target_deathrate)~cancer$pctblack, data=cancerdataset1))
summary(mod1)
plot(cancer$medincome, log(cancer$target_deathrate))
plot(log(cancer$target_deathrate)~cancer$pctblack, data=cancer)
mod2<- plot(lm(cancer$avgdeathsperyear~poly(cancer$medincome,6), data=cancerdataset1))

summary(mod2)
plot(mod2)
plot(log(cancer$avgdeathsperyear)~cancer$medincome, data=cancer)




