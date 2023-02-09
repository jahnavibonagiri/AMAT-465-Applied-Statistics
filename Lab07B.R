data<-View(ChickWeight)
pol<- lm(log(weight)~ poly(Time, 1), data=ChickWeight)
summary(pol)
plot(pol)
pol1<-lm(log(weight)~ poly(Time, 2), data=ChickWeight, subset=(Diet))
summary(pol1)

pol2<-lm(log(weight)~ poly(Time, 1), data=ChickWeight, subset=(Diet))
summary(pol2)

mod1<-lm(log(weight)~Time+I(Time^2), data=ChickWeight)
summary(mod1)
d<-as.factor(ChickWeight$Diet)


mod2<-lm(log(weight)~d*Time+d*I(Time^2), data=ChickWeight)
summary(mod2)

mod3<-lm(log(weight)~d*Time+I(Time^2), data=ChickWeight)
summary(mod3)



