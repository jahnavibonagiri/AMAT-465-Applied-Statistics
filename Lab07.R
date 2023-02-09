data<-View(insurance)
mod<- lm(expenses~age,  data=insurance)
plot(mod, pch = 15, col=rainbow(4)[insurance$smoker])
plot(insurance)
plot(insurance$bmi, insurance$expenses, col=rainbow(2))
plot(insurance$age, insurance$expenses, col=rainbow(2))

#mod1<-lm(gpa~hsm+hss+hse+satm+satv+as.factor(sex),data=csdata) #linear model
#summary(mod1)
