data<- View(weightloss)
data<- lm(weightloss$Loss ~ weightloss$Before)
plot(weightloss$Before,weightloss$Loss, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", col= weightloss$Diet+1)
plot(weightloss$Diet,weightloss$Loss, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", col= weightloss$Diet+1)
mod2<-lm(weightloss$Loss ~ weightloss$Before*weightloss$Diet+weightloss$Diet*I(weightloss$Loss^2), data=weightloss)
summary(mod2)

mod3<-lm(weightloss$Loss ~ weightloss$Before*weightloss$Diet+I(weightloss$Loss^2), data=weightloss)
summary(mod3)
