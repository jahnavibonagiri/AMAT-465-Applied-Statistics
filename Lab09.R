Groc<-View(CH06PR09)
names(CH06PR09)<-c('laborhrs','ncases','indcost','holiday')
data<-lm(laborhrs~ncases, data= CH06PR09)
data1<-lm(laborhrs~indcost, data= CH06PR09)
plot(laborhrs~ncases, data= CH06PR09)
plot(laborhrs~indcost, data= CH06PR09)
mod1<-with(CH06PR09,lm(laborhrs~ncases+indcost+factor(holiday)))
library(car)
LIdiag<- ls.diag(mod1)
round(cbind(std.res=LIdiag$std.res, stud.res = LIdiag$stud.res, hat=LIdiag$hat, dfits=LIdiag$dfits, cookd=LIdiag$cooks, dfbetas = dfbetas(mod1)),2)
outlierTest(mod1)
hat_cutoff<- .15
sum(LIdiag$hat_cutoff)
which(LIdiag$hat>hat_cutoff)
LIdiag$hat[which(LIdiag$hat>hat_cutoff)]
influencePlot(mod1, data=CH06PR09)
vif(mod1)
which(abs(dfbetas(mod1)[,2])>.27) #
head(dfbetas(mod1))
mod1<-with(CH06PR09,lm(laborhrs~ncases+indcost+factor(holiday)))

library(car)

LIdiag<- ls.diag(mod1)

round(cbind(std.res=LIdiag$std.res, stud.res = LIdiag$stud.res, hat=LIdiag$hat, dfits=LIdiag$dfits, cookd=LIdiag$cooks, dfbetas = dfbetas(mod1)),2)
length(mod1$residual)
mod1$df.residual
