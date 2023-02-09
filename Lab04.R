data<-View(WiscNursingHome)
nh2001<-subset(WiscNursingHome,CRYEAR==2001)
s<-summary(model)$sigma 
x_new<-seq(min(nh2001$NUMBED),max(nh2001$NUMBED),by=100)
n<-length(nh2001$TPY)
model<-lm(nh2001$TPY~nh2001$NUMBED)
Sxx<-var(nh2001$NUMBED)*(n-1)
s.mean.pred<- s*sqrt((1/n)+(x_new-mean(nh2001$NUMBED))^2/Sxx)
b0<--1.034922
b1<-0.932384
fitted<-b0+b1*x_new
mean.pred.lb<-fitted-qt(.975,n-2)*s.mean.pred
mean.pred.ub<-fitted+qt(.975,n-2)*s.mean.pred
plot(nh2001$NUMBED,nh2001$TPY, main="Plot", xlab="NUMBED", ylab="TPY")
abline(model)
abline(model,col='red')
lines(x_new,mean.pred.ub,col = 'blue', lty = 2, lwd = 2)
lines(x_new,mean.pred.lb,col = 'blue', lty = 2, lwd = 2)
s.ind.pred<- s*sqrt(((1/n)+(x_new-mean(300))^2/Sxx))
ind.pred.lb<-fitted-qt(.975,n-2)*s.ind.pred
ind.pred.ub<-fitted+qt(.975,n-2)*s.ind.pred
lines(x_new,ind.pred.ub,col = 'green', lty = 3, lwd = 2)
lines(x_new,ind.pred.lb,col = 'green', lty = 3, lwd = 2)
confint(model, level=0.98)





