X<-c(2,6,8,8,12,16,20,20,22,26)
Y<-c(58,105,88,118,117,137,157,169,149,202)
plot(X,Y)
par(mar=c(1, 1, 1, 1))
plot(X,Y,main="Pizza Sales",xlab="Population", ylab="Sales")
print("Correlation")
cor(X,Y)

print("Simple Regression")
model<-lm(Y~X)
summary(model)
mean(Y)
mean(X)
abline(model)

  n <- length(Y) 
  lm.model <- lm(Y ~ X) 
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  sse <- sum((Y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  t.val <- qt(0.975, n - 2)
model$residuals
sum(model$residuals)
sum(-12,15, -12,  18,  -3,  -3,  -3,   9 ,-21,  12)
var(X)
var(X)*(n-1)/n
var(Y)
var(Y)*(n-1)/n
sqrt((n-1)/n) * sd(X)
confint(model,level=.99 )
confint(model)
b0<-model$coefficients[1]
b1<-model$coefficients[2]
df<-model.frame(X,Y)
pizza<- lm(Y~X)
newdata<- model.frame(X=15)
predict(pizza, newdata)

