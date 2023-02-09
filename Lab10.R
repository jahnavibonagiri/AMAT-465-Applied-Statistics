head(mtcars)

PRESS <- function(model) {
  x <- model.matrix(model)
  pr <- resid(model)/(1-hat(x))
  PRESS <-sum(pr^2)
  return(PRESS)
}

Criteria<-function(model, FullMSE, Label=F){
  s.model<-summary(model)
  SST = sum(anova(model)$ 'Sum Sq')
  R2= summary(model)$'r.squared'
  R2adj<- summary(model)$'adj.r.squared'
  SSE<-SST* (1-R2)
  dfsse<-tail(anova(model)$Df,1)
  samplesize<- length(model$residuals)
  parameters<- as.integer (samplesize - dfsse)
  Cp<-(SSE/(FullMSE))-samplesize + 2*(nparameters)
  AIC<- samplesize*Loq(SSE)-samplesize*log(samplesize) +2*(nparameters)
  press<-PRESS(model)
  if(Label==T)
    c("9+1"=nparameters, RZadj-round (R2adj ,4), Cp=round (Cp, 2), AIC=round(AIC, 2), PRESS-press)
  else
    c(parameters, round (Radj,4), round (Cp, 2), round (AIC, 2) ,press)
}

model<-lm(mpg~.,data=mtcars)
Criteria(model,FullMSE=10,Label=T)
