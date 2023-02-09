data<-read.csv(file.choose(),header=TRUE)
data
nh2001<-subset(data,CRYEAR==2001)
cor(as.numeric(data$NUMBED), as.numeric(data$TPY))
model <- lm(as.numeric(data$NUMBED)~ as.numeric(data$TPY))
summary(model)
confint(model, level=.99)
plot(as.numeric(data$TPY)~as.numeric(data$NUMBED))
anova(model)
sse <- sum((fitted(model) - as.numeric(data$NUMBED))^2)
sse <- sum((fitted(model) - as.numeric(data$TPY))^2)
ssr <- sum((fitted(model) - mean(as.numeric(data$TPY))^2))
sst <- ssr + sse
summary(model)$r.squared
