#data<-View(hills)
wind.lm<-View(wind)
wind.lm<-lm(wind$speed~wind$output)
par(mar = c(1,1,1,1)) 
plot(wind$speed~wind$output)
abline(wind.lm)
summary(wind.lm)
text(hills$dist[7],hills$time[7]-2,row.names(hills)[7],cex=.5) #bens of jura
text(hills$dist[11]-3,hills$time[11]+2,row.names(hills)[11],cex=.5) #Lairig Ghru
text(hills$dist[18]+3,hills$time[18]+2,row.names(hills)[18],cex=.5) #Knock Hill


