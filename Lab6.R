panel.hist<-function(x,...){usr<-par("usr"); on.exit(par(usr)); 
par(usr=c(usr[1:2], 0, 1.5)); 
h<-hist(x, plot = FALSE); 
breaks <-h$breaks; nB <-length(breaks); 
y<-h$counts; y <-y/max(y); 
rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
} 
panel.cor <- function(x,y,digits=2,prefix="",cex.cor, ...) {
  usr <-par("usr"); on.exit(par(usr));par(usr = c(0, 1, 0, 1)); 
  r <-abs(cor(x, y,use="complete.obs")); 
  abrasion <-format(c(r, 0.123456789), digits = digits)[1]; 
  abrasion <-paste0(prefix, abrasion);
  if(missing(cex.cor)) 
    cex.cor <- 0.8/strwidth(abrasion); 
  text(0.5, 0.5, abrasion, cex = cex.cor * r)
}
par(mar=c(.5,.5,.5,.5)); 
pairs(abrasion,panel=panel.smooth,diag.panel=panel.hist,lower.panel=panel.cor)

bereal<- lm(Loss~x1+x2, data=abrasion)
summary(bereal)
