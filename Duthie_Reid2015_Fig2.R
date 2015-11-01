
setEPS();
postscript("ClassicInbr.eps");

## Recreating Waser et al.'s plot:  

ri <- matrix(data=c(1,0.5,0,0.5,1,0,0,0,1),nrow=3,byrow=TRUE);

# This function is just for a focal male in the presence of a sister
# and an unrelated male.

Inbreed <- function(focal,mate,alt){
  Inb <- ri[focal,focal] + ri[focal,mate];
  Out <- ri[focal,mate]  + ri[focal,alt];
  DEL <- 1 - (Out/Inb);
  return(DEL);
}

rs <- seq(from=0,to=1,by=0.0001);
dl <- rep(x=0,length=length(rs));
for(i in 1:length(rs)){
  ri <- matrix(data=c(1,rs[i],0,rs[i],1,0,0,0,1),nrow=3,byrow=TRUE);
  dl[i] <- Inbreed(focal=1,mate=2,alt=3);
}


par(mar=c(5,5,1,1));
plot(x=rs,y=dl,type="l",lwd=2,
  xlab=expression(paste("Relatedness to potential mate (",italic(r[M1F1]),")")),
  ylab=expression(paste("Inbreeding depression threshold (",italic(delta[M1F1]),")")),
  cex.lab=1.5,cex.axis=1.5,ylim=c(0,1));

# This function is just for a focal female in the presence of two brothers
Inbreed <- function(focal,mate,alt){
  Inb <- ri[focal,focal] + ri[focal,mate];
  Out <- ri[focal,focal]  + ri[focal,alt];
  DEL <- 1 - (Out/Inb);
  return(DEL);
}
dk <- rep(x=0,length=length(rs));
for(i in 1:length(rs)){
  ri <- matrix(data=c(1,rs[i],0,rs[i],1,0,0,0,1),nrow=3,byrow=TRUE);
  dk[i] <- Inbreed(focal=2,mate=1,alt=3);
}

points(x=rs,y=dk,type="l",lwd=2,
  cex.lab=1.5,cex.axis=1.5,ylim=c(0,1));
polygon(x=c(rs,rev(rs)),y=c(dk,rev(dl)),col="black");
polygon(x=c(rs,rev(rs)),y=c(rep(0,length(dk)),rev(dk)),col="grey40");

points(x=seq(from=0,to=1,by=0.1),y=rep(1,11),type="l");
points(y=seq(from=0,to=1,by=0.1),x=rep(1,11),type="l");

text(x=0.75,y=0.16,labels=expression(paste(italic(F1)," threshold")),cex=1.75);
arrows(x0=0.75,y0=0.2,x1=0.6,y1=0.375, length = 0.15, angle = 30, code = 2, lwd=2);

text(x=0.75,y=0.84,labels=expression(paste(italic(M1)," threshold")),cex=1.75);
arrows(x0=0.75,y0=0.8,x1=0.6,y1=0.625, length = 0.15, angle = 30, code = 2, lwd=2);

dev.off();

