# ==============================================================================
# ============= This will be the big figure for the Plos model.
# ==============================================================================

B0 <- 1;

B1funM <- function(fi,M1,F1,M2){
  a <- -1 * log((2*(fi[M1,F1]+fi[M1,M2]))/(1+(2*fi[M1,F1])+fi[M1,M1]));
  b <- fi[M1,F1] - fi[M2,F1];
  return(a/b);
}

B1funF <- function(fi,M1,F1,M2){
  a <- -1 * log((1+fi[F1,F1]+(2*fi[F1,M2]))/(1+fi[F1,F1]+(2*fi[F1,M1])));
  b <- fi[F1,M1] - fi[F1,M2];
  return(a/b);
}

setEPS();
postscript("NewBigFig.eps");
par(mar=c(0.75,0.25,0.5,0.25),mfrow=c(3,3),oma=c(6,6.5,1,1));

################################################################################
## Upper left plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(fs))),col="grey40");
text(x=0.2015,y=7.6,labels=expression(f[M2F1]==0.0),cex=1.0);
text(x=0.20,y=6.8,labels=expression(f[M1M2]==0.0),cex=1.0);

text(x=0.1,y=5.45,labels=expression(paste(italic(M1)," threshold")),cex=1);
arrows(x0=0.1,y0=5,x1=0.075,y1=3.5,length=0.1,angle=30,code=2,lwd=2,col="grey30");

text(x=0.175,y=3.45,labels=expression(paste(italic(F1)," threshold")),cex=1);
arrows(x0=0.175,y0=3,x1=0.125,y1=0.6,length=0.1,angle=30,code=2,lwd=2,,col="grey30");
################################################################################
## Upper mid plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0,0.0,0,0.0625,0.0,0.0625,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
text(x=0.2015,y=7.6,labels=expression(f[M2F1]==0.0625),cex=1.0);
text(x=0.180,y=6.8,labels=expression(f[M1M2]==0.000),cex=1.0);
################################################################################
## Upper mid plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0,0.0,0,0.125,0.0,0.125,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
Fl[25001] <- 1.333333;
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
text(x=0.2015,y=7.6,labels=expression(f[M2F1]==0.125),cex=1.0);
text(x=0.185,y=6.8,labels=expression(f[M1M2]==0.000),cex=1.0);
################################################################################
## Mid left plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0625,0.0,0,0.0,0.0625,0.0,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
text(x=0.175,y=7.6,labels=expression(f[M2F1]==0.0),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.0625),cex=1.0);
################################################################################
## Mid mid plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0625,0.0,0,0.0625,0.0625,0.0625,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
text(x=0.195,y=7.6,labels=expression(f[M2F1]==0.0625),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.0625),cex=1.0);
################################################################################
## Mid right plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.0625,0.0,0,0.125,0.0625,0.125,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),xaxt="n",ylab="",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
text(x=0.1905,y=7.6,labels=expression(f[M2F1]==0.125),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.0625),cex=1.0);
################################################################################
## Lower left plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.125,0.0,0,0.0,0.125,0.0,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),ylab="",xaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
axis(side=1,at=c(0,0.10,0.20),cex.axis=2);
text(x=0.18,y=7.6,labels=expression(f[M2F1]==0.0),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.125),cex=1.0);
################################################################################
## Lower mid plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.125,0.0,0,0.0625,0.125,0.0625,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),ylab="",xaxt="n",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
axis(side=1,at=c(0,0.10,0.20),cex.axis=2);
text(x=0.2,y=7.6,labels=expression(f[M2F1]==0.0625),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.125),cex=1.0);
################################################################################
## Lower right plot 
################################################################################
fi <- matrix(data=c(0,0.0,0.125,0.0,0,0.125,0.125,0.125,0.0),nrow=3,byrow=TRUE);
fs <- seq(from=0,to=0.25,by=0.00001);
Ml <- rep(0,length(fs));
Fl <- rep(0,length(fs));
for(i in 1:length(fs)){
  fi[1,2] <- fs[i];
  fi[2,1] <- fs[i];
  Ml[i] <- B1funM(fi=fi,M1=1,F1=2,M2=3);
  Fl[i] <- B1funF(fi=fi,M1=1,F1=2,M2=3);
  if(i %% 10000 == 0){
    print(c(i,"of",length(fs)));
  }
}
Ml[Ml<0] <- exp(8);
Ml[Ml>exp(8)] <- exp(8);
Fl[which(is.na(Fl[Fl>0])==TRUE)] <- NA; 
par(mar=c(0.5,0.5,0.5,0.5));
plot(x=fs[-1],y=log(Ml[-1]),type="l",lwd=3,cex.axis=2,cex.lab=1.5,pch=20,lty="solid",
  xlab="",col="black",ylim=c(0,8),xlim=c(0,0.25),ylab="",xaxt="n",yaxt="n");
points(x=fs,y=log(Fl),type="l",lwd=2,lty="solid");
Fl[which(is.na(Fl))] <- mean(c(Fl[which(is.na(Fl))+1],Fl[which(is.na(Fl))-1]));
polygon(x=c(fs,rev(fs)),y=c(log(Ml),rep(0,length(Ml))),col="black",lwd=3);
polygon(x=c(fs,rev(fs)),y=c(log(Fl),rep(0,length(Fl))),col="grey40");
axis(side=1,at=c(0,0.10,0.20),cex.axis=2);
text(x=0.195,y=7.6,labels=expression(f[M2F1]==0.125),cex=1.0);
text(x=0.195,y=6.8,labels=expression(f[M1M2]==0.125),cex=1.0);

################################################################################
################################################################################
################################################################################

mtext(expression(paste("Kinship with potential mate (",italic(f[M1F1]),")")),
	outer=TRUE,side=1,line=3.25,cex=1.5);

mtext(expression(paste("Ln Inbreeding depression threshold (",italic(beta[1]),")")),
	outer=TRUE,side=2,line=2.5,cex=1.5);

dev.off();














