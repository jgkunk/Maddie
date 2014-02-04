# NutEric1ReadFromURLstereoPlot.R
# Produces a stereo pair of slightly canted and 0.1 radian relatively rotated offset data pairs.
# Adapt to read "Ha20140121_Alpha01.txt" "Ha20140121_Beta01.txt"  "Ha20140121_Gamma01.txt"
rm(list=ls())

dorotXY<- function(ang, X,Y,Z) {
	      X<- X* cos(ang) - Y*sin(ang)
	      Y<- X* sin(ang) + Y*cos(ang)
	      data.frame(X,Y,Z)
}
dorotYZ<- function(ang, X,Y,Z) {
	      Z<- Z* cos(ang) - Y*sin(ang)
	      Y<- Z* sin(ang) + Y*cos(ang)
	      data.frame(X,Y,Z)
}

DoLob<- function(filnam,Grp1,Grp2,Grp3,ti,si){
XX<-read.csv(filnam,header=FALSE)
lGrps<-length(c(Grp1,Grp2,Grp3))
XXX<-matrix(0,lGrps,3)
XXX[Grp1,]<-as.matrix(XX[Grp1,])
XXX[Grp2,]<-as.matrix(XX[Grp2,])
XXX[Grp3,]<-as.matrix(XX[Grp3,])
quartz(width=12, height=3)   # was 8 and 2
par(mar=c(0.5,0.5,1,0.2))
sep=1.2
AngZ<- -0.1   # was -0,1
AngX<- 0.5    # was 0.2
XXX<- data.frame(XXX[,1],XXX[,2],XXX[,3])  
names(XXX)=c('x','y','z')
attach(XXX)
x<- x-mean(x);y<- y-mean(y);z<- z-mean(z);
dx<-max(x)-min(x)
x2<- c(x,x+sep*dx); y2<-c(y,y); z2<- c(z,z)
cat(names(XXX),'\n')
plot(1.0*x2,1.2*z2, typ='n', xlab='', ylab='', main=paste("Stereo Lobster",filnam))
XXX<-dorotYZ(AngX,x,y,z)
detach(XXX)
attach(XXX)
LWD=2
lgrp<- length(Grp1)
#lines(X,Z,typ='b',col='red',lty=1,lwd=2)
lines(X[ti],Z[ti],typ='b',col='black',lty=1,lwd=LWD)
lines(X[si],Z[si],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[ti+lgrp],Z[ti+lgrp],typ='p',col='black',lty=1,lwd=LWD)
lines(X[si+lgrp],Z[si+lgrp],typ='p',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[ti+2*lgrp],Z[ti+2*lgrp],typ='p',col='black',lty=1,lwd=LWD)
lines(X[si+2*lgrp],Z[si+2*lgrp],typ='p',col='blue',lty=1,lwd=LWD,pch=3)
XXX<-dorotXY(AngZ,X,Y,Z)
detach(XXX)
attach(XXX)
#lines(X+sep*dx,Z,typ='b',col='red',lty=1,lwd=2)
lines(X[ti]+sep*dx,Z[ti],typ='b',col='black',lty=1,lwd=LWD)
lines(X[si]+sep*dx,Z[si],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[ti+lgrp]+sep*dx,Z[ti+lgrp],typ='p',col='black',lty=1,lwd=LWD)
lines(X[si+lgrp]+sep*dx,Z[si+lgrp],typ='p',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[ti+2*lgrp]+sep*dx,Z[ti+2*lgrp],typ='p',col='black',lty=1,lwd=LWD)
lines(X[si+2*lgrp]+sep*dx,Z[si+2*lgrp],typ='p',col='blue',lty=1,lwd=LWD,pch=3)

detach(XXX)
 }
 endi<-23
 grp1<-1:endi;grp2<-grp1+endi; grp3<-grp2+endi
 Ti=1:12; Si=13:endi
 Filnam<-"Ha20140121_Alpha01.txt"
Out<-DoLob(Filnam,grp1,grp2,grp3,Ti,Si)
 endi<-21
 grp1<-1:endi;grp2<-grp1+endi; grp3<-grp2+endi
 Ti=1:12; Si=13:endi
 Filnam<-"Ha20140121_Beta01.txt"
Out<-DoLob(Filnam,grp1,grp2,grp3,Ti,Si)
 endi<-21
 grp1<-1:endi;grp2<-grp1+endi; grp3<-grp2+endi
 Ti=1:12; Si=13:endi
 Filnam<-"Ha20140121_Gamma01.txt"
Out<-DoLob(Filnam,grp1,grp2,grp3,Ti,Si)
