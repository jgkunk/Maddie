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

DoLob<- function(filnam){
XX<-read.csv(filnam,header=FALSE)
attach(XX)
#__t1__ __s1_    _v1__    ___t2__  __s2__    __v2___    __t3___ __s3___    __v3___
#             b1       e1                 b2         e2                 b3
#  1:12,13:22,NA,24:99,NA;101:112,113:122,NA,124:199,NA,201:212,213:222,NA,224:299
#  1:12,13:22,--,23:98,--, 99:110,111,120,--,121:196,--,197:208,209:218,--,219;294
isna=(1:(length(V1)))[is.na(V1)]
 b1=isna[1]; e1=isna[2]; b2=isna[3]; e2=isna[4]; b3=isna[5]
t1<- 1:12;       s1<- 13:(b1-1);          v1<-(b1+1):(e1-1) ; 
t2<- e1+t1;      s2<- (max(t2)+1):(b2-1); v2<-(b2+1):(e2-1) ;
t3<- e2+t1;      s3<- (max(t3)+1):(b3-1); v3<-(b3+1):(length(V1)) ;
Grp1=c(t1,s1,v1); Grp2=c(t2,s2,v2); Grp3=c(t3,s3,v3);
grp1=1:length(Grp1); grp2=max(grp1)+1:length(Grp2); grp3= max(grp2)+1:length(Grp3) 
lGrps<-length(c(Grp1,Grp2,Grp3))
XXX<-matrix(0,lGrps,3)
XXX[grp1,]<-as.matrix(XX[Grp1,])
XXX[grp2,]<-as.matrix(XX[Grp2,])
XXX[grp3,]<-as.matrix(XX[Grp3,])
browser()
t1<- 1:12;       s1<- 13:(b1-1);          v1<-(b1):(e1-2) ; 
t2<- e1-2+t1;      s2<- (max(t2)+1):(b2-3); v2<-(b2-2):(e2-4) ;
t3<- e2-4+t1;      s3<- (max(t3)+1):(b3-5); v3<-(b3-4):(length(V1)-5) ;
detach(XX)
quartz(width=12, height=3)   # was 8 and 2
par(mar=c(0.5,0.5,1,0.2))
sep=1.2
AngZ<- -0.1   # was -0.1
AngX<- 0.5*pi    # was 0.5
XXX<- data.frame(XXX[,1],XXX[,2],XXX[,3])  
names(XXX)=c('x','y','z')
attach(XXX)
x<- x-mean(x);y<- y-mean(y);z<- z-mean(z);
dx<-max(x)-min(x)
x2<- c(x,x+sep*dx); y2<-c(y,y); z2<- c(z,z)-(max(z)+min(z))/4
cat(names(XXX),'\n')
plot(1.0*x2,1.2*z2, typ='n', xlab='', ylab='', main=paste("Stereo Lobster",filnam))
mtext(date(), line= -1, adj=0.99)
XXX<-dorotYZ(AngX,x,y,z)
detach(XXX)
attach(XXX)
LWD=2
#lines(X,Z,typ='b',col='red',lty=1,lwd=2)
lines(X[t1],Z[t1],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s1],Z[s1],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v1],Z[v1],typ='l',col='red',lty=1,lwd=LWD,pch=3)
lines(X[t2],Z[t2],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s2],Z[s2],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v2],Z[v2],typ='l',col='red',lty=1,lwd=LWD,pch=3)
lines(X[t3],Z[t3],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s3],Z[s3],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v3],Z[v3],typ='l',col='red',lty=1,lwd=LWD,pch=3)

for (i in 1:12) text(X[t1[i]],Z[t1[i]],i, pos=1,col='red')
for (i in 1:12) text(X[t2[i]],Z[t2[i]],i, pos=2,col='green')
for (i in 1:12) text(X[t3[i]],Z[t3[i]],i, pos=3,col='blue')
ls=length(s1)
for (i in 1:ls) text(X[s1[i]],Z[s1[i]],letters[i], pos=1,col='red')
for (i in 1:ls) text(X[s2[i]],Z[s2[i]],letters[i], pos=2,col='green')
for (i in 1:ls) text(X[s3[i]],Z[s3[i]],letters[i], pos=3,col='blue')

XXX<-dorotXY(AngZ,X,Y,Z)
detach(XXX)
attach(XXX)
#lines(X+sep*dx,Z,typ='b',col='red',lty=1,lwd=2)
lines(X[t1]+sep*dx,Z[t1],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s1]+sep*dx,Z[s1],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v1]+sep*dx,Z[v1],typ='l',col='red',lty=1,lwd=LWD,pch=3)
lines(X[t2]+sep*dx,Z[t2],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s2]+sep*dx,Z[s2],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v2]+sep*dx,Z[v2],typ='l',col='red',lty=1,lwd=LWD,pch=3)
lines(X[t3]+sep*dx,Z[t3],typ='b',col='black',lty=1,lwd=LWD)
lines(X[s3]+sep*dx,Z[s3],typ='b',col='blue',lty=1,lwd=LWD,pch=3)
lines(X[v3]+sep*dx,Z[v3],typ='l',col='red',lty=1,lwd=LWD,pch=3)

for (i in 1:12) text(X[t1[i]]+sep*dx,Z[t1[i]],i, pos=1,col='red')
for (i in 1:12) text(X[t2[i]]+sep*dx,Z[t2[i]],i, pos=2,col='green')
for (i in 1:12) text(X[t3[i]]+sep*dx,Z[t3[i]],i, pos=3,col='blue')

for (i in 1:ls) text(X[s1[i]]+sep*dx,Z[s1[i]],letters[i], pos=1,col='red')
for (i in 1:ls) text(X[s2[i]]+sep*dx,Z[s2[i]],letters[i], pos=2,col='green')
for (i in 1:ls) text(X[s3[i]]+sep*dx,Z[s3[i]],letters[i], pos=3,col='blue')

detach(XXX)
 }
 Filnam<-"http://www.bio.umass.edu/biology/kunkel/LabWiki/images/9/9b/Ha20140128_Alpha02.txt"
Out<-DoLob(Filnam)
 Filnam<-"http://www.bio.umass.edu/biology/kunkel/LabWiki/images/0/05/Ha20140127_Beta02.txt"
Out<-DoLob(Filnam)
 Filnam<-"http://www.bio.umass.edu/biology/kunkel/LabWiki/images/7/74/Ha20140127_Gamma02.txt"
Out<-DoLob(Filnam)
