quartz("Stereo lobsters", width=13, height=5)
op <- par(mfrow = c(1, 3), # 2 x 2 pictures on one plot
          pty = "m")       # square plotting region,
                           # independent of device size
par(mar=c(0.1,0.1,0.1,0.1))
filnam<-"Ha20140121_Alpha01.txt"
XX<-read.csv(filnam,header=FALSE)
attach(XX)
plot(V1,V2,typ='b')
mtext(filnam,line=-2)
detach(XX)
filnam<-"Ha20140121_Beta01.txt"
XX<-read.csv("Ha20140121_Beta01.txt",header=FALSE)
attach(XX)
plot(V1,V2,typ='b')
mtext(filnam,line=-2)
detach(XX)
filnam<-"Ha20140121_Gamma01.txt"
XX<-read.csv("Ha20140121_Gamma01.txt",header=FALSE)
attach(XX)
plot(V1,V2,typ='b')
mtext(filnam,line=-2)
detach(XX)
