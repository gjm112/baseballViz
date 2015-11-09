1. Walter Johnson
2. Lefty Grove
3. Roger Clemens
4. Bob Gibson
5. Don Drysdale
6. Pete Alexander
7. Christy Mathewson
8. Eddie Plank
9. Tom Seaver
10. Cy Young

library(XML)
datListP<-list()

★
#1
url<-"http://www.baseball-reference.com/players/j/johnswa01.shtml"
datListP[["johnson"]]<-readHTMLTable(url)[[2]]

#2
url<-"http://www.baseball-reference.com/players/g/grovele01.shtml"
datListP[["grove"]]<-readHTMLTable(url)[[2]]

#3
url<-"http://www.baseball-reference.com/players/c/clemero02.shtml"
datListP[["clemens"]]<-readHTMLTable(url)[[2]]

#4
url<-"http://www.baseball-reference.com/players/g/gibsobo01.shtml"
datListP[["gibson"]]<-readHTMLTable(url)[[2]]

#5
url<-"http://www.baseball-reference.com/players/d/drysddo01.shtml"
datListP[["drysdale"]]<-readHTMLTable(url)[[2]]

#6
url<-"http://www.baseball-reference.com/players/a/alexape01.shtml"
datListP[["alexander"]]<-readHTMLTable(url)[[2]]

#7
url<-"http://www.baseball-reference.com/players/m/mathech01.shtml"
datListP[["mathewson"]]<-readHTMLTable(url)[[2]]

#8
url<-"http://www.baseball-reference.com/players/p/planked01.shtml"
datListP[["plank"]]<-readHTMLTable(url)[[2]]

#9
url<-"http://www.baseball-reference.com/players/s/seaveto01.shtml"
datListP[["seaver"]]<-readHTMLTable(url)[[2]]

#10 
url<-"http://www.baseball-reference.com/players/y/youngcy01.shtml"
datListP[["young"]]<-readHTMLTable(url)[[2]]

save(datListP,file="/Users/gregorymatthews/Dropbox/top10pitcher.RData")

for (i in 1:10){
  datListP[[i]][,1]<-gsub("★","",datListP[[i]][,1])
  datListP[[i]]<-as.data.frame(apply(datListP[[i]],2,function(x){as.numeric(as.character(x))}))
  #datListP[[i]]$'1B'<-datListP[[i]]$'H'-datListP[[i]]$'HR'-datListP[[i]]$'3B'-datListP[[i]]$'2B'
}

names(datListP)<-c("W. Johnson","L. Grove","R. Clemens","B. Gibson","D. Drysdale","P. Alexander","C. Mathewson","E. Plank","T. Seaver","C. Young")

png("/Users/gregorymatthews/testITP.png",w=30,h=10,units="in",res=300)
par(mfrow=c(2,5))
for (k in 1:10){
temp<-datListP[[k]]
temp[is.na(temp)]<-0
#cBA<-sum(temp$H)/sum(temp$AB)
#cOBP<-sum(temp$H+temp$BB+temp$HBP)/sum(temp$AB+temp$BB+temp$HBP+temp$SF)
cERA<-sum(temp$ER)/sum(temp$IP)*9
n<-dim(temp)[1]
plot(0,0,frame.plot=F,xlim=c(-1,n+1),ylim=c(-1,91),col="white",xaxt='n',yaxt='n',xlab="",ylab="Earned Run Average",main=names(datListP)[k])
for (i in 1:dim(temp)[1]){
  #polygon(i-1+temp$G[i]/162*c(-.5,.5,.5,-.5),c(0,0,min(temp$H[i],40),min(temp$H[i],40)),col=rgb(1-0/200,0,0/200,.2),bor=rgb(1-0/200,0,0/200,.2))
  for (q in c(0,80,160,240,320,400)){
    if (temp$IP[i]>q){polygon(i-1+temp$GS[i]/50*c(-.5,.5,.5*temp$W[i]/30,-.5*temp$W[i]/30),c(0,0,min(80,temp$IP[i]-q),min(80,temp$IP[i]-q)),col=rgb(1-q/400,0,q/400,.2),bor=rgb(1-q/400,0,q/400,.2))}
  }

  #polygon(i-1+temp$G[i]/154*c(-.25,.25,.25,-.25),c(0,0,min(temp$HR[i],40),min(temp$HR[i],40)),col=rgb(1,0,0,.5),bor=rgb(1,0,0,.5))
  #if (temp$HR[i]>40){polygon(i-1+temp$G[i]/154*c(-.5,.5,.5,-.5),c(0,0,temp$HR[i]-40,temp$HR[i]-40),col=rgb(1,0,0,.5),bor=rgb(1,0,0,.5))}
  #BA deviations
  #if (temp$BA[i]>cBA){points(c(i,i)-1,c(temp$BA[i]*40,cBA*40),type="l",lwd=5,col="green")}
  #if (temp$BA[i]<=cBA){points(c(i,i)-1,c(temp$BA[i]*40,cBA*40),type="l",lwd=5,col="black")}
  #OBP
  polygon(c(0,1:length(temp$ERA)-1,length(temp$ERA)-1),c(cERA*15,temp$ERA*15,cERA*15),lwd=0.1,col=rgb(.5,.5,.1,.05),bor=rgb(.5,.5,1,.05))
  if (temp$ERA[i]>cERA){points(c(i,i)-1,c(temp$ERA[i]*15,cERA*15),type="l",lwd=1,col="black",lty=3)}
  if (temp$ERA[i]<=cERA){points(c(i,i)-1,c(temp$ERA[i]*15,cERA*15),type="l",lwd=1,col="black",lty=3)}
  if (temp$ERA[i]>cERA){points(c(i)-1,c(temp$ERA[i]*15),pch=16,col="black",cex=temp$SO[i]/100)}
  if (temp$ERA[i]<=cERA){points(c(i)-1,c(temp$ERA[i]*15),pch=16,col="black",cex=temp$SO[i]/100)}
  
}

points(c(0,n-1),c(cERA,cERA)*15,type="l",lty=3)

points(c(0:(n))-.5,rep(80,n+1),cex=1,pch="|")
points(c(0:(n))-.5,rep(0,n+1),cex=1,pch="|")
#points(c(0,n-1),c(cBA,cBA)*40,type="l")

CurlyBraces(-1.5, 0, 1, pos = 2, direction = 2 );text(0,-3,"Games Started",cex=.5)
CurlyBraces(81.5, 0, 1, pos = 2, direction = 1 );text(0,83,"Wins",cex=.5)
points(n/2-1.5,-3,pch=16,cex=.5)
points(n/2-.75,-3,pch=16,cex=1)
points(n/2,-3,pch=16,cex=1.5)
text(n/2+1.5,-3,"Strikeouts",cex=.5)

for (q in c(0,40,80,120)){
  polygon(n/2+5+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

for (q in c(0,40,80,120,160)){
  polygon(n/2+5.75+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

for (q in c(0,40,80,120,160,200)){
  polygon(n/2+5+1.5+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

text(n/2+5+3.25,-3,"Innings Pitched",cex=.5)


}
dev.off()