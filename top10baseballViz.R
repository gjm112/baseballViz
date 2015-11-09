# Function to create curly braces
# x, y position where to put the braces
# range is the widht
# position: 1 vertical, 2 horizontal
# direction: 1 left/down, 2 right/up
CurlyBraces <- function(x, y, range, pos = 1, direction = 1 ) {
  
  a=c(1,2,3,48,50)    # set flexion point for spline
  b=c(0,.2,.28,.7,.8) # set depth for spline flexion point
  
  curve = spline(a, b, n = 50, method = "natural")$y / 2 
  
  curve = c(curve,rev(curve))
  
  a_sequence = rep(x,100)
  b_sequence = seq(y-range/2,y+range/2,length=100)  
  
  # direction
  if(direction==1)
    a_sequence = a_sequence+curve
  if(direction==2)
    a_sequence = a_sequence-curve
  
  # pos
  if(pos==1)
    lines(a_sequence,b_sequence) # vertical
  if(pos==2)
    lines(b_sequence,a_sequence) # horizontal
  
}

plot(0,0,ylim=c(-10,10),xlim=c(-10,10))
CurlyBraces(2, 0, 10, pos = 1, direction = 1 )
CurlyBraces(2, 0, 5,  pos = 1, direction = 2 )
CurlyBraces(1, 0, 10, pos = 2, direction = 1 )
CurlyBraces(1, 0, 5,  pos = 2, direction = 2 )
1. Albert Pujols
2. Babe Ruth
3. Barry Bonds
4. Lou Gehrig
5. Honus Wagner
6. Mickey Mantle
7. Ty Cobb
8. Ted Williams
9. Joe DeMaggio
10. Willie Mays

library(XML)
datList<-list()

★
#1
url<-"http://www.baseball-reference.com/players/p/pujolal01.shtml"
datList[["pujols"]]<-readHTMLTable(url)[[3]]

#2
url<-"http://www.baseball-reference.com/players/r/ruthba01.shtml"
datList[["baberuth"]]<-readHTMLTable(url)[[2]]

#3
url<-"http://www.baseball-reference.com/players/b/bondsba01.shtml"
datList[["bonds"]]<-readHTMLTable(url)[[2]]

#4
url<-"http://www.baseball-reference.com/players/g/gehrilo01.shtml"
datList[["gehrig"]]<-readHTMLTable(url)[[2]]

#5
url<-"http://www.baseball-reference.com/players/w/wagneho01.shtml"
datList[["wagner"]]<-readHTMLTable(url)[[2]]

#6
url<-"http://www.baseball-reference.com/players/m/mantlmi01.shtml"
datList[["mantle"]]<-readHTMLTable(url)[[2]]

#7
url<-"http://www.baseball-reference.com/players/c/cobbty01.shtml"
datList[["cobb"]]<-readHTMLTable(url)[[2]]

#8
url<-"http://www.baseball-reference.com/players/w/willite01.shtml"
datList[["williams"]]<-readHTMLTable(url)[[2]]

#9
url<-"http://www.baseball-reference.com/players/d/dimagjo01.shtml"
datList[["dimaggio"]]<-readHTMLTable(url)[[2]]

#10 
url<-"http://www.baseball-reference.com/players/m/mayswi01.shtml"
datList[["mays"]]<-readHTMLTable(url)[[2]]

for (i in 1:10){
datList[[i]][,1]<-gsub("★","",datList[[i]][,1])
datList[[i]]<-as.data.frame(apply(datList[[i]],2,function(x){as.numeric(as.character(x))}))
datList[[i]]$'1B'<-datList[[i]]$'H'-datList[[i]]$'HR'-datList[[i]]$'3B'-datList[[i]]$'2B'
}

names(datList)<-c("A. Pujols","B. Ruth","B. Bonds","L. Gehrig","H. Wagner","M. Mantle","T. Cobb","T. Williams","J. DiMaggio","W. Mays")


png("/Users/gregorymatthews/testIT.png",w=30,h=10,units="in",res=300)
par(mfrow=c(2,5))
for (k in 1:10){
temp<-datList[[k]]
temp[is.na(temp)]<-0
cBA<-sum(temp$H)/sum(temp$AB)
cOBP<-sum(temp$H+temp$BB+temp$HBP)/sum(temp$AB+temp$BB+temp$HBP+temp$SF)
n<-dim(temp)[1]
plot(0,0,frame.plot=F,xlim=c(-1,n+1),ylim=c(-3,43),col="white",xaxt='n',yaxt='n',xlab="",ylab="On Base Percentage",main=names(datList)[k])
#points(c(0,n-1),40*c(0,0),col=rgb(.75,.75,.75,.5),type="l")
points(c(0,n-1),40*c(.25,.25),col=rgb(0,0,0,.5),type="l",lty=3)
points(c(0,n-1),40*c(.5,.5),col=rgb(0,0,0,.5),type="l",lty=3)
points(c(0,n-1),40*c(.75,.75),col=rgb(0,0,0,.5),type="l",lty=3)
#points(c(0,n-1),40*c(1,1),col=rgb(.75,.75,.75,.5),type="l")
for (i in 1:dim(temp)[1]){
  #polygon(i-1+temp$G[i]/162*c(-.5,.5,.5,-.5),c(0,0,min(temp$H[i],40),min(temp$H[i],40)),col=rgb(1-0/200,0,0/200,.2),bor=rgb(1-0/200,0,0/200,.2))
  for (q in c(0,40,80,120,160,200)){
  if (temp$H[i]>q){polygon(i-1+temp$G[i]/162*c(-.5,.5,.5*temp$HR[i]/75,-.5*temp$HR[i]/75),c(0,0,min(40,temp$H[i]-q),min(40,temp$H[i]-q)),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))}
}
  #polygon(i-1+temp$G[i]/154*c(-.25,.25,.25,-.25),c(0,0,min(temp$HR[i],40),min(temp$HR[i],40)),col=rgb(1,0,0,.5),bor=rgb(1,0,0,.5))
  #if (temp$HR[i]>40){polygon(i-1+temp$G[i]/154*c(-.5,.5,.5,-.5),c(0,0,temp$HR[i]-40,temp$HR[i]-40),col=rgb(1,0,0,.5),bor=rgb(1,0,0,.5))}
  #BA deviations
  #if (temp$BA[i]>cBA){points(c(i,i)-1,c(temp$BA[i]*40,cBA*40),type="l",lwd=5,col="green")}
  #if (temp$BA[i]<=cBA){points(c(i,i)-1,c(temp$BA[i]*40,cBA*40),type="l",lwd=5,col="black")}
  #OBP
polygon(c(0,1:length(temp$OBP)-1,length(temp$OBP)-1),c(cOBP*40,temp$OBP*40,cOBP*40),lwd=0.1,col=rgb(.5,.5,.1,.05),bor=rgb(.5,.5,1,.05))
  if (temp$OBP[i]>cOBP){points(c(i,i)-1,c(temp$OBP[i]*40,cOBP*40),type="l",lwd=1,col="black",lty=3)}
  if (temp$OBP[i]<=cOBP){points(c(i,i)-1,c(temp$OBP[i]*40,cOBP*40),type="l",lwd=1,col="black",lty=3)}
  if (temp$OBP[i]>cOBP){points(c(i)-1,c(temp$OBP[i]*40),pch=16,col="black",cex=temp$SB[i]/40)}
  if (temp$OBP[i]<=cOBP){points(c(i)-1,c(temp$OBP[i]*40),pch=16,col="black",cex=temp$SB[i]/40)}

}

points(c(0,n-1),c(cOBP,cOBP)*40,type="l",lty=3)

points(c(0:(n))-.5,rep(40,n+1),cex=1,pch="|")
points(c(0:(n))-.5,rep(0,n+1),cex=1,pch="|")


# Function to create curly braces
# x, y position where to put the braces
# range is the widht
# position: 1 vertical, 2 horizontal
# direction: 1 left/down, 2 right/up
CurlyBraces(-1.5, 0, 1, pos = 2, direction = 2 );text(0,-3,"Games Played",cex=.5)
CurlyBraces(41.5, 0, 1, pos = 2, direction = 1 );text(0,43,"Home Runs",cex=.5)
points(n/2-1.5,-3,pch=16,cex=.5)
points(n/2-.75,-3,pch=16,cex=1)
points(n/2,-3,pch=16,cex=1.5)
text(n/2+1.5,-3,"Stolen Bases",cex=.5)

for (q in c(0,40,80,120)){
polygon(n/2+5+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

for (q in c(0,40,80,120,160)){
  polygon(n/2+5.75+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

for (q in c(0,40,80,120,160,200)){
  polygon(n/2+5+1.5+c(-.15,.15,.15,-.15),-3+c(-.35,-.35,.35,.35),col=rgb(1-q/200,0,q/200,.2),bor=rgb(1-q/200,0,q/200,.2))
}

text(n/2+5+2.25,-3,"Hits",cex=.5)
}
dev.off()
