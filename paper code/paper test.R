x=hist(iris[,1])
x
class(x)
b=list()
b[[1]]=1:9
b[[2]]=1:8
b[[3]]=(b$counts/diff(b$breaks))/sum(b$counts)
b[[4]]=(b$breaks[1:8]+b$breaks[2:9])/2
names(b)=c("a","b")
name=names(x)
names(b)=name[1]
names(b)=name[1:2]
names(b)=name[1:3]
names(b)=name[1:4]
class(b)="histogram"
plot(b)

(x$counts/diff(x$breaks))/sum(x$counts)

total=data2

x=data2[[1]][[1]]

####################################################### me


hcalEX(data2)
hcalvar(data2)
sqrt(hcalvar(data2))
  
#########################################package test
pp=c(0,cumsum(a2))
library(HistDAWass)
mydist<-distributionH(x=a1, p=pp)
meanH(mydist)
stdH(mydist)

meanH(hist(iris[,1]))
#############################################teacher code
n=p=1
Bij=10
mu <- numeric(p)
for(j in 1: p){
  for(i in 1:n){                                            
    px <- sum(a2*(a1[1:Bij]+
                    a1[2:(Bij+1)]))
    mu[j] <- mu[j] + px
  }         
  mu[j] <- mu[j]/(2*n)
}
mu


mu <- numeric(p)
s2 <- numeric(p)
for(j in 1: p){
  for(i in 1:n){                            
    ## mean            
    px <- sum(a2*(a1[1:Bij]+
                    a1[2:(Bij+1)]))
    mu[j] <- mu[j] + px
    
    ## var
    px <- sum(a2*(a1[1:Bij]^2+
                    a1[2:(Bij+1)]^2+
                    a1[1:Bij]*
                    a1[2:(Bij+1)]))            
    s2[j] <- s2[j] + px
  }         
  mu[j] <- mu[j]/(2*n)
  s2[j] <- s2[j]/(3*n)-mu[j]^2
}

list(mu=mu, s2=s2)
#####################################################



library(gregmisc)


image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, axis.pos=1, add.axis=TRUE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  if(axis.pos %in% c(1,3)){ylim<-c(0,1); xlim<-range(breaks)}
  if(axis.pos %in% c(2,4)){ylim<-range(breaks); xlim<-c(0,1)}
  plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
  box()
  if(add.axis) {axis(axis.pos)}
}



