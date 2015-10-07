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


#############################################teacher code
n=p=1
Bij=8
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
