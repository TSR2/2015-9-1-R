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


b=matrix(c(rep(1:4,)),ncol=2,nrow=4)
cor(b)



total=data2

x=data2[[1]][[1]]

#######################################################
data1=data2
  p=length(data1)
  calEX2=function(x){
    b=length(x$count)
    parta=x$count*(x$breaks[1:b]^2  
                  +  x$breaks[1:b]*x$breaks[2:(b+1)]  
                  +  x$breaks[2:(b+1)]^2)
    sum(parta)
  }
  n=length(data1[[1]])
  EX2=c()
  for (j in 1:p){
    m=sum(sapply(data1[[j]],calEX2))/(3*n)
    EX2=c(EX2,m)
  }
  EX2
  mu=hcalEX(data1)
  var=EX2-mu^2
  var

  
  
  a1=b1
  a2=b2
  
  pp=c(0,cumsum(a2))
  
  library(HistDAWass)
  mydist<-distributionH(x=a1, p=pp)
  meanH(mydist)
  stdH(mydist)
  
  