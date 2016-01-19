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
#####################################################簡單一般pca測試


test1=kmeans(iris[,1:4],centers = 3)
test1 %>% names()
test1$centers
p=iris[,1:4] %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=rep(1:3,each=50))

dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
des_e=function(x,b=10){
  ymin=min(x[,1])
  ymax=max(x[,1])
  xmin=min(x[,2])
  xmax=max(x[,2])
  
  yrange=seq(ymin,ymax,length.out = b+1)
  xrange=seq(xmin,xmax,length.out = b+1)
  
  qq1=matrix(0,nrow=b,ncol=b)
  for (j in 1:b){
    for(i in 1:b){
      qq1[i,j]=sum(x[,1]>=yrange[j] & x[,1]<=yrange[j+1] & x[,2]>=xrange[i] & x[,2]<=xrange[i+1])
    }
  }
  qq1
}

image(des_e(test2,b=10))
