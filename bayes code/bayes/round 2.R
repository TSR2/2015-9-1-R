library(plyr)
library(dplyr)
library(magrittr)

xx=read.table("C:/Users/tsr/Desktop/上課用檔案/貝氏計算/studentdata.txt",header=T)

head(xx)
xx %>% dim()


xx$hours.of.sleep = xx$WakeUp - xx$ToSleep
plot(jitter(xx$ToSleep),jitter(xx$hours.of.sleep))


tstatistic=function(x,y)
{
  m=length(x)
  n=length(y)
  sp=sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
  t.stat=(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t.stat)
}

m=10; n=10
my.tsimulation=function(){
  tstatistic(rnorm(m,mean=10,sd=2), rexp(n,rate=1/10))
}

tstat.vector=replicate(10000, my.tsimulation())

plot(density(tstat.vector),xlim=c(-5,8),ylim=c(0,.4),lwd=3)
curve(dt(x,df=18),add=TRUE)
legend(4,.3,c("exact","t(18)"),lwd=c(3,1))

binom.test(0.4,100,0.5,conf.level = 0.9)

binomial.conf.interval=function(y,n)
{
  z=qnorm(.95)
  phat=y/n
  se=sqrt(phat*(1-phat)/n)
  return(c(phat-z*se,phat+z*se))
}
dd=rbinom(n=20,1,prob=.5)

inter=binomial.conf.interval(sum(dd),20)
a=c()
for (i in 1:100){
  dd1=rbinom(n=20,1,prob=.5)
  a[i]=mean(dd1)
}

sum(a<=inter[2] & a>=inter[1])




asd=function(n,p,m){
  dd=rbinom(n=m,1,prob=.5)
  inter=binomial.conf.interval(sum(dd),m)
  a=c()
  for (i in 1:10000){
    dd1=rbinom(n=m,1,prob=p)
    a[i]=mean(dd1)
  }
}

#####chapter2

p = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")
library(LearnBayes)
data = c(11, 16)
post = pdisc(p, prior, data)
round(cbind(p, prior, post),2)

library(lattice)
PRIOR=data.frame("prior",p,prior)
POST=data.frame("posterior",p,post)
names(PRIOR)=c("Type","P","Probability")
names(POST)=c("Type","P","Probability")
data=rbind(PRIOR,POST)
xyplot(Probability~P|Type,data=data,layout=c(1,2),
          type="h",lwd=3,col="black")




a = 3.26
b = 7.19
s = 11
f = 16
curve(dbeta(x,a+s,b+f), from=0, to=1,
        + xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,s+1,f+1),add=TRUE,lty=2,lwd=4)
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
legend(.7,4,c("Prior","Likelihood","Posterior"),
         lty=c(3,2,1),lwd=c(3,3,3))

