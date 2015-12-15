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



library(LearnBayes)
 midpt = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
curve(histprior(x,midpt,prior), from=0, to=1,
        ylab="Prior density",ylim=c(0,.3))

curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),
      from=0, to=1, ylab="Posterior density")

p = seq(0, 1, by = 0.125)
post = histprior(p, midpt, prior) *
  dbeta(p, s+1, f+1)
post = post/sum(post)
ps = sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="")




##########################################################ex1
p = seq(0,1, by = 0.125)
prior = c(.001 ,.001 ,.950 ,.008 ,.008 ,.008 ,.008 ,.008 ,.008)
prior = prior/sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")
library(LearnBayes)
data = c(6, 4)
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




############################ex2
par(mfrow=c(1,3))
library(LearnBayes)
midpt = seq(0,1, by = 0.1)
prior = c(0.01, 0.01, 0.02, 0.3, 0.3,0.6, 0.3, 0.02, 0.01, 0.01,0.01)
prior = prior/sum(prior)
curve(histprior(x,midpt,prior), from=0, to=1,
      ylab="Prior density",ylim=c(0,.3))
s=5
f=15
curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),
      from=0, to=1, ylab="Posterior density")

p = seq(0, 1, by = 0.1)
post = histprior(p, midpt, prior) *
  dbeta(p, s+1, f+1)
post = post/sum(post)
ps = sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="")





####################################
ab=c(3.26, 7.19)
m=20; ys=0:20
pred=pbetap(ab, m, ys)

par(mfrow=c(1,1))
p=rbeta(1000, 3.26, 7.19)
y = rbinom(1000, 20, p)
table(y)
freq=table(y)
ys=as.integer(names(freq))
predprob=freq/sum(freq)
plot(ys,predprob,type="h",xlab="y",
        ylab="Predictive Probability")
sum(predprob[1:12])

####################################

qbeta(c(0.05,0.95),shape1 = 23,shape2 = 8)
for (i in seq(0.6,0.8,length.out = 30)){
  print(c(i,pbeta(i,shape1 = 23,shape2 = 8)))
}

p=rbeta(1000,shape1 = 23,shape2 = 8)
y = rbinom(1000, 10, p)
fr=table(y)
sum(fr[7:8])/sum(fr)

#############################q6
lambda=c(.5, 1, 1.5, 2, 2.5,3)

midpt = c(.5, 1, 1.5, 2, 2.5,3)
prior=c(.1, .2, .3, .2, .15,0.05)

aaa=function(g,t,lamb,y){
  g*exp(-t*lamb)*(t*lamb)^y
}

#######因為天數是六天，次數是12
post=aaa(prior,6,midpt,12)
post=post/sum(post)

ddd=cbind(midpt,prior,post)
ddd

####q6b
bpost=aaa(prior,7,midpt,0)

bpost =bpost/sum(bpost)
kk=bpost*exp(-7*midpt)
kk %>% sum()



s=5
f=15
curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),
      from=0, to=1, ylab="Posterior density")

p = seq(0, 1, by = 0.1)
post = histprior(p, midpt, prior) *
  dbeta(p, s+1, f+1)
post = post/sum(post)
ps = sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="")

data = c(11, 16)
post = pdisc(p, prior, data)
round(cbind(p, prior, post),2)

########ch3
data(footballscores)
xx=footballscores
head(xx)
attach(xx)
d = favorite - underdog - spread
n = length(d)
v = sum(d^2)

p = rchisq(1000, n)/v
s = sqrt(1/p)
hist(s,main="")

quantile(s, probs = c(0.025, 0.5, 0.975))

#p43
alpha=16;beta=15174
yobs=1; ex=66
y=0:10
lam=alpha/beta
py=dpois(y, lam*ex)*dgamma(lam, shape = alpha,
                            rate = beta)/dgamma(lam, shape= alpha + y,
                            rate = beta + ex)
cbind(y, round(py, 3))

lambdaA = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

ex = 1767; yobs=4
y = 0:10
py = dpois(y, lam * ex) * dgamma(lam, shape = alpha,
                                 rate = beta)/dgamma(lam, shape = alpha + y,
                                rate = beta + ex)
cbind(y, round(py, 3))
lambdaB = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

par(mfrow = c(2, 1))
plot(density(lambdaA), main="HOSPITAL A",
       xlab="lambdaA", lwd=3)

curve(dgamma(x, shape = alpha, rate = beta), add=TRUE)

legend("topright",legend=c("prior","posterior"),lwd=c(1,3))

plot(density(lambdaB), main="HOSPITAL B",
        xlab="lambdaB", lwd=3)
curve(dgamma(x, shape = alpha, rate = beta), add=TRUE)
legend("topright",legend=c("prior","posterior"),lwd=c(1,3))

quantile1=list(p=.5,x=100); quantile2=list(p=.95,x=120)
normal.select(quantile1, quantile2)


mu = 100
tau = 12.16
sigma = 15
n = 4
se = sigma/sqrt(4)
ybar = c(110, 125, 140)
tau1 = 1/sqrt(1/se^2 + 1/tau^2)
mu1 = (ybar/se^2 + mu/tau^2) * tau1^2
summ1=cbind(ybar, mu1, tau1)
summ1

tscale = 20/qt(0.95, 2)
tscale

par(mfrow=c(1,1))
curve(1/tscale*dt((x-mu)/tscale,2),
      from=60, to=140, xlab="theta", ylab="Prior Density")
curve(dnorm(x,mean=mu,sd=tau), add=TRUE, lwd=3)
legend("topright",legend=c("t density","normal density"),
         lwd=c(1,3))

norm.t.compute=function(ybar) {
  theta = seq(60, 180, length = 500)
  like = dnorm(theta,mean=ybar,sd=sigma/sqrt(n))
  prior = dt((theta - mu)/tscale, 2)
  post = prior * like
  post = post/sum(post)
  m = sum(theta * post)
  s = sqrt(sum(theta^2 * post) - m^2)
  c(ybar, m, s) }
summ2=t(sapply(c(110, 125, 140),norm.t.compute))
dimnames(summ2)[[2]]=c("ybar","mu1 t","tau1 t")
summ2