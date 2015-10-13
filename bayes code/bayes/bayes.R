########################測試速度用
Rprof()
invisible(  mstep(a=1,b=1,m=31,n=1000)  )
Rprof(NULL)
summaryRprof()
###############################################ˇ


######一元方程求解
f1=function(x,a,b) {a+b*x}
a=5
b=10
pp=uniroot(f1,c(-10,10),a=a,b=b,tol=0.00001)
pp
###########

createbeta=function(n=200,v=0.1){
  ff=function(x,v,uni){x^v*(2-x)^v-uni}
  bb=0
  uu=0
  for (i in 1:n){
    uni=runif(1)
    uu[i]=uni
    pp=uniroot(ff,c(0,1),v=v,uni=uni,tol=0.00000000000000000000001)
    bb[i]=pp[[1]]
  }
  rb=bb
}
rb=createbeta(200,0.1)
ff(x=rb,v=0.1,uni=uu)


plot(rb)
#######  1 failure strategies for
######### t=1 is TL disbution
######### t=0 is beta disbution
kfail=function(a=1,b=1,n=100,alpha=0.1,t=1){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      p=ifelse(test = taget>=ru[j],"S","F")
      list=c(list,p)
      if (taget<ru[j]) {break}
    }
  }
  list
  count=sum(list=="F")/n
  list(log=list,persent=count)
}
v=0
for (i in 1:100){
  a=kfail(a=1,b=1,n=200,t=0)$persent
  v=v+a
}
v/100
########################################## k failrue parllce
library(magrittr)
library(dplyr)
detet=function(x,n=100){
  ru=runif(n)
  v=ifelse(x>=ru,"s","f")
}

park=function(a,b,n){
  rb=rbeta(n,a,b)
  total=sapply(rb,FUN = detet)
}

n=100
f=park(1,1,100)
f=t(f)
dd=which(f=="f",arr.ind = T)
dd=as.data.frame(dd)
cc=dd %>% group_by(row) %>% summarise(min=min(col))
vv=cumsum(cc$min)
names(vv) <- 1: 100
mm=names(vv[vv>=100])[1]
mm=as.numeric(mm)
(mm-1)/n
#######################################

############# m strategies

a=1;b=1;m=3;n=100
mstep=function(a,b,m,n){
  rb=rbeta(n = n,shape1 = a,shape2 = b)
  list=c()
  mtotal=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ###如果換手臂的次數大於m,則找出成功率最高的做下去
      if (length(mtotal)==m) {taget=rb[which.max(mtotal)]} 
      ru=runif(n)
      ###從beta分配抽出來的如果比uniform大，則給S,反之給F
      if (taget>=ru[j]) p="S"
      else p="F"
      ###收集每次的實驗結果
      list=c(list,p)
      ###累計該手臂成功的次數
      if (taget>=ru[j]) {scount=scount+1}
      ####使用的手臂在m個以下，而且該次實驗是失敗的，記錄下該次實驗的成功總次數
      if (taget<ru[j] & length(mtotal)<m) {
        mtotal=c(mtotal,scount)
        break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}


v=rep(0,1000)
for (i in 1:1000){
  v[i]=mstep(a=1,b=1,m=31,n=1000)[[2]]  
}
mean(v)

totallist[[2]]
rm(list=ls())
#################################### reduce m run

mreducestep=function(a,b,m,n){
  rb=rbeta(n = n,shape1 = a,shape2 = b)
  list=c()
  mtotal=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      ###從beta分配抽出來的如果比uniform大，則給S,反之給F
      p=ifelse(test = taget>=ru[j],"S","F")
      ###收集每次的實驗結果
      list=c(list,p)
      ###累計該手臂成功的次數
      if (taget>ru[j]) {
        scount=scount+1
      }else{
        ####使用的手臂在m個以下，而且該次實驗是失敗的，記錄下該次實驗的成功總次數
        mtotal=c(mtotal,scount)
        ######手臂成功數不足m，換手臂
        if (scount<m) break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}

mreducestep(a=1,b=1,m=9,n=100)


v=rep(0,1000)
for (i in 1:1000){
  v[i]=mreducestep(a=1,b=1,m=9,n=100)[[2]]  
}
mean(v)



##################################### N
Nkfail=function(a,b,n,N){
  rb=rbeta(n = n,shape1 = a,shape2 = b)
  list=c()
  p=0;count=0
  for (i in 1:n){
    taget=rb[i]
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      p=ifelse(test = taget>=ru[j],"S","F")
      list=c(list,p)
      if (length(list)==N & p=='S') {next}
      if (taget<ru[j] & length(list)<N) {break}
    }
  }
  list
  count=sum(list=="F")/n
  list(log=list,persent=count)
}
Nkfail(a=1,b=1,n=100,N=50)

v=rep(0,1000)
for (i in 1:1000){
  v[i]=Nkfail(a=1,b=1,n=100,N=50)[[2]]  
}
mean(v)
###################################### new N
Nkfail=function(a,b,n,N){
  rb=rbeta(n = n,shape1 = a,shape2 = b)
  list=c()
  mtotal=c()
  p=0
  count=0
  #####x=1是代表還未找到做到底的手臂,x=0是代表找到了
  x=1
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      #####在大於N以後，第一次出現F，判斷成功率最高的手臂
      if (length(mtotal)>=N & p=="F" & x==1) {taget=rb[which.max(mtotal)];x=0}
      ru=runif(n)
      ###從beta分配抽出來的如果比uniform大，則給S,反之給F
      if (taget>=ru[j]) {
        p="S"
        scount=scount+1
      }
      else p="F"
      ###收集每次的實驗結果
      list=c(list,p)
      if (taget<ru[j] & x==1) {
        mtotal=c(mtotal,scount)
        break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}

Nkfail(a=1,b=1,n=100,N=50)

v=rep(0,1000)
for (i in 1:1000){
  v[i]=Nkfail(a=1,b=1,n=100,N=50)[[2]]  
}
mean(v)



#############################計算N值
a=1;b=1;n=100;m=9;
calculateN=function(a,b,n,m){
  bb=beta(a=a+(0:(n-1)),b=b)
  N=(sum(bb)*m)/beta(a,b)
}

###403頁
###1.m-run
###2.N-learning
###3.non recalling m-run (un=m)
####表格裡的mn是我要用的m值
####Kn=m

##############################################

bw=function(a,b,x,u){
  expression( x^(a-1)*(1-x)^(b-1))
}
bw
?integrate

beint=function(a=0.1,b=1,u=1/2){
  integrate(f= function(x) x^(a-1)*(1-x)^(b-1) ,lower=0,upper=u)
}

EX=1-4^a*(gamma(1+a)^2/gamma(2+2*a))
EX


EXJ=function(j=2,a=0.1){
  EXJ=2^(j+2*a)*a*(beint(a=j+a,b=a,u=1/2)[[1]]-2*beint(a=1+j+a,b=a,u=1/2)[[1]])
}
x=EXJ()
x



integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)

ex=function()
  
  ?integrate




################################################################
list=list(1:10)
list

list=c(list,list(1:20))
list


y=c(10,11,14,13)
x=cbind(rep(1,4),1:4)
b=solve(t(x) %*% x) %*% t(x) %*%y
x-1
b
x
lm(y~x)
lm(y~x-1)

plot(dnorm(seq(-3,3,by=0.1)),type="l",axes = F)
axis(1,at=seq(1,61,by=10),label=seq(-3,3,by=1))
