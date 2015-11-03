########################???ճt?ץ?
Rprof()
invisible(  mstep(a=1,b=1,m=31,n=1000)  )
Rprof(NULL)
summaryRprof()
###############################################??

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
  a=kfail(a=1,b=1,n=200,t=1)$persent
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
mstep=function(a,b,m,n,alpha=0.1,t=1){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
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
      ###?p?G?????u?????Ƥj??m,?h???X???\?v?̰??????U?h
      if (length(mtotal)==m) {taget=rb[which.max(mtotal)]} 
      ru=runif(n)
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      if (taget>=ru[j]) p="S"
      else p="F"
      ###?????C???????????G
      list=c(list,p)
      ###?֭p?Ӥ??u???\??????
      if (taget>=ru[j]) {scount=scount+1}
      ####?ϥΪ????u?bm?ӥH?U?A?ӥB?Ӧ??????O???Ѫ??A?O???U?Ӧ??????????\?`????
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
  v[i]=mstep(a=1,b=1,m=44,t=1,n=200)[[2]]  
}
mean(v)

totallist[[2]]

#################################### reduce m run

mreducestep=function(a,b,m,n,alpha=0.1,t=1){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
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
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      p=ifelse(test = taget>=ru[j],"S","F")
      ###?????C???????????G
      list=c(list,p)
      ###?֭p?Ӥ??u???\??????
      if (taget>ru[j]) {
        scount=scount+1
      }else{
        ####?ϥΪ????u?bm?ӥH?U?A?ӥB?Ӧ??????O???Ѫ??A?O???U?Ӧ??????????\?`????
        mtotal=c(mtotal,scount)
        ######???u???\?Ƥ???m?A?????u
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
  v[i]=mreducestep(a=1,b=1,t=1,m=4,n=200)[[2]]  
}
mean(v)


#跑???次
###############################################################

ttest=list(c(200,4),c(500,5),c(1000,6))
ttest=list(c(100,4),c(100,5),c(100,6))
bb=c()
run=100
v=rep(0,run)
for (j in ttest){  
  for (i in 1:run){
    v[i]=mreducestep(a=1,b=1,t=1,m=j[2],n=j[1])[[2]]  
  }
  bb=c(bb,mean(v))
}
bb

##################################### N
Nkfail=function(a,b,n,N,alpha=0.1,t=1){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
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

v=rep(0,500)
for (i in 1:500){
  v[i]=Nkfail(a=1,b=1,n=1000,N=31,t=0)[[2]]  
}
mean(v)






###################################### new N
Nkfail=function(a,b,n,N,alpha=0.1,t=1){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  mtotal=c()
  p=0
  count=0
  #####x=1?O?N???٥????????????????u,x=0?O?N???????F
  x=1
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      #####?b?j??N?H???A?Ĥ@???X?{F?A?P?_???\?v?̰??????u
      if (length(mtotal)>=N & p=="F" & x==1) {taget=rb[which.max(mtotal)];x=0}
      ru=runif(n)
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      if (taget>=ru[j]) {
        p="S"
        scount=scount+1
      }
      else p="F"
      ###?????C???????????G
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

g=calculateN(1,1,100,9)
v=rep(0,500)
for (i in 1:500){
  v[i]=Nkfail(a=1,b=1,n=1000,alpha = 0.9,N=31,t=0)[[2]]  
}
mean(v)


#########################################run function
runs=function(run,fun,...){
  v=rep(0,run)
  for (i in 1:run){
    v[i]=fun(...)[[2]]  
  }
  mean(v)
}
runs(100,fun=Nkfail,a=1,b=1,n=1000,N=31,t=0)



#############################?p??N??
a=1;b=1;n=100;m=9;
calculateN=function(a,b,n,m){
  bb=beta(a=a+(0:(n-1)),b=b)
  N=(sum(bb)*m)/beta(a,b)
}

g=calculateN(1,1,100,9)

###403??
###1.m-run
###2.N-learning
###3.non recalling m-run (un=m)
####?????̪?mn?O?ڭn?Ϊ?m??
####Kn=m

##############################################

bw=function(a,b,x,u){
  expression( x^(a-1)*(1-x)^(b-1))
}
bw

beint=function(a=0.1,b=1,u=1/2){
  integrate(f= function(x) x^(a-1)*(1-x)^(b-1) ,lower=0,upper=u)
}
############################calculate TL rn,kn,Nn
EX=function(a){
  1-4^a*(gamma(1+a)^2/gamma(2+2*a))
}
EX2=function(a){
  (2+a/1+a)-2^(1+2*a)*(gamma(1+a)^2/gamma(2+2*a))
}
betamean=function(a,b){
  a/(a+b)
}
betavar=function(a,b){
  (a*b)/((a+b)^2*(a+b+1))
}
sol=function(a,b,aphla){
  betamean(a,b)-EX(aphla)
}

uniroot(f = sol,a=1,aphla=0.9,interval = c(0,5),tol=0.001)

TL=function(x,a,j){
  x^j*2*a*(1 - x)*x^(a-1)*(2 - x)^(a-1)
}
aaa=function(a,j){
  integrate(TL,lower = 0,upper = 1,a=a,j=j)[[1]]
}

n=100;a=0.9
rn=(2*n*a)^(1/3)
kn=((n*sqrt(pi))/(4*sqrt(a)))^(2/3)
kn=floor(kn)
Nn=kn*sum(sapply(0:(n-1),aaa,a=0.9))

################################################

EXJ=function(j=2,a=0.1){
  EXJ=2^(j+2*a)*a*(beint(a=j+a,b=a,u=1/2)[[1]]-2*beint(a=1+j+a,b=a,u=1/2)[[1]])
}
x=EXJ()
x

integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)

################################################################
k=ifail(a=1,b=1,n=100)
runs(run = 500,fun = ifail,a=1,b=1,n=100)

f=createbeta(n = 200,v = 0.9)
mf=mean(f)
vf=var(f)
######求 meanTL=meanBETA ,varTL=varBETA 時的 a,b
ff=function(z){
  x=z[1]
  y=z[2]
  (x/(x+y)-mf)^2+((x*y)/((x+y)^2*(x+y+1))-vf)^2
}
ll=nlm(ff,p = c(1,2),gradtol = 1e-20)
jj=optim(c(1,2),ff)
b1=ll[[2]][1]
b2=ll[[2]][2]
mf
b1/(b1+b2)
vf
(b1*b2)/((b1+b2)^2*(b1+b2+1))
###meanTL=meanBETA ,varTL=varBETA
runs(run = 200,fun = mstep,a=b1,b=b2,m=kn,n=500)
runs(run = 200,fun = mstep,alpha=0.9,m=kn,n=500,t=1)


bb=function(a,b){
  a/(a+b)-mf
}
a=0.5
mm=uniroot(bb,c(0,10),a=a)
mm$root
betavar(a,mm$root)
vf
##varTL<varBETA
runs(run = 200,fun = mstep,a=0.5,b=mm$root,m=kn,n=500)
runs(run = 200,fun = mstep,alpha=0.9,m=kn,n=500,t=1)

##varTL>varBETA
a=2
mm=uniroot(bb,c(0,10),a=a)
mm$root
betavar(a,mm$root)
vf

runs(run = 200,fun = mstep,a=a,b=mm$root,m=kn,n=500)
runs(run = 200,fun = mstep,alpha=0.9,m=kn,n=500,t=1)