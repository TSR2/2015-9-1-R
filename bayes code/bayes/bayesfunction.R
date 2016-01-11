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


#####################
kfail=function(a=1,b=1,n=100,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=character(n)
  p=0;d=0;i=1;count=0;ru=runif(n)
    for (j in 1:n){
      taget=rb[i]
      if(taget>=ru[j]) {p="S"
      }else {p="F"}
      list[j]=p
      if (taget<ru[j]) {
        i=i+1}
    }
  count=sum(list=="F")/n
  list(log=list,persent=count)
}

####################################
mstep=function(a,b,m,n,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=character(n);mtotal=c();p=0
  i=1;scount=0;ru=runif(n)  
    for (j in 1:n){
      if (length(mtotal)<m |scount>=m  ) { taget=rb[i]
      }else {taget=rb[which.max(mtotal)]}
      if(taget>=ru[j]) {p="S"
      scount=scount+1
      }else {p="F"}
      list[j]=p
      if (length(mtotal)==m) scount=0
      if (taget<ru[j] & length(mtotal)<m & scount<m) {
        mtotal=c(mtotal,scount)
        i=i+1
        scount=0
      }
    }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}


####################################
mreducestep=function(a,b,re,n,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=character(n);mtotal=c();p=0
  i=1;scount=0;ru=runif(n)  
  for (j in 1:n){
    if (scount>=re | scount==0) {taget=rb[i]}
    if(taget>=ru[j]) {p="S"
    scount=scount+1
    }else {p="F"}
    list[j]=p
    if (taget<ru[j] & scount<re) {
      mtotal=c(mtotal,scount)
      i=i+1
      scount=0
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}


##########################################
Nkfail=function(a,b,n,N,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=character(n);mtotal=c();p=0
  i=1;scount=0;ru=runif(n);x=1
  ru=runif(n)
    for (j in 1:n){
      if (x==1) taget=rb[i]
      if (j>=N & p=="F" & x==1) {
        taget=rb[which.max(mtotal)]
        x=0}
      if (taget>=ru[j]) {
        p="S"
        scount=scount+1
      }else {p="F"}
      list[j]=p
      if (taget<ru[j] & x==1) {
        mtotal=c(mtotal,scount)
        i=i+1
        scount=0
      }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}


###########################################
ifail=function(a=1,b=1,n=100,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=character(n);p=0
  i=1;ru=runif(n);Fcount=0
  ru=runif(n)
    for (j in 1:n){
      taget=rb[i]
      if (taget>=ru[j]) {p="S"
      }else {p="F"
      Fcount=Fcount+1}
      list[j]=p
      if (Fcount==i) {
        i=i+1
        Fcount=0
      }
    }
  persent=sum(list=="F")/n
  list(list,persent)
}


##########################################
runs=function(run,fun,...){
  v=rep(0,run)
  for (i in 1:run){
    v[i]=fun(...)[[2]]  
  }
  mean(v)
}


#############################################
bw=function(a,b,x,u){
  expression( x^(a-1)*(1-x)^(b-1))
}

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

TL=function(x,a,j){
  x^j*2*a*(1 - x)*x^(a-1)*(2 - x)^(a-1)
}

aaa=function(a,j){
  integrate(TL,lower = 0,upper = 1,a=a,j=j)[[1]]
}


calculateN=function(a,b,n,m){
  bb=beta(a=a+(0:(n-1)),b=b)
  N=(sum(bb)*m)/beta(a,b)
}

L=function(a,b){
  gamma(a+b)/(gamma(a)*gamma(b+1))
}

calculateRE=function(a,b,n){
  round((n*L(a,b)*gamma(1+b))^(1/(1+b)))
}

calculateM=function(a,b,n){
  round(((n*gamma(1+1/b))/(b*L(a,b)^(1/b)))^(b/(1+b)))
}

calculateN=function(a,b,n,m){
  bb=beta(a=a+(0:(n-1)),b=b)
  N=(sum(bb)*m)/beta(a,b)
  round(N)
}

calculateTLRn=function(a,n){
  round((2*n*a)^(1/3))
}

calculateTLKn=function(a,n){
  kn=((n*sqrt(pi))/(4*sqrt(a)))^(2/3)
  floor(kn)
}

calculateTLNn=function(a,n){
  k=calculateTLKn(a,n)
  k=floor(k)
  N=k*sum(sapply(0:(n-1),aaa,a=a))
  floor(N)
}

allrun=function(run=100,a=1,b=1,n=200,alpha=.1,t=1,m=4,N=50,re=4){ 
  pp=c()
  pp[1]=runs(run = run,fun = kfail,a=a,b=b,n=n,alpha=alpha,t=t)
  pp[2]=runs(run = run,fun = mstep,a=a,b=b,n=n,alpha=alpha,t=t,m=m)
  pp[3]=runs(run = run,fun = mreducestep,a=a,b=b,n=n,alpha=alpha,t=t,re=re)
  pp[4]=runs(run = run,fun = Nkfail,a=a,b=b,n=n,alpha=alpha,t=t,N=N)
  pp[5]=runs(run = run,fun = ifail,a=a,b=b,n=n,alpha=alpha,t=t)
  names(pp)=c('kfail','mstep','mreduce','Nkfail','ifail')
  pp
}

