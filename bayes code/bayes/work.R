ttest=c("kfail",'mstep','mreducestep','Nkfail','ifail')
        
runs(run = 100,fun = eval(parse(text="kfail")),a=1,b=1,n=200,t=0)

n=1000;a=0.9
rn=(2*n*a)^(1/3)
kn=((n*sqrt(pi))/(4*sqrt(a)))^(2/3)
kn=floor(kn)
Nn=kn*sum(sapply(0:(n-1),aaa,a=0.9))


library(LearnBayes)


#################test
f=createbeta(n = 2000,v = 0.2)
mf=mean(f)
vf=var(f)

ff=function(x,mf=mf,vf=bvar){
  y=2
  (x/(x+y)-mf)^2+((x*y)/((x+y)^2*(x+y+1))-vf)^2
}
ll=uniroot(ff,mf=mf,vf=vf,interval = c(0,10),tol = 1e-10)
ll

b1=ll[[2]][1]
b2=ll[[2]][2]
b1
b2


######求 meanTL=meanBETA ,varTL=varBETA 時的 a,b

c(0.05240259,.1,.15,.02,.03) #v=0.5
c(0.04310766,.07,.1,.02,.03) #v=0.3
c(0.01681744,.03,.04,.01,.005) #v=0.1
varlist=list(v1=c(0.01681744,.03,.04,.01,.005),
             v3=c(0.04310766,.07,.1,.02,.03),
             v5=c(0.05240259,.1,.15,.02,.03),
             v9=c(0.05702166,.1,.18,.02,.03))
aa=c(.1,.3,.5,.9)
n=1500
alllast=list()
for (j in 1:4){
  a=aa[j]
  test=data.frame()
  for (i in varlist[[j]] ) {
    f=createbeta(n = 2000,v = j)
    mf=mean(f)
    vf=var(f)
    bvar=i
    ff=function(z,mf=mf,vf=bvar){
      x=z[1]
      y=z[2]
      (x/(x+y)-mf)^2+((x*y)/((x+y)^2*(x+y+1))-vf)^2
    }
    ll=nlm(ff,mf=mf,vf=bvar,p = c(1,2),gradtol = 1e-20)
    b1=ll[[2]][1]
    b2=ll[[2]][2]
    vf
    b1
    b2
    
    m=calculateM(a=b1,b=b2,n=n)
    N=calculateN(a=b1,b=b2,n=n,m=m)
    re=calculateRE(a=b1,b=b2,n=n)
    a1=allrun(run=500,n=n,a=b1,b=b2,t=0,m=m,N=N,re=re)
    a1
    
    rn=calculateTLRn(a=a,n=n)
    kn=calculateTLKn(a=a,n=n)
    Nn=calculateTLNn(a=a,n=n)
    a2=allrun(run=500,n=n,alpha=a,t=1,m=kn,N=Nn,re=rn)
    a2
    
    test1=rbind(a1,a2)
    a3=matrix(c('Beta','TL',paste('a=',round(b1,2),',','b=',round(b2,2),sep='')
                ,a,n,n,'=','=',round(bvar,3),round(vf,3))
      ,byrow = F,ncol=5)
    colnames(a3)=c('distributed','parameter','n','EX','Var')
    a4=cbind(a3,test1)
    a4
  
    test=rbind(test,a4)
  }
  alllast[[j]]=test
}


n1500=alllast
n1000

n500

test



tt1=function(x){ 
  x=as.character(x) %>% as.numeric()
  round(x,digits = 4)
}

trans=function(x){  
  test1500=do.call(rbind,x)
  kk1=test1500 %>% colwise(tt1,.cols = 6:10)(.)
  cbind(test1500[,1:5],kk1)
}


final500=trans(n500)
final1000=trans(n1000)
final1500=trans(n1500)


library(xtable)
final500 %>% dim
final1000 %>% dim
final1500 %>% dim
names(final500)[6:10]=c('1-fa','m-run','non','Nlearn','i')
names(final1000)[6:10]=c('1-fa','m-run','non','Nlearn','i')
names(final1500)[6:10]=c('1-fa','m-run','non','Nlearn','i')
row.names(final500)=1:40
row.names(final1000)=1:40
row.names(final1500)=1:40
final500$group=rep(1:4,each=10)
final1000$group=rep(1:4,each=10)
final1500$group=rep(1:4,each=10)
xtable(final500,digits=4)
xtable(final1000,digits=4)
xtable(final1500,digits=4)


