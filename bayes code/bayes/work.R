ttest=c("kfail",'mstep','mreducestep','Nkfail','ifail')
        
runs(run = 100,fun = eval(parse(text="kfail")),a=1,b=1,n=200,t=0)

n=1000;a=0.9
rn=(2*n*a)^(1/3)
kn=((n*sqrt(pi))/(4*sqrt(a)))^(2/3)
kn=floor(kn)
Nn=kn*sum(sapply(0:(n-1),aaa,a=0.9))


library(LearnBayes)

######求 meanTL=meanBETA ,varTL=varBETA 時的 a,b

f=createbeta(n = 200,v = 0.9)
mf=mean(f)
vf=var(f)
ff=function(z){
  x=z[1]
  y=z[2]
  (x/(x+y)-mf)^2+((x*y)/((x+y)^2*(x+y+1))-vf)^2
}
ll=nlm(ff,p = c(1,2),gradtol = 1e-20)
b1=ll[[2]][1]
b2=ll[[2]][2]

ggg=proc.time()
runs(run = 1000,fun = kfail,a=1,b=1,n=100,t=0)
proc.time()-ggg

ggg=proc.time()
runs(run = 1000,fun = mstep,a=1,b=1,m=9,n=100,t=0)
proc.time()-ggg

ggg=proc.time()
runs(run = 1000,fun = mreducestep,a=1,b=1,m=9,n=100,t=0)
proc.time()-ggg

ggg=proc.time()
runs(run = 1000,fun = Nkfail,a=1,b=1,n=100,alpha=0.9,N=g,t=0)
proc.time()-ggg

ggg=proc.time()
runs(run = 1000,fun = ifail,a=1,b=1,n=100,t=0)
proc.time()-ggg



runs(run = 1000,fun = mstep,a=1,b=1,m=rn,n=100,t=0)
runs(run = 100,fun = mreducestep,a=1,b=1,m=kn,n=100,t=0)
runs(run = 100,fun = Nkfail,a=1,b=1,n=1000,alpha=0.9,N=Nn,t=1)
runs(run = 100,fun = ifail,a=1,b=1,n=100,t=0)
 
runs(run = 100,fun = kfail,a=1,b=1,n=200,t=1)
runs(run = 100,fun = mstep,a=1,b=1,m=4,n=200,t=1)
runs(run = 100,fun = mreducestep,a=1,b=1,m=4,n=200,t=1)
runs(run = 100,fun = Nkfail,a=1,b=1,n=200,N=50,t=1)
runs(run = 100,fun = ifail,a=1,b=1,n=200,t=1)




if(1>2) {p="S"
}else {p="F"}