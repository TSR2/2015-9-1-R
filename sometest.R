library(magrittr)
library(plyr)
library(dplyr)


dd=function(x){x^2-x}
ff=function(x){x-1}
plot(dd,xlim = c(-1,2))
lines(c(-10,10),c(0,0),col="red")
lines(c(0,0),c(-10,1100),col="red")
plot(ff,add=T,xlim=c(-2,100))

optimize(dd,c(-10,10))
library(ggplot2)

x=-10:10
y1=dd(x)
y2=ff(x)
jj='C:\\Users\\TSR\\Desktop\\test\\'
for (i in 1:500){
  png(paste(jj,i,".png"))
  hist(y)
  dev.off()
}
for (i in 1:500){
  png(paste(jj,i,".png"))
  qplot(y,geom = "histogram")
  dev.off()
}
qplot(y,geom = "histogram") 
 qplot(x=x,y=y2,geom = "line")

 
 x=iris
 na=names(x[,1:3])
 kk=paste(na,sep="",collapse = "+")
 kk=paste("Petal.Width",kk,sep="~")
 lm(as.formula(kk),data=x)
x=1:5
y=5:10
union(x,y)
?union
setequal(x,y)

 
kk=vector("list",3)
kk
tracemem(kk)
kk=vector("list",100000)
gg=proc.time()
for (i in 1:100000){
  kk[[i]]=1
}
proc.time()-gg

kk=list()
gg=proc.time()
for (i in 1:100000){
  kk[[i]]=1
}
proc.time()-gg
pp=list(NA,2,3)
pp
g=is.na(pp)


data()

CO2 %>% head
methods(class="POSIXct")
methods(class="numeric")

a="2015-11-02 10:20:23" %>% as.POSIXct()
a %>% is
show(a)
"["(a)
b=1:3
b %>% is
"["("b")
