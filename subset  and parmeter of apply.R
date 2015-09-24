library(sqldf)
library(magrittr)
x=iris
pp=function(x,n){
  a=x+n
}
apply(x[,1:4],2,FUN = pp,n=2)

sqldf('select v1,v2 from x')

names(x)=paste('v',1:5,sep='')
subset(x = x,select=c('v1','v2'),subset=v1>7  & v2<3)
