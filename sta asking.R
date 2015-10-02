library(magrittr)
library(plyr)
library(dplyr)
library(data.table)
library(lattice)
x=iris
names(x)=paste('v',1:5,sep="")
setDT(x)
x
x[,list(mean(v1),sd(v1))]
x[,list(mean(v1),sd(v1)),by=v5]
x %>% group_by(v5) %>% summarise(mean(v1))
histogram(~v1+v2|v5,data = x)

x %>% group_by(v5) %>% summarise(n=n())

ll=rbeta(shape1 = 1,shape2 = 1,n=15)
ll
?beta
library(lattice)
bwplot(v2~v1,data=x)
?bwplot
library(readxl)
dd=read_excel("c:/Users/tsr/Downloads/FEV.xls")
head(dd)
data1=dd$FEV[dd$Sex==1]
data2=dd$FEV[dd$Sex==0]
t.test(x=data1[1:318],y=data2,paired=T)
dd %>% group_by(Sex) %>% summarise(bwplot(~FEV,data=.))
bwplot(Age~FEV|Sex,data=dd,fill=c(1:17))
