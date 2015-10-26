library(data.table)
x=iris
is(x)
setDT(x)
is(x)
x1=data.table(x)
is(x1)
x1[1:10,2:3,with=F]
unique(x,by = 'Species')

library(readxl)
cc=read_excel("c:/Users/TSR/Desktop/data/104-1實習課上課一覽表（公告版）1040902.xlsx")
head(cc)
tapp
library(magrittr)
a=1
a %>% sum(3)
a
a %<>% sum(3)
a
library(plyr)
library(dplyr)

x=iris
setDF(x)
setDT(x)
setnames(x)=paste('v',1:5,sep="")
x %>% group_by(v5) %>% summarise(N=n(),mean=mean(v3),std=sd(v3))

x=iris
setDF(x)
names(x)=paste('v',1:5,sep="")
x[,v1]
x %>% arrange(v1)
x %>% filter(v1>7,v2>2)
x %>% mutate(v6=v1+v2)
x %>% select(v1,v2)
x %>% tbl_df()
setDT(x)
x[,v1]
x[,list(v1)]
x[,list(sum(v1),mean(v1)),by=v5]
x %>% print()



x %>% mutate(v6=sample(1:3,150,replace = T)) %>% group_by(v5,v6) %>% summarise(mean=mean(v1),std=sd(v1))
x[,v6:=v5]
x
x %>% select(matches('v.'))
x %>% select(contains('2'))
x %>% select(one_of('1',"2"))
ddply(x,.(v5),summarise,mean=mean(v3),std=sd(v3))
library(lattice)
setDF(x)
x %>% group_by(v5)%>%  {histogram(~v4,data=.)} 

print(iris)
