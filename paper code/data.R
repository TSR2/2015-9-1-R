library(plyr)
library(dplyr)
library(magrittr)
library(MASS)
library(geigen)


set.seed(123456789)
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    p=scale(iris[,1:4])
    his <- hist(p[sample(x=range1,20),j],plot=F)
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}








kk=function(x,i){
  llply(x,'[[',i)
}
llply(total,kk,1)


t1=llply(total,llply,'[[',2)
llply(t1,'[[',1)


##########################################
xP=list()
for (i in 1:30){
    aa=llply(t1,'[[',i)
    xP[[i]]=aa
}
xP

t2=llply(total,llply,'[[',1)
xI=list()
for (i in 1:30){
  aa=llply(t2,'[[',i)
  xI[[i]]=aa
}
xI

xO=rep(1:3,each=10)
Bij=ldply(xP,laply,length) %>% as.matrix()

histo=list(xI=xI,xP=xP,xO=xO,Bij=Bij)
