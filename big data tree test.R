library(C50)
library(rpart)

library(magrittr)
library(plyr)
library(dplyr)
library(pryr)


dim(pp)
p1=pp %>% head(2)
is(p1)
as.data.frame(p1)

C5.0(`Method Type`~.,data=p1)
p1$`Method Type` = as.factor(p1$`Method Type`)
p1$`Method Type` %>% is
p1 %>% str
p1$`Date Local` %<>% as.numeric()
p1$`Date GMT` %<>% as.numeric()
p1$`Date of Last Change` %<>% as.numeric()

C5.0(`Method Type`~.,data=p1)

pp$`Method Type` = as.factor(pp$`Method Type`)
pp$`Method Type` %>% is
pp$`Date Local` %<>% as.numeric()
pp$`Date GMT` %<>% as.numeric()
pp$`Date of Last Change` %<>% as.numeric()
  
t=proc.time()
ss=C5.0(`Method Type`~.,data=pp[1:500000,])
summary(ss)
proc.time()-t


t=proc.time()
ff=rpart(`Method Type`~.,data=pp[1:500000,])
summary(ss)
proc.time()-t

rm(pp)
################iris 決策樹
p=iris[,1:4]
for (i in 1:12){
  p=cbind(p,p)
}

p=cbind(p,iris$Species)

for (i in 1:5){
  p=rbind(p,p)
}

p %>% dim
names(p)=c(paste('v',1:(dim(p)[2]-1),sep=""),'aa')
library(rpart)
library(maptree)
t=proc.time()
a=rpart(aa~.,data=p)
proc.time()-t
draw.tree(a)


summary(a)


################lm
library(car)
p=iris[,1:3]
for (i in 1:8){
  p=cbind(p,p)
}

p=cbind(p,iris[,4])

for (i in 1:5){
  p=rbind(p,p)
}
ee=t(as.matrix(p))%*% as.matrix(p)
solve(ee)
p %>% dim
t(p) %>% dim
names(p)=c(paste('v',1:(dim(p)[2]-1),sep=""),'taget')
lmmodel=lm(taget~.,data=p)
lmmodel
summary(lmmodel)
vif(lmmodel)
s1=step(lmmodel,direction = 'backward')
s2=step(lmmodel,direction = 'forward')
s3=step(lmmodel)

################lm test
tt=function(x){
  p=iris[,1:3]
  for (i in 1:x){
    p=cbind(p,p)
  }
  
  p=cbind(p,iris[,4])
  
  for (i in 1:5){
    p=rbind(p,p)
  }
  
  p %>% dim
  names(p)=c(paste('v',1:(dim(p)[2]-1),sep=""),'taget')
  t1=proc.time()
  lmmodel=lm(taget~.,data=p)
  time=proc.time()-t1
}
ll=c()
for (i in 1:9){
  time=tt(i)
  ll[i]=time[3]
}

ll
  