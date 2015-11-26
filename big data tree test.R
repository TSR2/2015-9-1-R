library(C50)
library(rpart)
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
iris2=iris
for (i in 1:15){
  iris2=rbind(iris2,iris2)
}

ff=rpart(Species~.,data=iris2)
summary(ff)

rm(iris2)
