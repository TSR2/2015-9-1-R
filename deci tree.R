library(rpart)
library(rpart.plot)
library(maptree)
library(mvpart)
library(sampling)

x=iris
t=sample(1:150,120)
trainx=x[t,]
testx=x[-t,]
model=rpart(Species~.,data=trainx)
##########
rpart.plot(model)
draw.tree(model)
hh=predict(model,testx[,1:4])
hh=as.data.frame(hh)
name=names(hh)
index=apply(hh,1,which.max)
pre=name[index]
sum(pre==testx[,5])/dim(testx)[1]


######################
x=car.test.frame
head(x)

range(x$Mileage)
pp=rep("A",60)
pp[x$Mileage<25]="A"
pp[25<=x$Mileage & x$Mileage<32]="B"
pp[x$Mileage>=32]="C"
x$group=pp
table(x$group)
x=x[,-4]
ss=sample(1:60,45)
trainx=x[ss,]
testx=x[-ss,]
model=rpart(group~.,data=trainx)
rpart.plot(model)
hh=predict(model,testx)

hh=as.data.frame(hh)
name=names(hh)
index=apply(hh,1,which.max)
pre=name[index]
sum(pre==testx$group)/dim(testx)[1]
