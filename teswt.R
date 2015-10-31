library(rpart)
library(rpart.plot)
library(maptree)
library(sampling)
library(magrittr)
library(data.table)
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


######################car data
x=car.test.frame
head(x)
summary(x)
x$Type %>% is
range(x$Mileage)
pp=rep("A",60)
pp[x$Mileage<25]="A"
pp[25<=x$Mileage & x$Mileage<32]="B"
pp[x$Mileage>=32]="C"
x$group=as.factor(pp)
table(x$group)

##################################利用隨機抽樣產生訓練集，再用一般決策樹建模
ss=sample(1:60,45)
x1=x[,-4]
trainx=x1[ss,]
testx=x1[-ss,]

model=rpart(group~.,data=trainx)
rpart.plot(model)
hh=predict(model,testx)

hh=as.data.frame(hh)
name=names(hh)
index=apply(hh,1,which.max)
pre=name[index]
sum(pre==testx$group)/dim(testx)[1]

#######################################
dis=table(x$group)
dis=floor(dis*(1/4))
sub=strata(x,stratanames = "group",size = dis,method = "srswor")
sub
x=setDT(x)
x1=x[,group:=NULL]
trainx=x1[-sub$ID_unit,]
testx=x1[sub$ID_unit,]
car_reg=rpart(Mileage~.,data=trainx,method="anova")
rpart.plot(car_reg)
draw.tree(car_reg)
print(car_reg)
printcp(car_reg)
summary(car_reg)

####控指每個節點在多少的時候停止
car_reg=rpart(Mileage~.,data=trainx,method="anova",minsplit=10)
rpart.plot(car_reg)
rpart.plot(car_reg,type=4)
post(car_reg,file="")
draw.tree(car_reg)
print(car_reg)
printcp(car_reg)
summary(car_reg)

######控制當cp大於多少才作樹的分割
car_reg=rpart(Mileage~.,data=trainx,method="anova",cp=0.1)
rpart.plot(car_reg)
draw.tree(car_reg)
print(car_reg)
printcp(car_reg)
summary(car_reg)
###控制樹的深度
car_reg=rpart(Mileage~.,data=trainx,method="anova",maxdepth=1)
rpart.plot(car_reg)
draw.tree(car_reg)
print(car_reg)
printcp(car_reg)
summary(car_reg)
