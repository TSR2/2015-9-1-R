f=function(x,y)x+y
f(1:9,2:11)


####################簡單版
f=function(x,y) x^2-y
sq=function(y){
  v=1;s=100
  while (all(s>(10^-10))){
    v=v-f(v,y)/(2*v)
    s=f(v,y)
  }
  v
}
sq(c(3,4))

##################較完整版
sq=function(y,f=expression(x^2-y),tol=10^-10){
  x=1;s=100
  fp=D(f,"x")
  while (all(abs(s)>tol)){
    x=x-eval(f)/eval(fp)
    s=eval(f)
  }
  x
}

ff=sq(c(3,11))
format(ff,digits = 20)
options(digits = 10)
gg=sqrt(c(3,11))
format(gg,digits = 20)
################

a=c(expression(x^2-y),expression(x^2-y^2))
a[2]
ta=c("x","y")
m=NULL
m=c(m,D(a[1],ta[1]))
is(m)
sqq=function(ex=a){
  x=1;y=1;s=100;
  ta=c("x","y")
  m=NULL
  for (j in 1:2){
    for(i in 1:2){
      m[i+j-1]=D(ex[i],ta[j])
    }
  }
  m
}
sqq()


################################ deci tree
library(rpart)
library(maptree)
pp=rpart(Species~.,data=iris)
summary(pp)
names(pp)
draw.tree(pp)
plot(pp)
text(pp)
A=predict(pp,as.character(iris[1:61,1:4]),type="class")
table(iris[1:61,5],A)
#######################svm
library(e1071)
x=iris
r=sample(1:150,130)
train=x[r,]
test=x[-r,]

ss=svm(Species~.,data=train)
aa=predict(ss,test[,1:4])
table(test[,5],aa)

#######################
go=read.table("C:/Users/tsr/Desktop/額外檔案/新文字文件.txt",sep="",header=T)
names(go)
go=go[,-1]
a=rpart.control(minsplit = 2,cp=0,minbucket = 2)
mode=rpart(formula = PlayTennis~.,data=go,control = rpart.control(minsplit = 5,cp=0))
par(mai=rep(0.5,4))
plot(mode)
text(mode)

########################作業4
library(C50)
C5.0Control()
f=C5.0(PlayTennis~.,data=go)
summary(f)
plot(f)
####################333
library(neuralnet)


