library(magrittr)
library(plyr)
library(dplyr)
x=seq(0,0.8,0.2)
y=c(1,4,6,2,5)
xx=lsfit(x,y)
fitted(xx,x=0)
n=data.frame(x=c(2,3))
xx=lm(y~x)
predict(object=xx,newdata = n)
fitted(xx,2)
ls.diag(xx)
library(foreign)

######讀取spss資料
vv=read.spss("C:\\Users\\tsr\\Desktop\\上課用檔案\\統計諮詢\\BW.sav"
             ,to.data.frame=T)
vv
par(mfrow=c(1,1),mai=rep(1,4))
#######估計pdf
dd=density(iris[,1])
hist(iris[,1],probability = T)
lines(dd)

####估計cdf
ee=ecdf(iris[,1])
plot(ee,verticals = T,do.p=F)
####用qqplot比較兩組分布是否相同
qqplot(rnorm(100,0,1),rnorm(100,10,1))
qqline(rnorm(100,10,1))
qqline()
plot(1:10)
####畫盒狀圖
boxplot(iris[,1],iris[,2],iris[,3],names=letters[1:3],notch=T)
boxplot(iris[,1]~iris[,5])

####常態性檢定
shapiro.test(1:100)

######plot
coplot(iris[,1]~iris[,2] |iris[,5])
x=iris
names(x)=paste("v",1:5,sep="")
mo=x %>% group_by(v5) %>% do(m=boxplot(v1~v2,data=.))
mo$m[1]

######H0=兩者無相關
cor.test(~v1+v2,data=x)
cor.test(~v1+v3,data=x)


for (i in 1:30){
  if (i<=10) {p=1}
  else if (10<i & i<=20){p=2}
  else p=3
  lines(1:4,x[i,],col=p)
}


x=iris[c(1:10,51:60,101:110),1:4]
pp=1:30
x$p=ifelse(pp<=10,1,ifelse(pp>20,3,2))
plot(c(1,4),y=c(min(x),max(x)),type="n")

x %>% rowwise %>% do(m=lines(1:4,.[1:4]))

#################星圖
x=iris[c(1:10,51:60,101:110),1:4]
stars(x,labels = names(x))

##########求導數(表達示)
v=deriv(expression(x^3+y),'x')
is(v)
v
y=1
x=1
eval(v)
#########求導數(函數)
v=deriv(expression(x^3),"x",function.arg = T)
p=v(4)
p

###########一元求解
f=function(x) x^2+3*x
uniroot(f,c(-4,-1)) #####注意好像要符合中間值定理
f2=function(x) (x^2+3*x)^2
optimize(f2,c(-4,-1))
nlm(f2,1)


###############2元求解
f2=function(x) (x[1]^2+3*x[2])^2
nlm(f2,c(1,2))
optim(c(1,2),f2)

###################檢定平均數
x=rnorm(100)
t.test(x,alternative = "greater")
t.test(x,alternative = "less",mu=3)
t.test(x,m=3)
y=rnorm(100)
t.test(x,y)
t.test(x,y,paired = T)

#################檢定變異數
var.test(x,y)
var.test(x,y,ratio = 2)
var.test(x,y,conf.level = 0.99,alternative = "greater")
