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


#######################分組
v=sample(0:100,20,replace = T)
o=cut(v,breaks = c(0,40,50,70,100))
o
as.numeric(o)
table(o)
dnorm(c(0,1,2,3))

######################chisq test
###檢定比例
chisq.test(c(335,125,160),p=c(9,3,4)/16)

####檢定poisson
x=0:6
y=c(7,10,12,8,3,2,0)
p=dpois(x,lambda = mean(rep(x,y)))
p
chisq.test(y,p=p/sum(p))
###列聯表(獨立性,齊一性)
m=matrix(c(60,3,32,11),ncol=2)
###不做連續性修正
chisq.test(m,correct = F)
###做連續性修正
chisq.test(m)

#####當不滿足卡方檢定時，要用fisher
m=matrix(c(4,5,18,6),ncol=2)
fisher.test(m)

#########檢驗想同個體上的兩次檢驗，看前後分部有無變化
mcnemar.test(m)

################符號檢定  H0:M>=90 ,H1:M<90
x=sample(1:150,100,replace = T)

binom.test(sum(x>=90),n = length(x),alternative = "l")

###################rank test nonparameter
x=sample(1:150,100,replace = T)
y=sample(1:100,100,replace = T)
cor.test(rank(x),rank(y),method = "pearson")
###kendall H0:無相關
cor.test(x,y,method = "kendall")
############################符號檢定(有利用差距)
wilcox.test(x,mu = 90)
##看前後有無差異變化(類似pair t)
wilcox.test(y,x,paired = T,alternative = "l")
####兩組比較有無相關
wilcox.test(y,x,alternative = "l")
 

#####回歸

x=iris
plot(Sepal.Length~Sepal.Width,data=iris,col=Species)
plot(Petal.Length~iris$Petal.Width,data=iris,col=Species)

model=lm(Petal.Length~Petal.Width,data=iris)
model
anova(model)
summary(model)

###多元回歸
model2=lm(Petal.Length~Petal.Width+Sepal.Width,data=iris)
anova(model2)
summary(model2)
model3=update(model2,sqrt(.)~.)

#####二次回歸項
model4=update(model3,.~.+I(Sepal.Width^2),data=iris)
model4
model5=update(model4,.~.+Petal.Width:Sepal.Width)
model5
####逐步回歸
model6=step(model5)
drop1(model5)

####殘差
residuals(model2)
rstandard(model2)
rstudent(model2)
plot(model2)
plot(model2,which=1)
plot(model2,which=2)
plot(model2,which=3)
plot(model2,which=4)

##### 檢驗影響點
hatvalues(model2)
dffits(model2)
cooks.distance(model2)
covratio(model2)

tt=data.frame(x1=c(rep(1,4),1.1),x2=c(rep(2,4),2.1),x3=seq(1,5,1))
corr=cor(tt)
kappa(corr)
eigen(corr) ###找eigenvalue最小的，在看他的特徵向量

