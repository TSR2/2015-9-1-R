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

