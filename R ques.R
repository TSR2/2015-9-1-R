library(magrittr)
library(plyr)
library(dplyr)
par(mfrow=c(1,1))
r1=dweibull(seq(0,5,0.01),shape = 1,scale = 1)
r2=dweibull(seq(0,5,0.01),shape = 2,scale = 1)
r3=dweibull(seq(0,5,0.01),shape = 5,scale = 1)
plot(seq(0,5,0.01),r1,xlim=c(0,5),ylim=c(0,2),type="l"
     ,xlab=expression(x),ylab = expression(f(x)),main=expression(X%~%Weib(alpha,beta==1)))
lines(seq(0,5,0.01),r2,lty=3)
lines(seq(0,5,0.01),r3,lty=5)
text(3,0.2,expression(alpha==1))
text(2,0.4,expression(alpha==2))
text(1.5,1.5,expression(alpha==5))



wei=function(x,a,b=1){
  a*b^(-a)*x^(a-1)*exp(-(x/b)^a)
}
plot(wei(a=1))

###################q2
q2=read.table('c:/Users/TSR/Desktop/104-1-EDA-Quiz1/intevals.txt',sep='\t',head=T)
q2
q2$col=q2$Group
levels(q2$col)=c(rainbow(5)[3],rainbow(5)[1])
plot(0, xlim=c(155,175), ylim=c(54, 68),type = "n",
     xlab = "AD", ylab = "BC")
rect(q2$AD.min,q2$BC.min,q2$AD.max,q2$BC.max,
     border=as.character(q2$col))
text(q2$AD.max,q2$BC.max,labels = q2$Data)


###################q3
q3=read.table('c:/Users/TSR/Desktop/104-1-EDA-Quiz1/stat.txt'
              ,sep='\t',head=T)
q3
packageNames <- c("plyr", "ggplot2","rgeos", "maptools", "scales", "raster")
lapply(packageNames, library, character.only=TRUE)
twDist0 <- getData('GADM', country='TW', level=0)
twDist1 <- getData('GADM', country='TW', level=1)
twDist2 <- getData('GADM', country='TW', level=2)
plot(twDist0)
plot(twDist1)
plot(twDist2)

twDist2 <- fortify(twDist2, region = "NAME_2")
head(twDist2)
twDist2$id %>% unique() 
wq3=q3

#####################q4
y=iris[,2]
x=iris[,1]

b1=function(x,y){
  (length(x)*sum(x*y)-sum(x)*sum(y))/(length(x)*sum(x^2)-(sum(x))^2)
}
b1(x,y)
b0=function(x,y){
  mean(y)-b1(x,y)*mean(x)
}
b0(x,y)
b1(x,y)
lm(y~x)


###########################Q5
library(magrittr)
library(plyr)
library(dplyr)
q5=read.csv('c:/Users/TSR/Desktop/104-1-EDA-Quiz1/104年-即時犯罪資料統計數據1.csv',
            encoding='UTF-8',sep=',')
a1=q5 %>% group_by(案類別) %>% do(ss=summary(.))
a1$ss[1]
a1$ss[2]
a1$ss[3]

q5$搶奪 %<>% as.character() %>% as.numeric() #將遺失值轉換成0
q5$搶奪[is.na(q5$搶奪)]=0

mea=q5 %>% group_by(案類別) %>%
  do(mean=colMeans(.[,3:9],na.rm = T))
mea$mean

q5occur=q5 %>% filter(案類別=='發生數')
q5slove=q5 %>% filter(案類別=='破獲數')
par(mfrow=c(3,3))
for (i in 3:9){
  plot(q5occur[,i],type='l',col='red',main=names(q5)[i])
  lines(q5slove[,i],col='green')
}
q5$搶奪 %<>% as.character() %>% as.numeric()
par(mfrow=c(3,1))
q5 %>% group_by(案類別) %>%
  do(h=hist(.$'強盜',xlab=.$案類別[1],main="強盜"))

par(mfrow=c(3,3))
q5 %>% group_by(案類別) %>%
  do(h=hist(.$'強盜',xlab=.$案類別[1],main="強盜"))

hist(q5$毒品[q5$案類別=='發生數'])
