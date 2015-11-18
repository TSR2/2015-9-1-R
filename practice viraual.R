library(plyr)
library(dplyr)
library(magrittr)

spe=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsSpe.csv")
env=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsEnv.csv")
spa=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsSpa.csv")

spe[1:5,]
env[1:5,]
spa[1:5,]
spe %>% dim
env %>% dim
spa %>% dim
spe %>% names
env %>% names
spa %>% names
spa %>% summary()
env %>% summary()
spe %>% summary()
plot(spa[,2:3],type="l")
text(x=spa$x,y=spa$y,labels = spa$X,cex = 1)

boxplot(spe[,-1])
boxplot(env[,-1])
boxplot(env[,c(-1,-2,-3)])
std=function(x) (x-mean(x))/sd(x)

stdenv=apply(env[,-1],2,std ) %>% as.data.frame
 boxplot(stdenv)

########先畫點，再補線
plot(spa[,2:3],cex=spe$TRU,main="TRU")
lines(spa[,2:3],col="blue",lwd=2)
########先畫線，再補點
plot(spa[,2:3],type="l",main="TRU")
points(spa[,2:3],cex=spe$TRU,pch=21,col="blue")

#############################挑出變異最大的4組，並畫圖
###method1
spesd=apply(spe[,-1],2,sd)
top4=spesd %>% sort(decreasing = T) %>% head(n=4)
####method2
top4=spe[,-1] %>% apply(.,2,sd) %>% sort(decreasing = T) %>% head(n=4)

par(mfrow=c(2,2))
for (i in 1:4){
  plot(spa[,2:3],type="l",main=names(top4)[i])
  points(spa[,2:3],cex=spe[[names(top4)[i]]],pch=21,col="blue")
}

par(mfrow=c(1,2))
(spe[,-1]>0) %>% apply(.,2,sum) %>% sort() 

(spe[,-1]>0) %>% apply(.,2,sum) %>%  
  sort() %>% hist(.,breaks = seq(0,30,5))

################boxplot
boxplot(iris[,1:4])
par(mfrow=c(1,1),pty="s")
pairs(env,panel = panel.smooth,diag.panel = panel.hist)

##########################
x=iris
names(x)=paste("v",1:5,sep="")
no=rnorm(100,mean = mean(x$v1),sd = sd(x$v1))


x$v1 %>% plot(.,type="l")
x %$% plot(v1[1:49],v1[2:50])
x$v1 %>% hist
x$v1 %>% qqnorm()
qqline(y=no)

###sort過
x$v1 %>% sort %>% plot
sx=x$v1 %>% sort
plot(sx[1:49],sx[2:50])
###################################ch4
r=rnorm(50)
z= (r-min(r))/(max(r)-min(r))
par(mfrow=c(1,1))
rainbow(150) %>% plot(1:150,y=rep(1,150),col=.,pch=15)
mycolor=rainbow(150)[1:100] 
mycolor %>% plot(seq(0,1,length.out = 100),y=rep(1,100),col=.,pch=15)
points(z,y=rep(0.8,50),col=mycolor[floor(z*99)+1],pch=15)
points(z,y=rep(0.7,50),col=mycolor[floor(z*99)],pch=15)
points(z,y=rep(0.6,50),col=mycolor[floor(z*100)],pch=15)

####################################chaper 5

pdf("myplot.pdf", onefile = TRUE)
for(i in 1: 4){
  plot(iris[,i])
}
dev.off()

abline(0,1)


methods(class="abline")
plot %>% methods
abline %>% methods


x <- runif(12)
y <- rnorm(12)
plot(x, y, main="arrows and segments")
arrows(x[1], y[1], x[2], y[2], col= "black", length=0.2)
segments(x[3], y[3], x[4], y[4], col= "red")
segments(x[3:4], y[3:4], x[5:6], y[5:6], col= c("blue", "green"))

par(mfrow=c(1,2))

plot(x, y, main="arrows and segments")
cbind(x,y+0.1) %>% text(.,label=1:12)
lines(x=cbind(x,y)[1:2,],col="black")
lines(x=cbind(x,y)[3:4,],col="red")
lines(x=cbind(x,y)[c(3,5),],col="blue")
lines(x=cbind(x,y)[c(4,6),],col="green")

par(mai=rep(0.1,4),fig=c(90,10))
m=matrix(c(0,1,0,2,3,4,0,5,0),byrow = T,ncol=3)
layout(mat = m,widths = c(1,3,1),heights = c(1,3,1))
hist(iris[,1])
boxplot(iris[,1])
plot(iris[,c(1,3)],col=iris[,5])
hist(iris[,3])
boxplot(iris[,2],horizontal = T)

###
par(mfrow=c(1,1))
plot(1:10,rep(1,10),cex=5,col=1:10,pch=20)
text(1:10,rep(1.05,10),labels = 1:10)


colors()
col2rgb("tomato2")
palette() #常用顏色

plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[,3], iris[,4], labels=my.label, cex=0.7)

plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[,3], iris[,4], my.label, cex=0.7,
       col=ifelse(iris[,1] > median(iris[,1]), "red", "blue"))


plot(iris[,3],iris[,4])
plot(iris[,3:4],type="n")
mylabel=rep(letters[1:3],each=50)
mycol=rep(c("red","blue","green"),each=50)
text(iris[,3:4],labels = mylabel,col=mycol)

legend(5,0.6,legend = unique(iris$Species),
       col =c("red","blue","green"),pch="abc" )

################找出各分類有幾個
x %>% select(v1,v5)  %>% arrange(v5) %>%
  mutate(v6=1:150) %>% group_by(v5) %>% 
  summarise(start=min(v6),end=max(v6))

x %>% select(v5)  %>%
  group_by(v5) %>% summarise(n=n())
aggregate(Sepal.Length~Species,data=iris,length)
aggregate(x=iris[,1:4],by=list(iris$Species),sum)
################

main.ex <- expression(paste("Math Symbols: ", list(alpha, theta)))
plot(1:10, 1:10, type="n", main=main.ex, xlab="", ylab="")
text(5, 9, expression(list({f * minute}(x), {f * second}(x))))
text(5, 7, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(5, 5, expression(bar(x) == sum(frac(x[i], n), i==1, n)))

text(5, 5, expression("(a,b]"))
par(mfrow=c(2,2))
for (i in 1:4){
  plot(iris[,i],col=iris[,5],main=names(iris)[i],ylim=c(0,8))
}

par(mfrow=c(2,2))
for (i in 1:4){
  hist(iris[,i],main=names(iris)[i],ylim=c(0,40),xlim=c(0,8))
}
par(mfrow=c(1,1))
dotchart(as.matrix(iris[,1:4]))

boxplot(iris[,1:4],horizontal = T)
iris[,1] %>% hist(pro=T) 
density(iris[,1]) %>% lines

par(mfrow=c(2,2))
for (i in 1:4){
  iris[,i] %>% hist(main=names(iris)[i],pro=T)
  iris[,i] %>% density() %>% lines(col="red")
  }
