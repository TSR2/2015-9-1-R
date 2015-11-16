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





####################################chaper 5

pdf("myplot.pdf", onefile = TRUE)
for(i in 1: 4){
  plot(iris[,i])
}
dev.off()

abline(0,1)
