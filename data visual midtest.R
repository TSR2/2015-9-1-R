library(plyr)
library(dplyr)
library(magrittr)
library(reshape2)
setwd("C:\\Users\\tsr\\Desktop\\上課用檔案\\104-1-EDA-MidtermExam")
xx=read.csv("C:\\Users\\tsr\\Desktop\\上課用檔案\\104-1-EDA-MidtermExam\\aaaa.csv")
xx %>% head
xx %>% str
xx %>% dim
summary(xx) 
xx=xx[,c(-1,-3,-5,-6,-7)] #因為有5個變數只是英文或者是代號，所以先去除
xname=names(xx)
xname
xx$MonitorValue13
levels(xx$MonitorValue13) #發現有異常值
(xx=="" |xx=='x' )%>% apply(.,2,sum) ##每個資料內遺失值筆數
#轉換遺失值表示方式
for (i in 3:26) {
  xx[,i]=as.character(xx[,i])
  aa=xx[,i]
  aa[xx[,i]==""|xx[,i]=="x" ]=NA
  xx[,i]=as.numeric(aa)
}

is.na(xx)%>% apply(.,2,sum) #轉換成NA以後確認一下

summary(xx)
summary(xx$SiteName)
summary(xx$ItemName)
xx$SiteName %>% levels
boxplot(xx$MonitorValue00~xx$ItemName)
summary(xx$MonitorValue00)
new00=xx$MonitorValue00[!is.na(xx$MonitorValue00)]

###################################yy
yy=melt(xx,id=c("SiteName","ItemName"))
head(yy)
#################################最大觀察值最小觀察值
max(yy$value,na.rm = T)
min(yy$value,na.rm = T)
yy %>% filter(value<0) ###發現有數值小於0

for (i in 3:26) {
  xx[,i]=as.character(xx[,i])
  aa=xx[,i]
  aa[xx[,i]<0 ]=NA
  xx[,i]=as.numeric(aa)
}
is.na(xx)%>% apply(.,2,sum) #轉換成NA以後確認一下
yy=melt(xx,id=c("SiteName","ItemName"))
par(mfrow=c(1,1),mai=rep(0.5,4))
boxplot(xx$MonitorValue00~xx$ItemName)
###################################標準化
std=function(x) (x-mean(x,na.rm = T))/sd(x,na.rm = T)
#yy %>% group_by(ItemName) %>% mutate(stdvalue=std(.$value))
kkk=numeric(0)
zz=yy %>% arrange(ItemName)
zname=zz$ItemName %>% unique()
for (i in zname){
  stdd=zz[zz$ItemName==i,]$value %>% std
  kkk=c(kkk,stdd)
}
stddata=cbind(zz,kkk)
stddata %>% names 
test00=stddata %>% filter(variable=='MonitorValue00') %>% select(ItemName,kkk) 
par(mai=rep(1,4))
boxplot(test00$kkk~test00$ItemName,cex.axis=1,las=2)
stddata %>% filter(ItemName=="雨量"&kkk>10)
test00 %>% filter(ItemName=="雨量"&kkk>10)
 
#############################選出各種觀測量，平均數值最高的三個地區
options(digits = 5)
d1=yy %>% group_by(ItemName) %>% 
  summarise(allmean=mean(value,na.rm=T),allstd=sd(value,na.rm=T)) %>%
  arrange(desc(allmean))
citystat=yy %>% group_by(SiteName,ItemName) %>% 
  summarise(mean=mean(value,na.rm=T),std=sd(value,na.rm=T))
big=citystat %>% group_by(ItemName) %>% arrange(desc(mean)) %>% do(h=head(.,n=3))
dim(big)
bigdata=big$h[[1]]
for (i in 2:dim(big)[1] ){
  bigdata=rbind(bigdata,big$h[[i]])
}
d1
print(bigdata,n = 66)
max3=merge(bigdata,d1,by = 'ItemName')
max3=max3[,c(1,2,3,5,4,6)]
max3

###############直方圖 groupby item  
jpeg("hist.jpeg",height = 1000,width=2000)
aa=yy %>% group_by(ItemName) %>%
  do(h=hist(.$value))
aa %>% dim
par(mfrow=c(5,5),mai=rep(0.2,4))
for (i in 1:22){
  plot(aa$h[[i]],main=aa$ItemName[i])
}
dev.off()
####boxplot  map 00
b1=xx %>% select(MonitorValue00,ItemName) %>% group_by(ItemName) %>%
  do(h=boxplot(.$MonitorValue00))
b1 %>% dim
par(mfrow=c(5,5),mai=rep(0.3,4))
for (i in 1:22){
  bxp(b1$h[[i]],main=b1$ItemName[i])
}
b1$ItemName
##################### boxplot by item map 00~23

b2=xx[,-1] %>% group_by(ItemName)   %>%
  do(h=boxplot(.[,-1]))
b2 %>% dim
par(mfrow=c(5,5),mai=rep(0.1,4))
#jpeg(paste('boxplot',(i %/% 6)+1,".jpeg",sep=""),height = 1000,width=2000)
for (i in 1:22){
  #if ((i %% 6)==1) 
  bxp(b2$h[[i]],main=b2$ItemName[i],show.names = F)
  #if ((i %% 6)==0 | i==22) 
  
}
dev.off()


######################boxplot by city map
b3=xx %>% group_by(ItemName,SiteName)  %>%
  do(h=boxplot(.[,c(-1,-2)] %>% t ))
b3 %>% dim
par(mfrow=c(10,10),mai=rep(0.1,4))
for (i in 1:100){
  title(main=list(paste(b3$SiteName[[i]],b3$ItemName[[i]],sep=""),cex=0.5))
  bxp(b3$h[[i]])
}
b3$h[[1]]

#####################################根據每種指標畫每個城市的boxplot

par(mfrow=c(2,3),mai=rep(0.1,4))
windows()
for (j in levels(xx$ItemName)){
  d3=list()
  x1=xx %>% filter(ItemName==j)
  for (i in 1:dim(x1)[1]){
    d3[[i]]=x1[i,3:26] %>% t
  }
  boxplot(d3,main=j,names = x1$SiteName,cex.axis=0.8,las=2)
  savePlot(filename = j,type=c("eps"))
}