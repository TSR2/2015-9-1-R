library(magrittr)
library(plyr)
library(dplyr)


###轉換成txt後讀取
path1='C:/Users/TSR/Desktop/新增資料夾 (2)/各縣市汽車加油站汽柴油銷售統計分析表/asdsd.txt'
test=read.table(path1,fill = T,skip = 5)
test %>% head 
names(test)=c('縣市別','x5','x5-x10','x10-x15','x15-x20','x20-x25','x25-x30'
              ,'x30-x40','x40','total')
head(test)


####直接讀取csv
path='C:/Users/TSR/Desktop/新增資料夾 (2)/各縣市汽車加油站汽柴油銷售統計分析表/各縣市當月加油站汽柴油銷售量分析表opendata10403.csv'
aaa=readLines(path,encoding = 'UTF-8')
aaa1=aaa[-c(1:5,28,29)]
aaa2=strsplit(aaa1,split = ',')
#aaa2[[1]] %>% is
#創建資料框
test1=matrix('0',nrow=22,ncol=10)
for (i in 1:22){
  test1[i,]=aaa2[[i]]
}
test1 %<>% as.data.frame()
test1
###names
tablename=c('縣市別','x5','x5-x10','x10-x15','x15-x20','x20-x25','x25-x30'
  ,'x30-x40','x40','total')

names(test1)=tablename
test1



#############################讀入所有資料
aa1=sprintf('%02d',1:11)
tt1=paste('各縣市當月加油站汽柴油銷售量分析表opendata104',aa1,'.csv',sep='')
#######路徑
realpath='C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/'
allpath=paste(realpath,tt1,sep='')

for (j in 1:11){
  aaa=readLines(allpath[j],encoding = 'UTF-8')
  aaa1=aaa[-c(1:5,28,29)]
  aaa2=strsplit(aaa1,split = ',')
  #aaa2[[1]] %>% is
  #創建資料框
  test1=matrix('0',nrow=22,ncol=10)
  for (i in 1:22){
    test1[i,]=aaa2[[i]]
  }
  test1 %<>% as.data.frame(.,stringsAsFactors=F)
  test1
  ###names
  tablename=c('縣市別','x5','x5-x10','x10-x15','x15-x20','x20-x25','x25-x30'
              ,'x30-x40','x40','total')
  
  names(test1)=tablename
  test1$month=j
  if(j==1){
    alldata=test1
  }else{
    alldata=rbind(alldata,test1)
  }
}
for (i in 2:10){
  alldata[,i]=as.numeric(alldata[,i])
}

#write.csv(x = alldata,file='C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/alldata.csv')
#########################################畫圖分析

summary(alldata)

alldata %>%　filter(month==1)
alldata[1,2:9] %>% barplot(height = .) 
parnames=alldata %>% names %>%　'['(2:9)
bplot=function(x,max=80){
  y=x[2:9]
  if(is.vector(y)) y=t(y)
  names(y)=parnames
  barplot(as.matrix(y),main=paste(x[1],x[11],sep='_'),ylim=c(0,max))
}

#迴圈測試畫單個月份
par(mfrow=c(5,5),mar=rep(0.7,4))
for (i in 1:22){
  bplot(alldata[i,])
}
###向量化畫單個月分
par(mfrow=c(5,5),mar=rep(2,4))
aa=alldata %>%　filter(month==1) 
apply(aa, 1, bplot)

g1=alldata %>%　group_by(縣市別) %>% do(aa=colSums(.[2:9]))
pp1=do.call(rbind,g1$aa)
gsum=cbind(g1[,1],pp1)
par(mfrow=c(5,5),mar=rep(0.7,4))
apply(gsum,1,bplot,max=1000)

par(mfrow=c(1,1))
alldata %>% filter(縣市別=='臺北市') %>% '['(2:9) %>%   boxplot()

par(mfrow=c(5,5),mar=rep(0.7,4))
alldata %>% group_by(縣市別) %>% do(dd=boxplot(.[,2:9],main=.[1,1]))

par(mfrow=c(5,5),mar=rep(0.7,4))
alldata %>% group_by(縣市別) %>% do(dd=boxplot(t(.[,2:9]),main=.[1,1]))


alldata %>% filter(縣市別=='桃園縣')
#####





