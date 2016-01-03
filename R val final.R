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



#############################test
aa1=sprintf('%02d',1:11)
tt1=paste('各縣市當月加油站汽柴油銷售量分析表opendata104',aa1,'.csv',sep='')
realpath='C:/Users/TSR/Desktop/新增資料夾 (2)/各縣市汽車加油站汽柴油銷售統計分析表/'
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
  test1 %<>% as.data.frame()
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
alldata