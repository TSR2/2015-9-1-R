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

#write.csv(x = alldata,row.names = F,fileEncoding = "UTF-8",
#          file='C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/alldata.csv')
#########################################畫圖分析

summary(alldata)

alldata %>%　filter(month==1)
alldata[1,2:9] %>% barplot(height = .) 
parnames=alldata %>% names %>%　'['(2:9)
bplot=function(x,max=80,las=1){
  y=x[2:9]
  if(is.vector(y)) y=t(y)
  names(y)=parnames
  barplot(as.matrix(y),main=paste(x[1],x[11],sep='_'),ylim=c(0,max),las=las)
}
pdf('C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/test.pdf',onefile=T,family="GB1")
#CairoPDF('C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/ro.pdf',family="SimHei")
#迴圈測試畫單個月份
#par(mfrow=c(5,5),mar=rep(0.7,4))
#for (i in 1:22){
#  bplot(alldata[i,])
#}
###向量化畫單個月分
pdf('C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/test2.pdf',onefile=T,family="GB1")
par(mfrow=c(5,5),mar=c(2,.5,1,.5))
aa=alldata %>%　filter(month==1) 
apply(aa, 1, bplot,las=1)
dev.off()
######畫單個城市每個月分
par(mfrow=c(3,4),mar=rep(2,4))
alldata %>% filter(縣市別=='臺中市') %>%
  group_by(month) %>% do(ss=bplot(.))


 
###畫每個縣市的加油站數
par(mfrow=c(1,1),mar=rep(4,4))
aa=alldata %>%　filter(month==1) 
p1=aa$total
names(p1)=aa$縣市別
barplot(p1,main='台灣各個縣市的加油站數',las=2)
usemap=alldata %>%　filter(month==1) %>% select(縣市別,total)
###將全部的月份加總，畫出各縣市的長條圖
g1=alldata %>%　group_by(縣市別) %>% do(aa=colSums(.[2:9]))
pp1=do.call(rbind,g1$aa)
gsum=cbind(g1[,1],pp1)
par(mfrow=c(5,5),mar=rep(0.7,4))
apply(gsum,1,bplot,max=1000)

#####台北市的盒狀圖
par(mfrow=c(1,1),mar=rep(3,4))
alldata %>% filter(縣市別=='臺北市') %>% '['(2:9) %>%   boxplot(main='台北市')
#####每個縣市各自的盒狀圖
par(mfrow=c(5,5),mar=rep(0.7,4))
alldata %>% group_by(縣市別) %>% do(dd=boxplot(.[,2:9],main=.[1,1]))
#####每個縣市每個月的盒狀圖
par(mfrow=c(5,5),mar=rep(0.7,4))
alldata %>% group_by(縣市別) %>% do(dd=boxplot(t(.[,2:9]),main=.[1,1]))

####################################################################標準化
st=function(x){
  tt=as.numeric(x[2:9])/as.numeric(x[10])
  dd=c(x[1],tt,x[10:11])
  names(dd)=names(x)[1:11]
  t(dd)
}

ff=apply(alldata,1,st)
testdata=t(ff) %>% as.data.frame(stringsAsFactors =F)
names(testdata)=names(alldata)[1:11]
testdata %>% head
str(testdata)
for (i in 2:11){
  testdata[,i]=as.numeric(testdata[,i])
}
str(testdata)

###向量化畫單個月分

for (i in 1:11)  {
  par(mfrow=c(5,5),mar=rep(2,4))
  aa=testdata %>%　filter(month==i) 
  apply(aa, 1, bplot,max=.5)
}

#####每個縣市各自的盒狀圖
par(mfrow=c(5,5),mar=rep(0.7,4))
testdata %>% group_by(縣市別) %>% do(dd=boxplot(.[,2:9],main=.[1,1],ylim=c(0,0.5)))

dev.off()


#####################################失敗區 會產生奇特的dataframe
if(0){
  test4=alldata %>% rowwise() %>% do(ss1=st(.))
  testdata=do.call(rbind,test4$ss1)
  testdata=Reduce(rbind,test4$ss1)
  
  testdata %>% head
  testdata %>% is
  testdata %<>% as.data.frame()
  testdata %>% is
  testdata %>% head
  testdata %>%　dim
  for (i in 2:10){
    testdata[,i]=as.numeric(testdata[,i])
  }
  names(testdata)=names(alldata)[1:10]
  str(testdata)
  summary(testdata)
  
  for (i in 2:10){
    alldata[,i]=as.numeric(alldata[,i])
  }
}
########################################


ll1=matrix(1:30,ncol=10)
names(ll1)=letters[1:10]

####################################################map
q3=read.table('c:/Users/TSR/Desktop/104-1-EDA-Quiz1/stat.txt'
              ,sep='\t',head=T)
q3
packageNames <- c("plyr", "ggplot2","rgeos", "maptools", "scales", "raster")
lapply(packageNames, library, character.only=TRUE)
library(magrittr)
twDist0 <- getData('GADM', country='TW', level=0)
twDist1 <- getData('GADM', country='TW', level=1)
twDist2 <- getData('GADM', country='TW', level=2)
plot(twDist0)
plot(twDist1)
plot(twDist2)

twDist0 <- getData('GADM', country='JP', level=0)
twDist1 <- getData('GADM', country='JP', level=1)
twDist2 <- getData('GADM', country='JP', level=2)
plot(twDist0)
plot(twDist1)
plot(twDist2)


twDist2 <- fortify(twDist2, region = "NAME_2")
head(twDist2)
twDist2 %>% dim 
twDist2$id %>% unique()

new=c("Yilan","Hualien","Nantou","Pingtung","Miaoli","Taoyuan",
      "Kaohsiung City","Kaohsiung City","Keelung","Yulin","New Taipei City",
      "Hsinchu County","Chiayi County","Changhua","Taichung City","Taichung City",
      "Taipei","Taitung","Tainan City","Tainan City","Penghu")
q4=q3
q4$Area=new
q4$地區=as.character(q4$地區)
q5=rbind(q4,c('新竹市','4524.25','Hsinchu City'))
q5$人口密度=as.numeric(q5$人口密度)
q5[12:13,1]=c('新竹縣','嘉義縣')
q5 %>% str
twDist2 <- fortify(twDist2, region = "NAME_2")#fortify function helps us transform a SpatialPolygonDataFrame into a data frame, which is easier to manipulate.
twDist2$id[twDist2$id=='Taichung'] <- 'Taichung City'
twDist2$id[twDist2$id=='Tainan'] <- 'Tainan City'
twDist2$id[twDist2$id=='Kaohsiung'] <- 'Kaohsiung City'

ggplot() + geom_map(data=q5, aes(map_id =Area , fill = 人口密度), map = twDist2) + expand_limits(x = twDist2$long, y = twDist2$lat)
distanceCenter <- ddply(twDist2, .(id), summarize, latCenter = mean(lat), longCenter = mean(long))
ggplot() + 
  geom_map(data=q5, aes(map_id = Area , fill = 人口密度), map = twDist2) + 
  expand_limits(x = twDist2$long, y = twDist2$lat) + 
  scale_fill_gradient2(low = "white",  mid = "palevioletred1", midpoint = mean(q5$'人口密度'), high = muted("palevioletred4"), limits = c(min(q5$'人口密度')-3, max(q5$'人口密度')+3))+
  geom_text(data = distanceCenter, aes(x = longCenter, y = latCenter, label = id, size = 0.2))+xlab("")+ylab("")+ggtitle("Older Population in Taiwan")

q5
usemap
q6=merge(q5,usemap,by.x='地區',by.y='縣市別',all.x=T)
q6[c(6,8,17,21),4]=c(260,266,308,279)
q6
ggplot() + geom_map(data=q6, aes(map_id =Area , fill = total), map = twDist2) + expand_limits(x = twDist2$long, y = twDist2$lat)
distanceCenter <- ddply(twDist2, .(id), summarize, latCenter = mean(lat), longCenter = mean(long))
pdf('C:/Users/TSR/Desktop/各縣市汽車加油站汽柴油銷售統計分析表/test3.pdf',family="GB1")
ggplot() + 
  geom_map(data=q6, aes(map_id = Area , fill = total), map = twDist2) + 
  expand_limits(x = twDist2$long, y = twDist2$lat) + 
  scale_fill_gradient2(low = "white",  mid = "palevioletred1", midpoint = mean(q6$total), high = muted("palevioletred4"), limits = c(min(q6$total)-3, max(q6$total)+3))+
  geom_text(data = distanceCenter, aes(x = longCenter, y = latCenter, label = id, size = 0.2))+xlab("")+ylab("")+ggtitle("count of gas station in Taiwan")
dev.off()