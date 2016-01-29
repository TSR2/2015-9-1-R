library(plyr)
library(dplyr)
library(magrittr)

#####################?H?????ͪ????ϸ???
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    his <- hist(iris[sample(x=range1,20),j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
######################################?ۦP??????
a=list()
total=list()
for (j in 1:7){
  for (i in 1:30){
    his <- hist(iris[,1])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
tt=proc.time()
dda=createh(data1=total,com=j)
proc.time()-tt

##################?e??????
par(mfrow=c(6,4),mai=c(0,0,0,0))
for (j in 1:4){
  for (i in 1:30){
    plot(total[[j]][[i]],main = "",xlab = "",ylab = "")
  }
}
his
length(total[[1]][[1]][[1]])




####################畫30*2 抽取iris的質方圖
plotcom(total)

#####################################################簡單一般pca測試

par(mfrow=c(1,1))
test1=kmeans(iris[,1:4],centers = 3)
test1 %>% names()
test1$centers

p=iris[,1:4] %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=rep(1:3,each=50))



##################################畫出密度估計的圖
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
image(des_e(test2,b=10))

##################################將iris原始資料照組別分群製作成直方圖資料3*4
test3=iris
gr=list(1:50,51:100,101:150)

par(mfcol=c(3,4))
test4=point_to_h(iris[,1:4],group=gr)

#####################取兩個主成分畫出來

plotcom(test4)

#############################################joint histo

plotjointh(test4,10)

#########################draw test
image(lastjoint,col=rainbow(100)[lastjoint*10000])

GBRcol=color.Palette(low='green',mid='black',high='red')
for ( i in 1:3){
  image(part[[i]])
}


##########################################


