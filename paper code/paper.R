library(plyr)
library(dplyr)
library(magrittr)
source('c:/Users/TSR/Desktop/2015-9-1-R/paper code/paperfunction.R',encoding = "UTF-8")
#####################?H?????ͪ????ϸ???
set.seed(123456789)
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    p=scale(iris[,1:4])
    his <- hist(p[sample(x=range1,20),j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
#######################################3
set.seed(123456789)
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    p=scale(iris[,1:4])
    his <- hist(p[sample(x=range1,20),j])
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


#######################create big iris
tt2=function(x){
  p=x
  for (i in 1:15){
    p=rbind(p,p)
  }
  p 
}


big= iris %>% group_by(Species) %>% do(h=tt2(.))
bigtest=do.call(rbind,big$h)
bigtest %>% dim


################################kmean test
test1=kmeans(iris[,1:4],centers = 3)
test1 %>% names()
test1$centers


#####################################################簡單一般pca測試
test3=bigtest[,1:4]
test3=iris[,1:4]
par(mfrow=c(1,1))

p=test3 %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=rep(1:3,each=50))



##################################畫出密度估計的圖
pp=Sys.time()

p=test3 %>% as.matrix() 
t1=t(p) %*% p
eig=eigen(t1)
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
image(des_e(test2,b=10))

tt1=Sys.time()-pp
tt1
##################################將iris原始資料照組別分群製作成直方圖資料3*4


m=dim(p)[1]/3
gr=list(1:m,(m+1):(m*2),(2*m+1):(m*3))
par(mfcol=c(3,4))
test4=point_to_h(p,group=gr)

#####################取兩個主成分畫出來

plotcom(test4)

#############################################joint histo
par(mfcol=c(1,1))
Rprof()
pp=Sys.time()
plotjointh(test4,10,B=10)
tt2=Sys.time()-pp
tt2

Rprof(NULL)
summaryRprof()
#########################draw test
image(lastjoint,col=rainbow(100)[lastjoint*10000])

GBRcol=color.Palette(low='green',mid='black',high='red')
for ( i in 1:3){
  image(part[[i]])
}

#################################sir

sir=function(x,index){
  dn=dim(x)[1]
  dp=dim(x)[2]
  x1=x[,index] %>% as.factor() %>% as.numeric()
  n=max(x1)
  x2=x[,-index] %>% colMeans()
  x2=matrix(rep(x2,dn),nrow=dn,byrow = T)
  pp=matrix(0,ncol=dp-1,nrow=dp-1)
  for (i in 1:n){
    xx=x %>% filter(.[,index]==levels(x[,index])[i]) %>% select(-index)
    x3=as.matrix(xx-x2)
    a=t(x3) %*% x3
    pp=pp+a
    }
  eigen(pp)
}

sir(iris,5)


test3=iris[,1:4]
#test3=iris[,1:4]
par(mfrow=c(1,1))

p=test3 %>% as.matrix()
t1=t(p) %*% p
eig=sir(iris,5)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=a1$Species)


