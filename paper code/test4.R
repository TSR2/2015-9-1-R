source('paperfunction.R')
a1=read.table('wine.data',sep=',')
a1 %>% head
test3=a1[,11:14]
#test3=iris[,1:4]
par(mfrow=c(1,1))
#set.seed(123456789)
a=list()
total2=list()
for (j in 1:dim(test3)[2]){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:59,60:130,131:178) 
    p=scale(test3)
    his <- hist(p[sample(x=range1,20),j],plot=F)
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total2[[j]] <- a
}

total3=list()
for (j in 1:dim(test3)[2]){
  for (i in 1:15){
    range1=switch (((i-1) %/% 5)+1,1:59,60:130,131:178) 
    p=scale(test3)
    his <- hist(p[sample(x=range1,40),j],plot=F)
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total3[[j]] <- a
}



p=scale(test3) %>% as.matrix()

t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=a1$V1)

p=scale(test3) %>% as.matrix() 
t1=t(p) %*% p
eig=eigen(t1)
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
image(des_e(test2,b=10))



#####################



source('paperfunction.R')
a1=read.table('wine.data',sep=',')
a1 %>% head
test3=a1[,11:14]
#test3=iris[,1:4]
par(mfrow=c(1,1))
#set.seed(123456789)
a=list()
total2=list()
for (j in 1:dim(test3)[2]){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:59,60:130,131:178) 
    p=scale(test3)
    his <- hist(p[sample(x=range1,30),j],plot=F)
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total2[[j]] <- a
}
test4=total2
#test4=total3

index=rep(c(1,2,3),each=10)
method='SIR'
hsir(test4,index)
reh=list(createh(data1=test4,com=1,method=method,index=index)
         ,createh(data1=test4,com=2,method=method,index=index))

plotcom(reh)


par(mfrow=c(1,1))
plotjointh(reh,10)





par(mfrow=c(2,2),mai=rep(.3,4))
test4=point_to_h(p,group=a1$V1)

plotcom(test4)
plotjointh(test4,15,B=15)
###################################################seperate
test4=total2
test4=total3
plotcom(test4,method=1,index=rep(c(1,2,3),each=10))
par(mfrow=c(1,1))
plotjointh(test4,10,method=1,B=10,index=rep(c(1,2,3),each=10))


#############################kmean
test1=kmeans(test3,centers = 3)
tt1=test1$cluster
test4=point_to_h(p,group=tt1)
plotjointh(test4,10,B=10)


plotcom(test4)


#################################output
par(mfrow=c(2,2))
pdf('me_wine.pdf')
par(mfrow=c(2,2))
gr=list(which(a1$V1==1),which(a1$V1==2),which(a1$V1==3))
test4=point_to_h(p,group=gr)
plotjointh(test4,10,B=10)
dev.off()