source('paperfunction.R')
a1=read.table('wine.data',sep=',')
a1 %>% head
test3=a1[,11:14]
#test3=iris[,1:4]
par(mfrow=c(1,1))

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


par(mfrow=c(2,2),mai=rep(.3,4))
test4=point_to_h(p,group=a1$V1)

plotcom(test4)
plotjointh(test4,15,B=15)


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