source('paperfunction.R')
a1=iris
test3=iris[,1:4]
#test3=iris[,1:4]
par(mfrow=c(1,1))

p=test3 %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=a1$Species)

p=test3 %>% as.matrix() 
t1=t(p) %*% p
eig=eigen(t1)
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
image(des_e(test2,b=10))



#####################
par(mfrow=c(2,2),mai=rep(.3,4))
gr=list(which(a1[,5]=='setosa'),which(a1[,5]=='versicolor'),which(a1[,5]=='virginica'))
test4=point_to_h(p,group=gr)

plotjointh(test4,10,B=10)


#############################kmean
test1=kmeans(test3,centers = 3)
tt1=test1$cluster
gr=list(which(tt1==1),which(tt1==2),which(tt1==3))
test4=point_to_h(p,group=gr)
plotjointh(test4,10,B=10)


###################output
pdf('me_iris.pdf')
par(mfrow=c(2,2))
gr=list(which(a1[,5]=='setosa'),which(a1[,5]=='versicolor'),which(a1[,5]=='virginica'))
test4=point_to_h(p,group=gr)

plotjointh(test4,10,B=10)
dev.off()


################################################big iris



