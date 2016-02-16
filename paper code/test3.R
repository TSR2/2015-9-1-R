a1=read.table('transfusion.data',sep=',',skip = 2)
a1 %>% head
test3=a1[,1:4]
a1 %>% summary()
#test3=iris[,1:4]
par(mfrow=c(1,1))

p=scale(test3)%>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=a1$V5+1)

p=test3 %>% as.matrix() 
t1=t(p) %*% p
eig=eigen(t1)
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
image(des_e(test2,b=10))



#####################
test4=point_to_h(p,group=a1$V5)

plotjointh(test4,10,B=10)


#############################kmean
test1=kmeans(test3,centers = 2)
tt1=test1$cluster

test4=point_to_h(p,group=tt1)
plotjointh(test4,10,B=10)
