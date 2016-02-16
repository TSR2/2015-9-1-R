a1=read.table('data_banknote_authentication.txt',sep=',')
a1 %>% head
test3=a1[,1:4]
#test3=iris[,1:4]
par(mfrow=c(1,1))

p=scale(test3) %>% as.matrix()
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
par(mfrow=c(2,2),mai=rep(.3,4))
test4=point_to_h(p,group=a1$V5)

plotjointh(test4,10,B=10)


#############################kmean
test1=kmeans(test3,centers = 2)
tt1=test1$cluster
gr=list(which(tt1==1),which(tt1==2))
test4=point_to_h(p,group=gr)
plotjointh(test4,10,B=10)


plotcom(test4)
