a1=read.table('Skin_NonSkin.txt')
a1 %>% head
test3=a1[,1:3]
#test3=iris[,1:4]
par(mfrow=c(1,1))

p=scale(test3) %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
#plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=a1$V4)

p=test3 %>% as.matrix() 
t1=t(p) %*% p
eig=eigen(t1)
dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)
mcol=c(rainbow(10, start=0, end=0.3),rainbow(30, start=0.3, end=0.6),rainbow(10, start=0.6, end=0.7))
image(des_e(test2,b=10),col=mcol)
ddd <- des_e(test2,b=10)
ddd <- as.numeric(ddd)
boxplot(ddd)
#####################
par(mfrow=c(2,2),mai=rep(.3,4))
test4=point_to_h(p,group=a1$V4)
plotjointh(test4,10,B=10)



###############################################kmean
test1=kmeans(a1[,1:3],centers = 2)
test1 %>% names()
tt1=test1$cluster
test4=point_to_h(p,group=tt1)
plotjointh(test4,10,B=7)



nnn=iris[iris[,5]=='p',]
nnn=iris[1,]
nnn %>% is
