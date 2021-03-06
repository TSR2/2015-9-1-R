source('paperfunction.R')


a1=iris
test3=iris[,1:4]
#test3=iris[,1:4]
par(mfrow=c(1,1))
p=scale(test3) %>% as.matrix()

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
#gr=list(which(a1[,5]=='setosa'),which(a1[,5]=='versicolor'),which(a1[,5]=='virginica'))

test4=point_to_h(p,group=iris[,5])





#####################################





#set.seed(123456789)
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    p=scale(iris[,1:4])
    his <- hist(p[sample(x=range1,20),j],plot=F)
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}


test4=total
index=rep(c(1,2,3),each=10)
method='SR'
reh=list(createh(data1=test4,com=1,method=method,index=index)
         ,createh(data1=test4,com=2,method=method,index=index))

plotcom(reh)
par(mfrow=c(1,1))
plotjointh(reh,10)


#############################kmean
test1=kmeans(test3,centers = 3)
tt1=test1$cluster
test4=point_to_h(p,group=tt1)
plotjointh(test4,10,B=10,method='1')
plotjointh(test4,10,B=10)

###################output
pdf('me_iris.pdf')
par(mfrow=c(2,2))
test4=point_to_h(p,group=gr)

plotjointh(test4,10,B=10)
dev.off()


################################################dr tset
source('paperfunction.R')

library(dr)

aaa=dr(formula = Species~.,data = iris)
aaa

sir(iris,5)
sir(a1[,c(1,11:14)],1)
dr(formula = Species~.,data = iris)
dr(formula = V1~V11+V12+V13+V14,data = a1)
