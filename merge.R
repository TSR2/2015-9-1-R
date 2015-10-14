a=matrix(0,ncol=2,nrow=10)
b=matrix(0,ncol=2,nrow=10)
a=as.data.frame(a)
b=as.data.frame(b)
a[,1]=1:10
a[,2]=100
b[,1]=1:10
b[,2]=c(7,8)
b
a
merge(a,b,all=T)
merge(a,b,by="V1")
merge(a,b,by.x="V1",by.y="V1")
merge(c(1,2,3),c(1,2))

#####
b[,1]=c(1:5,1:5)
b[,2]=c(7,8)
b
a
merge(a,b,all=T)
merge(a,b,by="V1")
merge(a,b,by="V1",all.x=T)
merge(a,b,by.x="V1",by.y="V1")
merge(c(1,2,3),c(1,2))


