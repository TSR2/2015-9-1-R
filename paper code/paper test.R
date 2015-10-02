x=hist(iris[,1])
x
class(x)
b=list()
b[[1]]=1:9
b[[2]]=1:8
b[[3]]=(b$counts/diff(b$breaks))/sum(b$counts)
b[[4]]=(b$breaks[1:8]+b$breaks[2:9])/2
names(b)=c("a","b")
name=names(x)
names(b)=name[1]
names(b)=name[1:2]
names(b)=name[1:3]
names(b)=name[1:4]
class(b)="histogram"
plot(b)

(x$counts/diff(x$breaks))/sum(x$counts)


b=matrix(c(rep(1:4,)),ncol=2,nrow=4)
cor(b)
