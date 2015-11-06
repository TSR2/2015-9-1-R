x=iris[,1]
y=iris[,2]
plot(x,y)
xx=order(x)
lines(x,y)
x[order(x)]

########輸出圖片到pdf檔 %07部分為自動產生序號
########7代表有7位數，0為不足的時候補0
########onefile可以讓很多個檔存在同一個pdf中
pdf("aaaaa%07d.pdf",onefile = F)
for (i in 1:4){
  plot(iris[,i])
}
dev.off()


par(mai=rep(0.1,4))
m=matrix(c(1,1,1,2,2,3,4,4,4),ncol=3,byrow = T)
layout(m,heights = c(3,1,2))
plot(1:10,rep(1,10),pch=20,col=1:10,cex=5)
text(1:10,rep(1.2,10),labels = 1:10)

par(mai=rep(0.1,4))
m=matrix(c(0,1,0,2,3,4,0,5,0),ncol=3,byrow = T)
layout(m,heights = c(1,2,1),widths = c(1,2,1))
hist(iris[,1])
boxplot(iris[,1])
plot(iris[,1],iris[,2])
hist(iris[,2])
boxplot(iris[,2])


par(mai=rep(0.1,4))
m=matrix(1:25,ncol=5,byrow = T)
layout(m)
for (i in 1:25){
  plot(1,1,pch=i,cex=5,axes = F) 
  text(x = 0.8,y = 1,labels = i)
}
