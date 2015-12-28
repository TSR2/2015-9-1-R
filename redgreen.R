plot(1,col=rgb(0,1,0))
plot(1,col=rgb(1,0,0))
plot(1,col=rgb(0,0,0))

cc=rev(c(rgb(seq(1,0,-0.01),0,0),rgb(0,0,0),rgb(0,seq(0,1,0.01),0)))
plot(rep(1,203),1:203,col=cc,pch=15,cex=4,ylim=c(-20,230))
text(1.05,0,'低');text(1.05,200,'高');text(1,225,'量測變數')

rgb(1:0,0,0)


# 相關性

# 有共線性問題的資料
data=data.frame('x1'=sample(1:1000,1000,T))
data$x2 = data$x1 + rnorm(1000, 500, 100)
data$x3 = rev(data$x2 - rnorm(1000, 300, 50))
data$x4 = data$x3 + rnorm(1000, 800, 20)
data$y= data$x1 + 10*data$x2 - 3*data$x3 - 5*data$x4 + rnorm(1000, 0, 200)
x=lm(y~., data=data)
x

x2=lm(y~x1, data=data)
x2

x3=lm(y~x2, data=data)
x3

x4=lm(y~x3, data=data)
x4

x5=lm(y~x4, data=data)
x5


cor(data)

head(data)


y=c(104)
x2=
x2=  
data2 = matrix(c(104, 47, 22,70, 46, 20,90, 37, 13,56, 24, 2,84, 
                 43, 17,120, 54, 29,62, 35, 7,76, 39, 14,66, 31, 
                 6,96, 49, 26,70, 45, 19,114, 51, 24),12,3,byrow = T)

data2 = data.frame(data2)
cor(data2)

lm(X1~., data2)




