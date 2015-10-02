library(e1071)
data(iris)
d=sample(1:150,100)

iris1=iris[d,]
iris2=iris[-d,]

model=svm(Species~.,data=iris1)
model

######取出testdata的資料
x=subset(iris2,select=-Species)
y=iris2$Species
#####預測
pre=predict(model,x)
pre
table(pre)
table(pre,y)
plot(model,iris,Petal.Width~Petal.Length)
