source('paperfunction.R')

set.seed(123456789)
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