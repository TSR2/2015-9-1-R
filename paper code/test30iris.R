

source('paperfunction.R')
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch (((i-1) %/% 10)+1,1:50,51:100,101:150) 
    his <- hist(iris[sample(x=range1,20),j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}

gr=rep(1:3,each=10)
test4=total
plotcom(test4,index=gr)

plotjointh(test4,10,B=10,index=gr)
