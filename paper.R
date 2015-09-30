library(plyr)
#####################隨機產生直方圖資料
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    his <- hist(iris[sample(x=1:150,50),j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
######################################相同直方圖
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    his <- hist(iris[,j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}

##################畫直方圖
par(mfrow=c(6,4),mai=c(0,0,0,0))
for (j in 1:4){
  for (i in 1:30){
    plot(total[[j]][[i]],main = "",xlab = "",ylab = "")
  }
}
his
length(total[[1]][[1]][[1]])

#########################算平均

mu=c()
for (j in 1:4){
  sum=0
  for (i in 1:30){
    for (b in 1:(length(total[[j]][[i]][[1]])-1)){
      a=total[[j]][[i]][[2]][b]*(total[[j]][[i]][[1]][b+1]+total[[j]][[i]][[1]][b])
    sum=a+sum
    }
  }
  mu=c(mu,sum/(2*i))
}
mu
########################算平均  向量

mu=c()
for (j in 1:4){
  sum=0
  for (i in 1:30){
    b=length(total[[j]][[i]][[1]])-1
    a=total[[j]][[i]][[2]]*(total[[j]][[i]][[1]][2:(b+1)]+total[[j]][[i]][[1]][1:b])
    sum=sum+sum(a)
  }
  mu=c(mu,sum/(2*i))
}
mu


##################################test
calEX=function(x){
  b=length(x[[1]])-1
  a=x[[2]]*(x[[1]][2:(b+1)]+x[[1]][1:b])
  sum(a)
}
n=length(total[[1]])
mu=c()
for (j in 1:4){
  m=sum(sapply(total[[j]],calEX))/(2*n)
  mu=c(mu,m)
}
mu

##################################var parallel
calEX2=function(x){
  b=length(x[[1]])-1
  parta=x[[2]]*(x[[1]][1:b]^2  
                +  x[[1]][1:b]*x[[1]][2:(b+1)]  
                +  x[[1]][2:(b+1)]^2
                )
  sum(parta)
}
n=length(total[[1]])
EX2=c()
for (j in 1:4){
  m=sum(sapply(total[[j]],calEX2))/(3*n)
  EX2=c(EX2,m)
}
EX2
var=EX2-mu^2
var


################################################ var for
EX2=c()
for (j in 1:4){
  sum=0
  for (i in 1:30){
    b=length(total[[j]][[i]][[1]])-1
    a=total[[j]][[i]][[2]]*(total[[j]][[i]][[1]][2:(b+1)]^2+
                              total[[j]][[i]][[1]][2:(b+1)]*total[[j]][[i]][[1]][1:b]+
                              total[[j]][[i]][[1]][1:b]^2)
    sum=sum+sum(a)
  }
  EX2=c(EX2,sum/(3*i))
}
EX2
var=EX2-mu^2
var
###############################################covrance

cov=matrix(0,ncol=4,nrow=4)
for (i in 1:4){
  m1=laply(total[[i]],calEX)
  for (j in 1:4){
    m2=laply(total[[j]],calEX)
    cov[i,j]=sum(m1*m2)/(4*n)-sum(m1)*sum(m2)/(4*n^2)
  }
}
cov
eigen(cov)


