library(plyr)
library(dplyr)
library(magrittr)
#####################?H?????ͪ????ϸ???
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
######################################?ۦP??????
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    his <- hist(iris[,1])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}

##################?e??????
par(mfrow=c(6,4),mai=c(0,0,0,0))
for (j in 1:4){
  for (i in 1:30){
    plot(total[[j]][[i]],main = "",xlab = "",ylab = "")
  }
}
his
length(total[[1]][[1]][[1]])

#########################?⥭?? for

mu=c()
for (j in 1:3){
  sum=0
  for (i in 1:1){
    for (b in 1:(length(total[[j]][[i]][[1]])-1)){
      a=total[[j]][[i]][[2]][b]*(total[[j]][[i]][[1]][b+1]+total[[j]][[i]][[1]][b])
    sum=a+sum
    }
  }
  mu=c(mu,sum/(2*i))
}
mu
########################?⥭??  ?V?q

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

##################################mean parallel

hcalEX=function(var){
  p=length(var)
  calEX=function(x){
    b=length(x$count)
    a=x$count*(x$breaks[2:(b+1)]+x$breaks[1:b])
    sum(a)
  }
  n=length(var[[1]])
  mu=c()
  for (j in 1:p){
    m=sum(sapply(var[[j]],calEX))/(2*n)
    mu=c(mu,m)
  }
  mu
}
##################################var parallel
hcalvar=function(hisvar){
  p=length(hisvar)
  calEX2=function(x){
    b=length(x$count)
    parta=x$count*(x$breaks[1:b]^2  
                  +  x$breaks[1:b]*x[[1]][2:(b+1)]  
                  +  x$breaks[2:(b+1)]^2
                  )
    sum(parta)
  }
  n=length(hisvar[[1]])
  EX2=c()
  for (j in 1:p){
    m=sum(sapply(hisvar[[j]],calEX2))/(3*n)
    EX2=c(EX2,m)
  }
  EX2
  mu=hcalEX(hisvar)
  var=EX2-mu^2
  var
}

################################################ var for
EX2=c()
for (j in 1:1){
  sum=0
  for (i in 1:1){
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

hcalcov=function(x){
  p=length(x)
  cov=matrix(0,ncol=p,nrow=p)
  for (i in 1:p){
    m1=laply(x[[i]],calEX)
    for (j in 1:p){
      m2=laply(x[[j]],calEX)
      cov[i,j]=sum(m1*m2)/(4*p)-sum(m1)*sum(m2)/(4*p^2)
    }
  }
  list(cov,eigen(cov))
}


########################################
##linear combin
ppp=list()
data1=total
p=4
n=30
for (i in 1:30){
  bij=c()
  for (j in 1:p){
    bij[j]=length(data1[[j]][[i]]$count)
  }
  bij
  #造所有區間的組合
  exp=expand.grid(1:bij[1],1:bij[2],1:bij[3],1:bij[4])
  #把區間最小值和最大值分別放在不同矩陣(此時只是index)
  minI=matrix(0,ncol=p,nrow=dim(exp)[1])
  maxI=matrix(0,ncol=p,nrow=dim(exp)[1])
  #收集每個區間機率容器
  pro=rep(1,dim(exp)[1])
  #放入正確的數值
  for (j in 1:p){
    x=data1[[j]][[i]]$breaks
    count=data1[[j]][[i]]$count
    minI[,j]=x[exp[,j]]
    maxI[,j]=x[exp[,j]+1]
    pro=pro*count
  }
  #獲得主成分的係數向量
  coef=hcalcov(data1)[[2]]$vectors
  pro
  minI
  maxI
  #判定細數是否小於0，如果小於0，該系數的區間交換大小位置
  for (j in 1:p){
    if (coef[1,j]<0) {
      a=minI[,j]
      minI[,j]=maxI[,j]
      maxI[,j]=a
    }
  }
  
  #計算線性組合後的區間
  linmin=minI %*% coef[1,]
  linmax=maxI %*% coef[1,]
  m=cbind(linmin[,1],linmax[,1])
  
  np=as.data.frame(m)
  names(np)=c("Imin","Imax")
  #找出合併後直方圖的range
  his=np %>% summarise(hmin=min(Imin),hmax=max(Imax))
  Bi=max(bij)
  his=seq(from=his$hmin,to=his$hmax,length.out=Bi+1)
  

  #以下先進行對一個區間重疊的計算
  pp=c()
  for (b in 1:Bi){
    testm=cbind(linmin,linmax,his[b],his[b+1])
    
    #########完全包含的index  有時候會是空的，沒有任何全包含
    allcoverindex=which((testm[,2]<=testm[,4] & testm[,1]>=testm[,3]) |
                          (testm[,2]>=testm[,4] & testm[,1]<=testm[,3]))
    #########篩選出完全包含的
    allcover=testm[allcoverindex,]
    if (is(allcover)[2]=="vector"){
      allcover=t(as.matrix(allcover))
    }
    #計算完全包含的p值
    ratio1=min(abs(allcover[,4]-allcover[,3]),abs(allcover[,2]-allcover[,1]))/abs(allcover[,2]-allcover[,1])
    p1=sum(ratio1*pro[allcoverindex])
    
    ################判斷有重疊的index
    coverindex=which(testm[,2]>testm[,3] & testm[,1]<testm[,4])
    #######找出有相交，但不是完全包含的index
    out=coverindex %in% allcoverindex
    #######找出有相交，但是不是完全包含的
    takeindex=coverindex[!out]
    takem=testm[takeindex,]
    ####算出有take重疊的長度
    gg=cbind(takem[,2]-takem[,3],takem[,4]-takem[,1])
    takelen=apply(abs(gg),1,FUN = min)
    
    ratio2=takelen/abs(takem[,2]-takem[,1])
    p2=sum(ratio2*pro[takeindex])
    pp[b]=p1+p2
  }
ppp[[i]]=pp
}
ppp
