library(plyr)
library(dplyr)
library(magrittr)

#####################?H?????ͪ????ϸ???
a=list()
total=list()
for (j in 1:4){
  for (i in 1:30){
    range1=switch ((i %/% 11)+1,1:50,51:100,101:150) 
    his <- hist(iris[sample(x=range1,20),j])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
######################################?ۦP??????
a=list()
total=list()
for (j in 1:7){
  for (i in 1:30){
    his <- hist(iris[,1])
    his$counts <- his$counts/sum(his$counts)
    a[[i]] <- his
  }
  total[[j]] <- a
}
tt=proc.time()
dda=createh(data1=total,com=j)
proc.time()-tt

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
hhh=list()
data1=total


createh=function(data1,com){
  p=length(data1)
  n=length(data1[[1]])
  for (i in 1:n){
    bij=c()
    b=list()
    for (j in 1:p){
      bij[j]=length(data1[[j]][[i]]$count)
      b[[j]]=1:bij[j]
    }
    #造所有區間的組合
    exp=expand.grid(b)
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
      if (coef[com,j]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[com,]
    linmax=maxI %*% coef[com,]
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
  hhh[[i]]=his
  }
  list(hhh,ppp)
}

createh(data1=total,com=1)















####################畫30*2 抽取iris的質方圖
for (j in 1:2){
  if(j==1) {
    par(mfcol=c(30,2),mai=c(0,0,0,0))
    kk=hist(iris[,1],plot=F)
  }
  dda=createh(data1=total,com=j)
  tt=laply(dda[[1]],max)
  bma=max(tt)
  tt=laply(dda[[1]],min)
  bmi=min(tt)
  tt=laply(dda[[2]],max)
  pma=max(tt)
  tt=laply(dda[[2]],min)
  pmi=min(tt)
  for (i in 1:30){
    kk$breaks=dda[[1]][[i]]
    kk$counts=dda[[2]][[i]]
    plot(kk,xlim=c(bmi-2,bma+1),ylim=c(0,pma),main="",col="blue",ylab = "")
  }
}







#####################################################簡單一般pca測試

par(mfrow=c(1,1))
test1=kmeans(iris[,1:4],centers = 3)
test1 %>% names()
test1$centers

p=iris[,1:4] %>% as.matrix()
t1=t(p) %*% p
eig=eigen(t1)
plot(p %*% eig$vectors[,2],p %*% eig$vectors[,1],col=rep(1:3,each=50))

dr1=p %*% eig$vectors[,1]
dr2=p %*% eig$vectors[,2]

test2=cbind(dr1,dr2)


image(des_e(test2,b=10))

##################################將iris原始資料照組別分群製作成直方圖資料3*4
test3=iris
gr=list(1:50,51:100,101:150)

par(mfcol=c(3,4))
test4=point_to_h(iris[,1:4],group=gr)

#####################取兩個主成分畫出來

for (j in 1:2){
  if(j==1) {
    par(mfcol=c(3,2),mai=c(0,0,0,0))
    kk=hist(iris[,1],plot=F)
  }
  dda=createh(data1=test4,com=j)
  tt=laply(dda[[1]],max)
  bma=max(tt)
  tt=laply(dda[[1]],min)
  bmi=min(tt)
  tt=laply(dda[[2]],max)
  pma=max(tt)
  tt=laply(dda[[2]],min)
  pmi=min(tt)
  for (i in 1:3){
    kk$breaks=dda[[1]][[i]]
    kk$counts=dda[[2]][[i]]
    plot(kk,xlim=c(bmi-2,bma+1),ylim=c(0,pma),main="",col="blue",ylab = "")
  }
}
par(mfrow=c(1,1))
dda1=createh(data1=test4,com=1)
dda2=createh(data1=test4,com=2)
#############################################joint histo
lastjoint=matrix(0,ncol=(length(yr)-1),nrow=(length(xr)-1))
part=list()
for (s in 1:3){
  b1=list(breaks=dda1[[1]][[s]],counts=dda1[[2]][[s]])
  b2=list(breaks=dda2[[1]][[s]],counts=dda2[[2]][[s]])
  yr=-4:4
  xr=3:12
  joint=matrix(0,ncol=(length(yr)-1),nrow=(length(xr)-1))
  for (j in 1:(length(yr)-1)){
    for (i in 1:(length(xr)-1)){
      gg=0
      allrange=(xr[i+1]-xr[i])*(yr[j+1]-yr[j])
      for(p in 1:length(b1$counts)){
        for(k in 1:length(b2$counts)){
          aa=min(xr[i+1]-b1$breaks[p],b1$breaks[p+1]-xr[i])
          if (aa<0) aa=0
          bb=min(yr[j+1]-b2$breaks[k],b2$breaks[k+1]-yr[j])
          if (bb<0) bb=0
          if (b1$breaks[p]>=xr[i] & b1$breaks[p+1]<=xr[i+1]) aa=b1$breaks[p+1]-b1$breaks[p]
          if (b1$breaks[p]<=xr[i] & b1$breaks[p+1]>=xr[i+1]) aa=xr[i+1]-xr[i]
          if (b2$breaks[k]>=yr[j] & b2$breaks[k+1]<=yr[j+1]) bb=b2$breaks[k+1]-b2$breaks[k]
          if (b2$breaks[k]<=yr[j] & b2$breaks[k+1]>=yr[j+1]) bb=yr[j+1]-yr[j]
          coverratio=(aa*bb)/allrange
          gg=gg+coverratio*b1$counts[p]*b2$counts[k]
          }
        }
        joint[i,j]=gg
    }
  }
  part[[s]]=joint
  lastjoint=lastjoint+joint
}
par(mfrow=c(1,1))
  
lastjoint
GBRcol=color.Palette(low='green',mid='black',high='red')
for ( i in 1:3){
  image(part[[i]])
}

image(lastjoint)
image(lastjoint,col=rainbow(100)[lastjoint*10000])

plot(1:50,col=rainbow(100))
image(matrix(1:15,ncol=3),col=1:5)

##########################################













dd=c(dda1,dda2)
xx=-2:6
yy=-2:6




for (i in 1:30){  
  b1=dd[[1]][[1]]
  p1=dd[[2]][[1]]
  b2=dd[[3]][[1]]
  p2=dd[[4]][[1]]
  mm=outer(p1,p2)
  expand.grid(xx,yy)
  for (i in 1:(length(b1)-1)){
    for (j in 1:){
      
    }
  }
  image(mm)
}
g=matrix(1:25,ncol=5)
image(g,col = heat.colors(12))
points(x=0:100,y=rep(1,101),col=heat.colors(101),pch=15,cex=2) 
