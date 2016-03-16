library(plyr)
library(dplyr)
library(magrittr)
library(MASS)
library(geigen)
############################### mean parallel 
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
  mu=hcalEX(hisvar)
  var=EX2-mu^2
  var
}

#########################################calculate covance matrix
hcalcov=function(x){
  calEX=function(x){
    b=length(x$count)
    a=x$count*(x$breaks[2:(b+1)]+x$breaks[1:b])
    sum(a)
  }
  p=length(x)
  n=length(x[[1]])
  cov=matrix(0,ncol=p,nrow=p)
  for (i in 1:p){
    m1=laply(x[[i]],calEX)
    for (j in 1:p){
      m2=laply(x[[j]],calEX)
      cov[i,j]=sum(m1*m2)/(4*p)-(sum(m1)*sum(m2)/(4*p^2))
    }
  }
  list(cov,eigen(cov))
}


if(0){
  hsir=function(x,index){
    #x=total
    xcov=cov(x)
    n=length(index)
    dp=length(x)
    dn=length(x[[1]])
    allmean=hcalEX(x)
    pp=matrix(0,ncol=dp,nrow=dp)
    for (k in 1:n){
      xx=llply(x,'[',index[[k]])
      p=length(xx)
      a=hcalEX(xx)-allmean
      parcov=a %*% t(a)
      pp=pp+parcov
    }
    gx=ginv(xcov)
    list(pp,eigen(gx %*% pp))
  }
}

hsir=function(x,index=1){
  pcacov=hcalcov(x)[[1]]
  if(length(index)==1) index=1:length(x[[1]])
  n=index %>% unique() %>% length()
  uni=index %>% unique
  dp=length(x)
  dn=length(x[[1]])
  allmean=hcalEX(x)
  pp=matrix(0,ncol=dp,nrow=dp)
  for (k in 1:n){
    xx=llply(x,'[',index==uni[k])
    a=hcalEX(xx)-allmean
    parcov=a %*% t(a)
    w=length(xx[[1]])/dn
    pp=pp+parcov*w
  }
  
  if(1){
    ei=geigen(pp,pcacov,symmetric =T)
    ei$values=(ei$values)^2
    ss=order(ei$values,decreasing = T)
    ei$values=ei$values[ss]
    ei$vectors=ei$vectors[,ss]
  }

  if(0){
    gpca=ginv(pca)
    ei=eigen(gpca %*% pp)
    ss=order(ei$values,decreasing = T)
    ei$values=ei$values[ss]
    ei$vectors=ei$vectors[,ss]
    #ei$values=Re(ei$values)
    #ei$vectors=Re(ei$vectors)
  }
  #ei
  #print(hcalcov(x))
  list(pp,ei)
}
#########################create compo hist
createh=function(data1,com,B=0,method='SIR',...){
  p=length(data1)
  n=length(data1[[1]])
  ppp=list()
  hhh=list()
  for (i in 1:n){
    bij=c()
    b=list()
    ##算出某個觀測值得每個值方圖資料有幾個分割
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
    if(method=='SIR'){
      coef=hsir(data1,...)[[2]]$vectors
    }else{coef=hcalcov(data1)[[2]]$vectors}

    #判定細數是否小於0，如果小於0，該系數的區間交換大小位置
    for (j in 1:p){
      if (coef[j,com]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[,com]
    linmax=maxI %*% coef[,com]
    #找出合併後直方圖的range
    if (B==0) {Bi=max(bij)
    }else{Bi=B}
    his=seq(from=min(linmin[,1]),to=max(linmax[,1]),length.out=Bi+1)
    
    
    #以下先進行對一個區間重疊的計算
    pp=rep(0,Bi)
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
      if(dim(allcover)[1]>0){
        ratio1=min(abs(allcover[,4]-allcover[,3]),abs(allcover[,2]-allcover[,1]))/abs(allcover[,2]-allcover[,1])
        p1=sum(ratio1*pro[allcoverindex])
      }else{p1=0}
      ################判斷有重疊的index
      coverindex=which(testm[,2]>testm[,3] & testm[,1]<testm[,4])
      #######找出有相交，但不是完全包含的index
      out=coverindex %in% allcoverindex
      #######找出有相交，但是不是完全包含的
      takeindex=coverindex[!out]
      takem=testm[takeindex,]
      ####算出有take重疊的長度
      if(dim(takem)[1]>0){
        gg=data.frame(a=takem[,2]-takem[,3],b=takem[,4]-takem[,1])
        takelen=apply(abs(gg),1,FUN = min)
        ratio2=takelen/abs(takem[,2]-takem[,1])
        p2=sum(ratio2*pro[takeindex])
      }else{p2=0}
      pp[b]=p1+p2
    }
    cat(paste('',i))
    ppp[[i]]=pp/sum(pp)
    hhh[[i]]=his
  }
  list(hhh,ppp)
}

###########################################test

########2/1 以前
if (0){
createh=function(data1,com,B=0){
  p=length(data1)
  n=length(data1[[1]])
  ppp=list()
  hhh=list()
  for (i in 1:n){
    bij=c()
    b=list()
    #算出某個觀測值得每個值方圖資料有幾個分割
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
      if (coef[j,com]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[,com]
    linmax=maxI %*% coef[,com]
    m=cbind(linmin[,1],linmax[,1])
    
    np=as.data.frame(m)
    names(np)=c("Imin","Imax")
    #找出合併後直方圖的range
    his=np %>% summarise(hmin=min(Imin),hmax=max(Imax))
    if (B==0) {Bi=max(bij)
    }else{Bi=B}
    his=seq(from=his$hmin,to=his$hmax,length.out=Bi+1)
    
    
    #以下先進行對一個區間重疊的計算
    pp=rep(0,Bi)
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
      if(dim(allcover)[1]>0){
        ratio1=min(abs(allcover[,4]-allcover[,3]),abs(allcover[,2]-allcover[,1]))/abs(allcover[,2]-allcover[,1])
        p1=sum(ratio1*pro[allcoverindex])
      }else{p1=0}
      ################判斷有重疊的index
      coverindex=which(testm[,2]>testm[,3] & testm[,1]<testm[,4])
      #######找出有相交，但不是完全包含的index
      out=coverindex %in% allcoverindex
      #######找出有相交，但是不是完全包含的
      takeindex=coverindex[!out]
      takem=testm[takeindex,]
      ####算出有take重疊的長度
      if(dim(takem)[1]>0){
        gg=data.frame(a=takem[,2]-takem[,3],b=takem[,4]-takem[,1])
        takelen=gg %>% select(h=min(a,b))
        ratio2=takelen/abs(takem[,2]-takem[,1])
        p2=sum(ratio2*pro[takeindex])
      }else{p2=0}
      pp[b]=p1+p2
    }
    ppp[[i]]=pp/sum(pp)
    hhh[[i]]=his
  }
  list(hhh,ppp)
}


#######舊版
createh=function(data1,com,B=0){
  p=length(data1)
  n=length(data1[[1]])
  ppp=list()
  hhh=list()
  for (i in 1:n){
    bij=c()
    b=list()
    #算出某個觀測值得每個值方圖資料有幾個分割
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
      if (coef[j,com]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[,com]
    linmax=maxI %*% coef[,com]
    m=cbind(linmin[,1],linmax[,1])
    
    np=as.data.frame(m)
    names(np)=c("Imin","Imax")
    #找出合併後直方圖的range
    his=np %>% summarise(hmin=min(Imin),hmax=max(Imax))
    if (B==0) {Bi=max(bij)
    }else{Bi=B}
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
    ppp[[i]]=pp/sum(pp)
    hhh[[i]]=his
  }
  list(hhh,ppp)
}
######錯誤

createh=function(data1,com){
  p=length(data1)
  n=length(data1[[1]])
  ppp=vector("list",n)
  hhh=vector("list",n)
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
    #判定細數是否小於0，如果小於0，該系數的區間交換大小位置
    for (j in 1:p){
      if (coef[j,com]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[,com]
    linmax=maxI %*% coef[,com]
    m=cbind(linmin[,1],linmax[,1])
    
    np=as.data.frame(m)
    names(np)=c("Imin","Imax")
    #找出合併後直方圖的range
    his=np %>% summarise(hmin=min(Imin),hmax=max(Imax))
    Bi=max(bij)
    his=seq(from=his$hmin,to=his$hmax,length.out=Bi+1)
    
    
    #以下先進行對一個區間重疊的計算
    
    for (b in 1:Bi){
      testm=cbind(linmin,linmax,his[b],his[b+1])
      pp=numeric(Bi)
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
}
#######去除which 反而比較慢
if (0){
createh=function(data1,com){
  p=length(data1)
  n=length(data1[[1]])
  ppp=list()
  hhh=list()
  for (i in 1:n){
    bij=c()
    b=list()
    #算出某個觀測值得每個值方圖資料有幾個分割
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
      if (coef[j,com]<0) {
        a=minI[,j]
        minI[,j]=maxI[,j]
        maxI[,j]=a
      }
    }
    
    #計算線性組合後的區間
    linmin=minI %*% coef[,com]
    linmax=maxI %*% coef[,com]
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
      allcoverindex=(testm[,2]<=testm[,4] & testm[,1]>=testm[,3]) |
                            (testm[,2]>=testm[,4] & testm[,1]<=testm[,3])     #########篩選出完全包含的
      allcover=testm[allcoverindex,]
      if (is(allcover)[2]=="vector"){
        allcover=t(as.matrix(allcover))
      }
      #計算完全包含的p值
      ratio1=min(abs(allcover[,4]-allcover[,3]),abs(allcover[,2]-allcover[,1]))/abs(allcover[,2]-allcover[,1])
      p1=sum(ratio1*pro[allcoverindex])
      
      ################判斷有重疊的index
      coverindex=(testm[,2]>testm[,3] & testm[,1]<testm[,4]) |
                  (testm[,2]<testm[,3] & testm[,1]>testm[,4])
      #######找出有相交，但不是完全包含的index
      #######找出有相交，但是不是完全包含的
      takem=testm[coverindex,]
      if (is(takem)[2]=="vector"){
        takem=t(as.matrix(takem))
      }
      ####算出有take重疊的長度
      gg=cbind(takem[,2]-takem[,3],takem[,4]-takem[,1])
      takelen=apply(abs(gg),1,FUN = min)
      
      ratio2=takelen/abs(takem[,2]-takem[,1])
      p2=sum(ratio2*pro[coverindex])
      pp[b]=p1+p2
    }
    ppp[[i]]=pp/sum(pp)
    hhh[[i]]=his
  }
  list(hhh,ppp)
}
}
###############################################
transgroup=function(x){
  xx=x %>% as.factor() %>% as.numeric()
  as=unique(iris[,1]) 
  as[1]==5.1
  }


############################################

sir=function(x,index=1){
  #x=iris
  #index=5
  #xcov=cov(x[,-index])
  xx=x[,-index] %>% scale
  x=cbind(x[,index],scale(x[,-index])) %>% as.data.frame()
  
  #xx=x[,-index]
  #x=cbind(x[,index],x[,-index]) %>% as.data.frame()
  xcov=cov(x[,-1])
  
  #xx=scale(xx)
  #xx=t(t(xx)-colMeans(xx)) 
  
  dn=dim(x)[1]
  dp=dim(x)[2]
  x1=x[,1] %>% unique() %>% as.numeric()
  uni=x[,1] %>% unique()
  n=max(x1)
  x2=x[,-1] %>% colMeans()
  #x2=matrix(rep(x2,dn),nrow=dn,byrow = T)
  pp=matrix(0,ncol=dp-1,nrow=dp-1)
  for (i in 1:n){
    xx=x %>% filter(.[,1]==uni[i]) %>% '['(-1)
    w=dim(xx)[1]/dn
    x3=xx %>% colMeans()
    x4=as.matrix(x3)
    a=x4 %*% t(x4)
    pp=pp+w*a
  }
  print(geigen(pp,xcov,symmetric = T))
  gx=ginv(xcov)
  eigen(gx %*% pp)
}

###################################common desity estmate
des_e=function(x,b=10){
  ymin=min(x[,1])
  ymax=max(x[,1])
  xmin=min(x[,2])
  xmax=max(x[,2])
  
  yrange=seq(ymin,ymax,length.out = b+1)
  xrange=seq(xmin,xmax,length.out = b+1)
  
  qq1=matrix(0,nrow=b,ncol=b)
  for (j in 1:b){
    for(i in 1:b){
      qq1[i,j]=sum(x[,1]>=yrange[j] & x[,1]<=yrange[j+1] & x[,2]>=xrange[i] & x[,2]<=xrange[i+1])
    }
  }
  qq1
}

###################################一般資料轉換成直方圖資料
point_to_h=function(x,group){
  d=dim(x)
  n=group %>% unique() %>% length()
  uni=group %>% unique
  h=list()
  a=list()
  for ( j in 1:d[2]){
    for(i in 1:length(uni)){
      ff=hist(x[group==uni[i],j],plot=F)
      #ff$counts=ff$counts/sum(ff$counts)
      a[[i]]=ff
    }
    h[[j]]=a
  }
  h
}

#############################################取出質方圖資料的主成分畫圖
plotcom=function(x,...){
  n=length(x[[1]])
  p=length(x)
  for (j in 1:2){
    if(j==1) {
      par(mfcol=c(n,2),mai=c(0,0,0,0))
      kk=hist(iris[,1],plot=F)
    }
    dda=createh(data1=x,com=j,...)
    tt=laply(dda[[1]],max)
    bma=max(tt)
    tt=laply(dda[[1]],min)
    bmi=min(tt)
    tt=laply(dda[[2]],max)
    pma=max(tt)
    tt=laply(dda[[2]],min)
    pmi=min(tt)
    for (i in 1:n){
      kk$breaks=dda[[1]][[i]]
      kk$counts=dda[[2]][[i]]
      plot(kk,xlim=c(bmi-2,bma+1),ylim=c(0,pma),main="",col="blue",ylab = "")
    }
  }
}
################################################seperate
plotcom=function(reduceh){
  n=length(reduceh[[1]][[1]])
  p=length(reduceh)
  for (j in 1:2){
    if(j==1) {
      par(mfcol=c(n,2),mai=c(0,0,0,0))
      kk=hist(iris[,1],plot=F)
    }
    dda=reh[[j]]
    tt=laply(dda[[1]],max)
    bma=max(tt)
    tt=laply(dda[[1]],min)
    bmi=min(tt)
    tt=laply(dda[[2]],max)
    pma=max(tt)
    tt=laply(dda[[2]],min)
    pmi=min(tt)
    for (i in 1:n){
      kk$breaks=dda[[1]][[i]]
      kk$counts=dda[[2]][[i]]
      plot(kk,xlim=c(bmi-2,bma+1),ylim=c(0,pma),main="",col="blue",ylab = "")
    }
  }
}

#####################
color.Palette <- function(low = "black",
                          high = c("green", "red"),
                          mid="black",
                          k =50)
{
  low <- col2rgb(low)/255
  high <- col2rgb(high)/255
  if(is.null(mid)){
    r <- seq(low[1], high[1], len = k)
    g <- seq(low[2], high[2], len = k)
    b <- seq(low[3], high[3], len = k)
  }
  if(!is.null(mid)){
    k2 <- round(k/2)
    mid <- col2rgb(mid)/255
    r <- c(seq(low[1], mid[1], len = k2),
           seq(mid[1], high[1], len = k2))
    g <- c(seq(low[2], mid[2], len = k2),
           seq(mid[2], high[2], len = k2))
    b <- c(seq(low[3], mid[3], len = k2),
           seq(mid[3], high[3], len = k2))
  }
  rgb(r, g, b)
}


###################################################plot joint histo
#yr=seq(-4,4,by=.1)
#xr=seq(4,12,by=.1)
plotjointh=function(x,b,...){

  dda1=createh(data1=x,com=1,...)
  dda2=createh(data1=x,com=2,...)
  xmax=dda1[[1]] %>% unlist %>% max
  xmin=dda1[[1]] %>% unlist %>% min
  ymax=dda2[[1]] %>% unlist %>% max
  ymin=dda2[[1]] %>% unlist %>% min
  xr=seq(xmin,xmax,length.out = b)
  yr=seq(ymin,ymax,length.out = b)
  lastjoint=matrix(0,ncol=(length(yr)-1),nrow=(length(xr)-1))
  #par(mfrow=c(round(length(dda1[[1]])+1/2),2))
  part=list()
  for (s in 1:length(dda1[[1]])){
    b1=list(breaks=dda1[[1]][[s]],counts=dda1[[2]][[s]])
    b2=list(breaks=dda2[[1]][[s]],counts=dda2[[2]][[s]])
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
    #part[[s]]=joint
    mcol=c(rainbow(10, start=0, end=0.5),rainbow(190, start=0.5, end=0.7))
    image(joint,col=mcol)
    lastjoint=lastjoint+joint
  }
  mcol=c(rainbow(10, start=0, end=0.5),rainbow(190, start=0.5, end=0.7))
  image(lastjoint,col=mcol)
}

#####################################################seperate
plotjointh=function(reh,b,...){
  dda1=reh[[1]]
  dda2=reh[[2]]
  xmax=dda1[[1]] %>% unlist %>% max
  xmin=dda1[[1]] %>% unlist %>% min
  ymax=dda2[[1]] %>% unlist %>% max
  ymin=dda2[[1]] %>% unlist %>% min
  xr=seq(xmin,xmax,length.out = b)
  yr=seq(ymin,ymax,length.out = b)
  lastjoint=matrix(0,ncol=(length(yr)-1),nrow=(length(xr)-1))
  #par(mfrow=c(round(length(dda1[[1]])+1/2),2))
  part=list()
  for (s in 1:length(dda1[[1]])){
    b1=list(breaks=dda1[[1]][[s]],counts=dda1[[2]][[s]])
    b2=list(breaks=dda2[[1]][[s]],counts=dda2[[2]][[s]])
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
    #part[[s]]=joint
    mcol=c(rainbow(10, start=0, end=0.5),rainbow(190, start=0.5, end=0.7))
    image(joint,col=mcol)
    lastjoint=lastjoint+joint
  }
  mcol=c(rainbow(10, start=0, end=0.5),rainbow(190, start=0.5, end=0.7))
  image(lastjoint,col=mcol)
}
