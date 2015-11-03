createbeta=function(n=200,v=0.1){
  ff=function(x,v,uni){x^v*(2-x)^v-uni}
  bb=0
  uu=0
  for (i in 1:n){
    uni=runif(1)
    uu[i]=uni
    pp=uniroot(ff,c(0,1),v=v,uni=uni,tol=0.00000000000000000000001)
    bb[i]=pp[[1]]
  }
  rb=bb
}


#####################
kfail=function(a=1,b=1,n=100,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      p=ifelse(test = taget>=ru[j],"S","F")
      list=c(list,p)
      if (taget<ru[j]) {break}
    }
  }
  list
  count=sum(list=="F")/n
  list(log=list,persent=count)
}

####################################
mstep=function(a,b,m,n,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  mtotal=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ###?p?G?????u?????Ƥj??m,?h???X???\?v?̰??????U?h
      if (length(mtotal)==m) {taget=rb[which.max(mtotal)]} 
      ru=runif(n)
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      if (taget>=ru[j]) p="S"
      else p="F"
      ###?????C???????絲?G
      list=c(list,p)
      ###?֭p?Ӥ??u???\??????
      if (taget>=ru[j]) {scount=scount+1}
      ####?ϥΪ????u?bm?ӥH?U?A?ӥB?Ӧ??????O???Ѫ??A?O???U?Ӧ????窺???\?`????
      if (taget<ru[j] & length(mtotal)<m) {
        mtotal=c(mtotal,scount)
        break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}

####################################
mreducestep=function(a,b,m,n,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  mtotal=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      p=ifelse(test = taget>=ru[j],"S","F")
      ###?????C???????絲?G
      list=c(list,p)
      ###?֭p?Ӥ??u???\??????
      if (taget>ru[j]) {
        scount=scount+1
      }else{
        ####?ϥΪ????u?bm?ӥH?U?A?ӥB?Ӧ??????O???Ѫ??A?O???U?Ӧ????窺???\?`????
        mtotal=c(mtotal,scount)
        ######???u???\?Ƥ???m?A?????u
        if (scount<m) break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}

##########################################
Nkfail=function(a,b,n,N,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  mtotal=c()
  p=0
  count=0
  #####x=1?O?N???٥????찵?쩳?????u,x=0?O?N???????F
  x=1
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      #####?b?j??N?H???A?Ĥ@???X?{F?A?P?_???\?v?̰??????u
      if (length(mtotal)>=N & p=="F" & x==1) {taget=rb[which.max(mtotal)];x=0}
      ru=runif(n)
      ###?qbeta?��t???X?Ӫ??p?G??uniform?j?A?h??S,?Ϥ???F
      if (taget>=ru[j]) {
        p="S"
        scount=scount+1
      }
      else p="F"
      ###?????C???????絲?G
      list=c(list,p)
      if (taget<ru[j] & x==1) {
        mtotal=c(mtotal,scount)
        break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}

###########################################
ifail=function(a=1,b=1,n=100,alpha=0.1,t=0){
  if (t==1){rb=createbeta(n,alpha)}
  else {rb=rbeta(n = n,shape1 = a,shape2 = b)}
  list=c()
  mtotal=c()
  p=0
  count=0
  for (i in 1:n){
    taget=rb[i]
    scount=0
    if (length(list)>=n){break}
    for (j in 1:n){
      if (length(list)>=n){break}
      ru=runif(n)
      p=ifelse(test = taget>=ru[j],"S","F")
      list=c(list,p)
      if (taget>=ru[j]) {
        scount=scount+1
      }else{
        mtotal=c(mtotal,scount)
        if ((j-scount)>i) break
      }
    }
  }
  persent=sum(list=="F")/n
  list(list,persent,mtotal)
}


##########################################
runs=function(run,fun,...){
  v=rep(0,run)
  for (i in 1:run){
    v[i]=fun(...)[[2]]  
  }
  mean(v)
}
