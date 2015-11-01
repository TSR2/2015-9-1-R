f=function(x,y)x+y
f(1:9,2:11)


####################簡單版
f=function(x,y) x^2-y
sq=function(y){
  v=1;s=100
  while (all(s>(10^-10))){
    v=v-f(v,y)/(2*v)
    s=f(v,y)
  }
  v
}
sq(c(3,4))

##################較完整版
sq=function(y,f=expression(x^2-y),tol=10^-10){
  x=1;s=100
  fp=D(f,"x")
  while (all(s>tol)){
    x=x-eval(f)/eval(fp)
    s=eval(f)
  }
  x
}
sq(c(3,4))

################
a=c(expression(x^2-y),expression(x^2-y^2))
a[2]
ta=c("x","y")
m=NULL
m=c(m,D(a[1],ta[1]))
is(m)
sqq=function(ex=a){
  x=1;y=1;s=100;
  ta=c("x","y")
  m=NULL
  for (j in 1:2){
    for(i in 1:2){
      m[i+j-1]=D(ex[i],ta[j])
    }
  }
  m
}
sqq()
