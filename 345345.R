f=function(x,y)x+y
f(1:9,2:11)



f=function(x,y) x^2-y
sq=function(y){
  v=1;s=100
  while (all(s>(10^-3))){
    v=v-f(v,y)/(2*v)
    s=f(v,y)
  }
  v
}
sq(c(5,7))

par(col=4,bg=3)
plot(1:10)
sel=identify(1:10,labels = 1:3)


