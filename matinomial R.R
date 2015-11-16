x=iris
names(x)=paste("v",1:5,sep="")
aggregate(cbind(v1,v2)~v5,data=x,sum)
x %>% group_by(v5) %>% summarise(mv1=sum(v1),mv2=sum(v2))


library(moments)
my_summary <- function(x) {
  require(moments)
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}
sapply(iris[,1:3], my_summary)
c(sd(x$v1),sd(x$v2))

sapply(iris[,1:4],summary)
iris[,1:4] %>% summary