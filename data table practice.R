library(magrittr)
library(plyr)
library(dplyr)
library(data.table)
?recode
write.csv(iris,file="c:/Users/tsr/Desktop/iris.csv")
x=iris
names(x)=paste('v',1:5,sep="")
x=tbl(x)
x[x$v1>7,]
x %$% x[v1>7,]
cc=x %>% group_by(v5) %>% do(model=lm(v1~v2,data=.))
cc$model
cc=x %>% group_by(v5) %>% do(histo=hist(.$v1))
cc$histo
x=iris

