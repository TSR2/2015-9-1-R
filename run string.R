
eval(parse(text=paste("hist(","iris[,1]",")",sep="")))
eval(parse(text="hist(iris[,2])"))


hist(eval(parse(text="iris[,3]")))

#######################

x=iris
na=names(x[,1:3])
kk=paste(na,sep="",collapse = "+")
kk=paste("Petal.Width",kk,sep="~")
lm(as.formula(kk),data=x)