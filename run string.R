
eval(parse(text=paste("hist(","iris[,1]",")",sep="")))
eval(parse(text="hist(iris[,2])"))


hist(eval(parse(text="iris[,3]")))
