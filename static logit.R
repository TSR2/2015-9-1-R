library(foreign)

x=read.table("c:/Users/tsr/Desktop/上課用檔案/統計諮詢/CHD.csv",sep=",",head=T)
head(x)
mo=glm(chd~age,family = "binomial",data=x)
mo
names(mo)
exp(mo[[1]])
names1=paste("ms",1:7,sep="")
b=x

b$as.formula("ms1")=2


x=read.spss("c:/Users/tsr/Desktop/上課用檔案/統計諮詢/FEV.sav",to.data.frame=T)
head(x)
xx=x
xx$sex=as.factor(xx$sex)
mo=glm(fev~sex+age+hgt,data=xx)
summary(mo)
