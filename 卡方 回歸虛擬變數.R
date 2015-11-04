glm(Species~.,family = "",data=iris)
library(foreign)
x=read.spss("C:\\Users\\tsr\\Desktop\\上課用檔案\\統計諮詢\\BW.sav",
            to.data.frame=T)
head(x)
###卡方獨立性檢定
p=table(x$race,x$smoke)
pp=table(x$smoke,x$low)
chisq.test(x =pp)
chisq.test(x =p)
####分開做獨立性檢定
x1=x[x$smoke==0,]
x2=x[x$smoke==1,]
chisq.test(x=table(x1$smoke,x1$low))
chisq.test(x=table(x2$smoke,x2$low))

names(kk)
str(kk)
####先把變數變成因子，做回歸回自動變成虛擬變數
o=as.factor(x$race)

kk=lm(low~smoke+o,data=x)
summary(kk)
glm(low~smoke+o,family = "binomial",data=x)
