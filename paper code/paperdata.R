###################################data1
a1=c(0,3.5,7.0,10.5,14,17.5,21)
a2=c(3.094,6.5874,6.996,4.1761,1.5798,0.5667)
b1=c(0,4,7,10,13,16,19)
b2=c(4.0833,8.2672,5.1251,3.5680,1.6564,0.3)
c1=c(0,1,2,3,4,5)
c2=c(5.25,9.8,4.2167,2.9,0.8333)
a2=a2/sum(a2)
b2=b2/sum(b2)
c2=c2/sum(c2)


data1=list()
aa=bb=cc=list()
a=list(breaks=a1,count=a2)
class(a)="histogram"
b=list(breaks=b1,count=b2)
class(b)="histogram"
c=list(breaks=c1,count=c2)
class(c)="histogram"
aa[[1]]=a
bb[[1]]=b
cc[[1]]=c
data1[[1]]=aa
data1[[2]]=bb
data1[[3]]=cc
data1[[4]]=aa
data1[[5]]=bb
data1[[6]]=cc
data1
hcalEX(data1)
hcalvar(data1)
hcalcov(data1)


#############################單組資料1
a1=seq(60,210,15)
a2=c(0.00714,0.04286,0.24179,0.72488,1.2747,1.67364,1.975,0.885,0.135,0.04)
#############################單組資料2
a1=seq(125,325,25)
a2=c(0.6,0.766,2.893,4.495,4.176,2.193,0.398,0.021)
####################################################
a2=a2/sum(a2)
a=list(breaks=a1,count=a2)
class(a)="histogram"
a
data2=list()
data2[[1]]=list(a)
data2
hcalEX(data2)
hcalvar(data2)
##################################

