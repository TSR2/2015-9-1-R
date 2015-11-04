a=read.table("C:\\Users\\tsr\\Desktop\\ddd\\new5.txt",encoding ="UTF-8" )
b=read.table(file="C:\\Users\\tsr\\Desktop\\ddd\\new51.txt")
a$V1=as.character( a$V1)
b$V1=as.character( b$V1)
a
b
library(plyr)
p=merge(a,b,by = "V1")
write.csv(p,file="C:\\Users\\tsr\\Desktop\\ddd\\ppp.txt",row.names = F,col.names = F)
join(a,b,"V1")
