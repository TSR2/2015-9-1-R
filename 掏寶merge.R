a=read.table("C:\\Users\\tsr\\Desktop\\掏寶\\new5.txt",encoding ="UTF-8" )
b=read.table(file="C:\\Users\\tsr\\Desktop\\掏寶\\new51.txt")
a$V1=as.character(a$V1)
b$V1=as.character(b$V1)
a
b
a$V1==b$V1
str(a)
b
library(plyr)
p=merge(a,b,by = "V1")
write.csv(p,file="C:\\Users\\tsr\\Desktop\\掏寶\\pppp.txt",
          row.names = F,col.names = NA,quote = F)
join(a,b,"V1")
