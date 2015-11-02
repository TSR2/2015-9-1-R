p=summary(iris)
library(stringr)
a=p[1,5]
aa=c("aaaaaaaaavbab")
aa=strsplit(aa,"")
grep("a",aa[[1]])
grepl("a",aa)
regexpr("a",aa)
sub("a",x=aa)
str_trim("  dd  ")
str_locate("aa",aa)
str_locate_all("a",aa)
str_length(aa)
xx=substr(x = a,12,stop=100)
noquote(xx)
kk=noquote("dd ")
char(aa)
a=regexpr(" ",xx)
xx=substr(x = xx,1,stop=a[1]-1)
xx

vv="aab;asd,fsfddf:dfds;"
for (i in c(",",";",":")){
  vv=strsplit(x = vv[[1]],split =  i)
}

strsplit(x = vv,split = c(",",";",":"))
