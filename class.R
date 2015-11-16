library(magrittr)
library(plyr)
library(dplyr)
library(pryr)

x=1
attr(x,"class")="foo"

x %>% attributes()
x
x %>% class
x %>% otype


methods(histogram)


methods(mean)

teacher = function(x,...) UseMethod("teacher")
teacher.qw=function(x) print("1234")
teacher.as=function(x) print("11111111111")
aa=1
attr(aa,'class')="qw"
aa
teacher(aa)

iris$Species
iris %>% select(1:3)
"["(iris)[1]
