library(magrittr)
library(plyr)
library(dplyr)

library(xlsx)
data=read.xlsx("C:/Users/TSR/Desktop/多元統計分析與R語言建模/mvstats.xls","d5.1")
data
data %>% names
d5.1=data[c(-46,-47),1:4]
d5.1
cor(d5.1)
model=glm(y~x1+x2+x3,family = "binomial",data=d5.1)
model %>% summary

stepp=step(model,direction = "both")
summary(stepp)
