#library(VIM)
#library(VIMGUI)
library(plyr)
library(dplyr)
library(magrittr)


pp=read.table('sleep.txt')

pp %>% head
pp %>% dim

pp %>% na.omit
pp %>% na.omit %>% dim
complete.cases(pp)
pp %>% '['(complete.cases(pp),)


cc=iris
cc[1,1]=NA
complete.cases(cc)

cov(pp)
cov(pp,use = 'complete.obs')
cov(pp,use = 'pairwise.complete.obs')
pp %>% is
pp %>% select(NonD) 
pp %>% names

pp1=pp %>% '['(complete.cases(pp),)


