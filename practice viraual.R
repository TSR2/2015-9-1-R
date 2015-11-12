library(plyr)
library(dplyr)
library(magrittr)

spe=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsSpe.csv")
env=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsEnv.csv")
spa=read.csv("c:/Users/tsr/Desktop/上課用檔案/資料視覺化/DoubsFishData/NEwR data/DoubsSpa.csv")

spe[1:5,]
env[1:5,]
spa[1:5,]
spe %>% dim
env %>% dim
spa %>% dim
spe %>% names
env %>% names
spa %>% names
spa %>% summary()
