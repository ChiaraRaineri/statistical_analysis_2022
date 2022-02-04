# 1)
install.packages("labdsv")
library(labdsv)
library(vegan)


# 2)
# In R studio I don't have to set the working directory
setwd("C/bryce/data")

bryceveg <- read.csv("bryceveg.csv")
brycesite <- read.table("brycesite.txt", sep = " ", header = T)
