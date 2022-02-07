# 1)
install.packages("labdsv")
library(labdsv)
library(vegan)


# 2)
# In R studio I don't have to set the working directory
setwd("C:/bryce/data")
bryceveg <- read.csv("bryceveg.csv")
bryceveg
brycesite <- read.table("brycesite.txt", sep = " ", header = T)  
brtcesite

# Counting the number of rows and columns
nrow(bryceveg)  # Number of observations
ncol(bryceveg)  # Number of species
nrow(brycesite)  # Number of observations
ncol(brycesite)  # Number of environmental variables


# 3)
# https://www.rdocumentation.org/packages/labdsv/versions/2.0-1/topics/bryceveg 
# https://www.rdocumentation.org/packages/labdsv/versions/2.0-1/topics/brycesite


# 4)
# Let's do summary statistics
# I notice there are some missing values and some NAs
summary(bryceveg)
summary(brycesite)

str(bryceveg)
str(brycesite)

# Removing NAs (it removes the whole row, not only the single cell)
na.omit(brycesite)
# To check what happened
nrow(brycesite)
nrow(na.omit(brycesite))
# But now I have bryceveg that has still 50 rows, so this is not a good method

# Instead I should first check if there are NAs
# is.na(brycesite)
index_na <- which(is.na(brycesite), arr.ind = T)
index_na <- index_na[, 1]  # To have just the first column 
# Now I have a matrix that tells me exactly where the NA value is
# I want to remove row 15
brycesite <- na.omit(brycesite)
nrow(brycesite)
bryceveg <- bryceveg[- index_na, ]
nrow(bryceveg)

# But there are also empty cells that are not NAs
unique(brycesite$Habitat)
unique(brycesite$Stream)
# Put it at the beginning
# na.strings convert blank cells into NAs
brycesite <- read.table("data/brycesite.txt", 
                      sep = " ", 
                      header = T,
                      na.strings = c("NA", ""))
brycesite
index_na <- which(is.na(brycesite), arr.ind = T)
index_na <- index_na[, 1]
BCI_env <- na.omit(brycesite)
nrow(brycesite)
bryceveg <- bryceveg[- index_na, ]
nrow(bryceveg)
