# install.packages("vegan")
library(vegan)


# 1)
# Creating sub-folders
dir.create("outputs")
dir.create("data")


# 2)
# Importing the data
BCI <- read.csv("data/BCI.csv")
BCI
BCI_env <- read.table("data/BCI_env.txt", sep = " ", header = T)  
BCI_env

# How I solved problems on BCI_env: 
View(BCI_env)
# Problem: if I do the function View I can't see the names of the columns up above, so I write header = T
# Error: line 16 did not have 9 elements (9 columns but not enough elements!)
# To solve this error I write fill = T so BCI_env <- read.table("data/BCI_env.txt", fill = T, header = T)
# But this method shifts the values to the left, so I put sep = " " instead of fill


# class(env_variables)
# class(community_matrix)
# Extracting the informations about column names, class and row names
attributes(community_matrix)
attributes(env_variables)

# Counting the number of rows and columns
dim(community_matrix)
dim(env_variables)
# The number of observation refers to the rows, so the number of species and environmental variables refers to the columns
# The columns indicates the species (225) and the environmental variables


# 3)
?BCI
# The elements that are missing from BCI_env are the names of the columns




colnames(env_variables) <- c("UTM.EW", "UTM.NS", "Precipitation", "Elevation", "Age.cat", "Geology", "Habitat", "River", "EnvHet")
env_variables




