# install.packages("vegan")
library(vegan)


# 1)
# Creating sub-folders
dir.create("outputs")
dir.create("data")


# 2)
# Setting the working directory
setwd("C:/project_exam/data")

# Importing the data
# Assigning a name to the data
community_matrix <- read.csv("BCI.csv")
community_matrix
env_variables <- read.csv("BCI_env.txt")
env_variables

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




