# install.packages("vegan")
library(vegan)


# 1)
# Creating sub-folders
dir.create("outputs")
dir.create("data")


# 2)
# Importing the data
# Assigning a name to the data
species <- read.csv("data/BCI.csv")
species
env_var <- read.txt("data/BCI_env.txt")
env_var

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




