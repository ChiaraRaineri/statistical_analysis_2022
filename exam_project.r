# install.packages("vegan")
library(vegan)

# 1)
# Creating sub-folders
# dir.create("outputs")
# dir.create("data")


# 2)
# Importing the data
BCI <- read.csv("data/BCI.csv")  # Community matrix
BCI
BCI_env <- read.table("data/BCI_env.txt", sep = " ", header = T)  # Environmental variables
BCI_env

# How I solved problems on BCI_env: 
View(BCI_env)
# Problem: if I do the function View I can't see the names of the columns up above, so I write header = T
# Error: line 16 did not have 9 elements (9 columns but not enough elements!)
# To solve this error I write fill = T so BCI_env <- read.table("data/BCI_env.txt", fill = T, header = T)
# But this method shifts the values to the left, so I put sep = " " instead of fill

# Counting the number of rows and columns
nrow(BCI)  # Number of observations
ncol(BCI)  # Number of species
nrow(BCI_env)  # Number of observations
ncol(BCI_env)  # Number of environmental variables

# Matrices are better than data frames when the number of species is very high

# 3)
?BCI
# BCI is a tree counts in Barro Colorado Island, that is an island within Panama 
# Tree counts in 1-hectare plots in the Barro Colorado Island and associated site information

# They are bidimensional data frames
# In a data frame in a column I have the same values, while in a row I have different values
# In a community matrix I have always the same values
# The difference between a list and a data frame is that a data frame is a particular kind of list where every component has the same length
str(BCI)  # Community matrix
str(BCI_env)  # Data frame
# The elements that are missing from BCI_env are the names of the columns
# I solved it in the process of importing data by adding sep
# Talk about data types

# Convert characters into factors, because they can be categorical (factors are not just atomic vectors, they are objects)
# I should order Age.cat
# unique factors within a vector
unique(BCI_env$Age.cat)  # I have only c2 and c3
BCI_env$Age.cat <- factor(BCI_env$Age.cat, levels = c("c2", "c3"), ordered = T)
BCI_env$Habitat <- factor(BCI_env$Habitat, levels = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"))
BCI_env$Stream <- factor(BCI_env$Stream, levels = c("Yes", "No"))


# 4)
# Let's do summary statistics
# I notice there are some missing values and some NAs
summary(BCI_env)
# For the factors the numbers are the counts for each category


# I should check if there are NAs
# NAs can be removed with the na.omit function (SPIEGA I PASSAGGI)
index_na <- which(is.na(BCI_env), arr.ind = T)
index_na <- index_na[, 1]  # To have just the first column 
# Now I have a matrix that tells me exactly where the NA value is
# I want to remove row 15
BCI_env <- na.omit(BCI_env)
nrow(BCI_env)
BCI <- BCI[- index_na, ]
str(BCI)

write.table(x = BCI_env, file = "outputs/BCI_env.txt")
# I could write just outputs because at the beginning I created a project

# But there are also empty cells that are not NAs
unique(BCI_env$Habitat)
unique(BCI_env$Stream)
# Put it at the beginning
# na.strings convert blank cells into NAs
BCI_env <- read.table("data/BCI_env.txt", 
                      sep = " ", 
                      header = T,
                      na.strings = c("NA", ""))




# 5) 
# plot the univariate distribution (so the distribution of just one variable)
# we have numerical variables and categorical variables
# I have to use the function table to make a contingency table that contains the sum of the observations for each category
# Distribution of habitat types:
png("outputs/Figure1.png", res = 300, width = 3000, heigh = 2000)
barplot(table(BCI_env$Habitat),
        main = "Types of Habitat",
        ylim = c(0, 26),
        ylab = "Frequency",
        xlab = "Habitat type")
dev.off()
# Young and swamp are rare categories or even outlayers

# How the streams are distributed among the different habitats
png("outputs/Figure2.png", res = 300, width = 3000, heigh = 2000)
barplot(table(BCI_env$Stream, BCI_env$Habitat),
        main = "Presence or absence of Stream",
        ylim = c(0, 26),
        ylab = "Frequency",
        xlab = "Habitat type",
       legend.text = T)
dev.off()


# 6)
# I use the function decostand with method pa to convert the table into presence-absence
# The community matrix!
BCI <- decostand(BCI, method = "pa")

write.csv(x = BCI, file = "outputs/BCI.csv")


# 7a)
# I use specnumber to calculate species richness 
# By doing BCI_env$sr I create another column in the BCI_env matrix
BCI_env$sr <- specnumber(BCI)

# If I have to calculate species richness without specnumber I should count the values major than zero (?)


# 7b)
summary(BCI_env$sr)


# 7c) 
boxplot(BCI_env$sr ~ BCI_env$Habitat,
        main = "Distribution of species richness in respect with Habitat",
        ylim = c(70, 110),
        ylab = "Species richness",
        xlab = "Habitat type")
# Young and swamp are almost symmetrical because they have few observations
# The group with the highest variance is OldLow (larger max and min)


# 8)
# Species richness with respect to EnvHet
# Two numerical variables (scatterplot)
plot(BCI_env$EnvHet, BCI_env$sr,
     xlab = "Environmental heterogeneity", 
     ylab = "Species richness")
abline(lm(BCI_env$sr ~ BCI_env$EnvHet, data = BCI_env),
       col = "red")

# Pearson's correlation test
cor.test(BCI_env$EnvHet, BCI_env$sr)  
# p-value is high so the correlation is not significant, so we can't reject the null hypothesis
# Correlation (cor) is close to zero, so it is not significant

# Regression model
lm <- lm(sr ~ EnvHet, data = BCI_env)
summary(lm)
# This gives the intercept and the slope
# Multiple R-squared is low: poor fit of the model (EnvHet doesn't explain anything about the species richness in our model)
# Only a small percentage (2,2 %) of variability in our model is explained
# Are the residuals normally distributed?







# CODE ON R STUDIO

# Statistical analysis R project
# Chiara Raineri and Komal Iftikhar


# Task 1
# Creating sub-folders
dir.create(path = "outputs")
dir.create(path = "data")

# Installing the package if required and recalling the library
# install.packages(vegan)
library(vegan)


# Task 2
# Importing the datasets
BCI <- read.csv("data/BCI.csv")  # Community matrix
BCI_env <- read.table("data/BCI_env.txt", 
                      sep = " ", 
                      header = T,
                      na.strings = c("NA", ""))

# sep = " " is used to correct the shifted values
# header = T is used to transform the first row into column titles
# na.strings = c("NA", "") is used to transform blank spaces into NAs

# Counting the number of observations, species and environmental variables
nrow(BCI)      # Number of observations = number of rows (50)
ncol(BCI)      # Number of species = number of columns in the community matrix (225)
ncol(BCI_env)  # Number of environmental variables = number of columns in the data frame (9)


# Task 3
# Checking the documentation
?BCI

# Checking the structure of the dataset
str(BCI)  
# data.frame of	50 observations of  225 variables
# All the variables are integer
str(BCI_env)
# data.frame of	50 observations of  9 variables
# There are variables that are integer, numeric and charachters


# Transforming columns to match types and structures described in the documentation
# Checking Age.cat levels
unique(BCI_env$Age.cat)
# c3 is older than c2
# Transforming Age.cat into an ordered factor
BCI_env$Age.cat <- factor(BCI_env$Age.cat, 
       levels = c("c2", "c3"), 
       ordered = T)

# Transforming Stream into a factor (not ordered)
BCI_env$Stream <- factor(BCI_env$Stream, 
       levels = c("Yes", "No"))

# Transforming Habitat into a factor (not ordered)
BCI_env$Habitat <- factor(BCI_env$Habitat, 
       levels = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"))

# Transforming Geology into a factor (not ordered)
BCI_env$Geology <- factor(BCI_env$Geology, 
                          levels = c("Tb"))

str(BCI_env)


# Task 4
# Calculating summary statistics
summary(BCI_env)


# Checking if there are any NAs
# NAs can be removed with the na.omit function
index_na <- which(is.na(BCI_env), arr.ind = T)
index_na  # Position of the NAs (row and column)
index_na <- index_na[, 1]  # Subsetting the first column (row)

# Removing NA values
BCI_env <- na.omit(BCI_env)
nrow(BCI_env)  # Three rows were removed
nrow(BCI)  # But in BCI there are still those three rows
# Omitting the corresponding rows in BCI
BCI <- BCI[- index_na, ]

# Now the number of rows is equal to 47

# Exporting the datasets with NAs removed
write.table(x = BCI_env, "outputs/BCI_env_modified.txt")
write.table(x = BCI, "outputs/BCI_modified.txt")


# Task 5
# Graphically exploring the univariate distribution of environmental variables 
# Plotting a histogram for the numerical variable EnvHet
hist(BCI_env$EnvHet,
     main = "Relative Environmental Heterogeneity distribution",
     xlab = "Environmental Heterogeneity")

# Plotting and exporting a bar chart for the distribution of Habitat types
png("outputs/Figure1.png", res = 300, width = 3000, heigh = 2000)
barplot(table(BCI_env$Habitat),
        main = "Types of Habitat",
        ylim = c(0, 26),
        ylab = "Frequency",
        xlab = "Habitat type")
dev.off()

# Plotting and exporting a stacked bar chart for the distribution of streams among the different habitats
png("outputs/Figure2.png", res = 300, width = 3000, heigh = 2000)
barplot(table(BCI_env$Stream, BCI_env$Habitat),
        main = "Presence or absence of Stream",
        ylim = c(0, 26),
        ylab = "Frequency",
        xlab = "Habitat type",
        legend.text = T)
dev.off()


# Task 6
# Converting the community matrix into presence/absence using decostand function (vegan package)
BCI <- decostand(BCI, method = "pa")

# Exporting presence/absence matrix in .csv format
write.csv(x = BCI, file = "outputs/BCI_pa.csv")


# Task 7
# Calculating species richness
# Also adding it to the environmental variables dataframe as a new column
BCI_env$sr <- specnumber(BCI)

# Summary statistics for species richness
summary(BCI_env$sr)

# How does species richness distribute in respect with Habitat variable? 
# Making a boxplot 
boxplot(BCI_env$sr ~ BCI_env$Habitat,
        main = "Distribution of species richness in respect with Habitat",
        ylim = c(70, 110),
        ylab = "Species richness",
        xlab = "Habitat type")

# Task 8
# Plotting the distribution of species richness in respect with the available numeric environmental variables
# Plotting Species richness with respect to EnvHet (two numerical variables = scatterplot)
plot(BCI_env$EnvHet, BCI_env$sr,
     xlab = "Environmental heterogeneity", 
     ylab = "Species richness")
abline(lm(BCI_env$sr ~ BCI_env$EnvHet, data = BCI_env),
       col = "red")  # Regression line

# Pearson's correlation test
cor.test(BCI_env$EnvHet, BCI_env$sr)
# null hypothesis: correlation equal to 0
# alternative hypothesis: true correlation is not equal to 0
# p-value = 0.968 -> non significant -> null hypothesis can't be rejected
# cor = -0.006018848 -> not significant correlation between sr and EnvHet 

# Regression model
lm <- lm(sr ~ EnvHet, data = BCI_env)
summary(lm)
# Adjusted R-squared = -0.02219 -> poor fit of the model
# p-value = 0.968 -> very high value -> the mode is not significant

