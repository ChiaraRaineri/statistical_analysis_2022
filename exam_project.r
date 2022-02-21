# install.packages("vegan")
library(vegan)
library(ggplot2)

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

# Convert characters into factors
# I should order Age.cat
# unique factors within a vector
unique(BCI_env$Age.cat)  # I have only c2 and c3
factor(BCI_env$Age.cat, levels = c("c2", "c3"), ordered = T)
factor(BCI_env$Habitat, levels = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"))
factor(BCI_env$Stream, levels = c("Yes", "No"))


# 4)
# Let's do summary statistics
# I notice there are some missing values and some NAs
summary(BCI)
summary(BCI_env)


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
# You can use ggplot2
# we have numerical variables and categorical variables
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
png("outputs/Figure3.png", res = 300, width = 3000, heigh = 2000)
boxplot(BCI_env$sr ~ BCI_env$Habitat,
        main = "Distribution of species richness in respect with Habitat",
        ylim = c(70, 110),
        ylab = "Species richness",
        xlab = "Habitat type")
dev.off()
# Young and swamp are almost symmetrical because they have few observations
# The group with the highest variance is OldLow (larger max and min)


# 8)
# Species richness with respect to EnvHet
# Two numerical variables (scatterplot)
png("outputs/Figure4.png", res = 300, width = 3000, heigh = 2000)
plot(BCI_env$EnvHet, BCI_env$sr,
     xlab = "Environmental heterogeneity", 
     ylab = "Species richness")
     abline(lm$coefficients[1], lm$coefficients[2], col = "red")
dev.off()

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



