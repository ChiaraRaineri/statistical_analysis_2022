# install.packages("vegan")
library(vegan)

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

# str(BCI)
# str(BCI_env)
# These are bidimensional objects (matrices and data frames)


# 3)
?BCI
# The elements that are missing from BCI_env are the names of the columns
# I solved it in the process of importing data by adding sep


# 4)
# Let's do summary statistics
# I notice there are some missing values and some NAs
summary(BCI)
summary(BCI_env)

str(BCI)
str(BCI_env)

# Removing NAs (it removes the whole row, not only the single cell)
na.omit(BCI_env)
# To check what happened
nrow(BCI_env)
nrow(na.omit(BCI_env))
# But now I have BCI that has still 50 rows, so this is not a good method

# Instead I should first check if there are NAs
# is.na(BCI_env)
index_na <- which(is.na(BCI_env), arr.ind = T)
index_na <- index_na[, 1]  # To have just the first column 
# Now I have a matrix that tells me exactly where the NA value is
# I want to remove row 15
BCI_env <- na.omit(BCI_env)
nrow(BCI_env)
BCI <- BCI[- index_na, ]
nrow(BCI)

# But there are also empty cells that are not NAs
unique(BCI_env$Habitat)
unique(BCI_env$Stream)
# Put it at the beginning
# na.strings convert blank cells into NAs
BCI_env <- read.table("data/BCI_env.txt", 
                      sep = " ", 
                      header = T,
                      na.strings = c("NA", ""))
BCI_env
index_na <- which(is.na(BCI_env), arr.ind = T)
index_na <- index_na[, 1]
BCI_env <- na.omit(BCI_env)
nrow(BCI_env)
BCI <- BCI[- index_na, ]
nrow(BCI)


# Should we convert something into a factor? Maybe not
# BCI_env$Habitat <- factor(BCI_env$Habitat,
#                          levels = c("Swamp", "OldLow", "OldSlope", "OldHigh", "Young"),
#                          ordered = T)


# 5) ???
# plot the univariate distribution (so the distribution of just one variable)
# You can use ggplot2
# we have numerical variables and categorical variables
barplot(BCI_env)  


# 6)
# I use the function decostand with method pa to convert the table into presence-absence
# The community matrix!
BCI <- decostand(BCI, method = “pa”)
str(BCI)

# write.csv(BCI, file = "BCI.csv")


# 7a)
# I use specnumber to calculate species richness 
# By doing BCI_env$sr I create another column in the BCI_env matrix
BCI_env$sr <- specnumber(BCI)
# sort(specnumber(BCI, MARGIN = 2))  # Maybe not necessary
# plot(sort(specnumber(BCI, MARGIN = 2), decreasing = T))  # Maybe not necessary


# 7b)
summary(sr)


# 7c) 
p <- ggplot(data = BCI_env, aes(x = Habitat, y =sr, fill= Habitat)) +
geom_boxplot() +
labs(title = "Distribution of species richness in respect with the variable Habitat", x = "Habitat", y = "Species richness") + 
geom_point()
pl <- p + scale_fill_discrete(name = "Legend")
pl


# I don't know if this is useful, I plotted the species richness with the number of variables
# to export (res is for resolution): 
# I have to modify width and lenght because the margins are not enough for the resolution I chose
png("Figure1.png", res = 300, width = 3000, height = 2000)
hist(sr, main = "", xlab = "species richness", ylab = "number of variables")
dev.off  # DOESNT WORK


# 8)
# Two numerical variables (scatterplot)

# useless
plot(BCI_env$sr,BCI_env$Elevation, main = "correlation plot", xlab = "Species richness", ylab = "Elevation")
# Correlation test
cor.test(BCI_env$Elevation, BCI_env$sr)  # standard deviation is zero
# Regression model
lm(sr ~ Elevation, data = BCI_env)
summary(lm(sr ~ Elevation, data = BCI_env))

# useless
plot(BCI_env$sr,BCI_env$Precipitation, main = "correlation plot", xlab = "Species richness", ylab = "Precipitation")
# Correlation test
cor.test(BCI_env$Precipitation, BCI_env$sr)  # standard deviation is zero
# Regression model
lm(sr ~ Precipitation, data = BCI_env)
summary(lm(sr ~ Precipitation, data = BCI_env))

plot(BCI_env$sr,BCI_env$EnvHet, main = "correlation plot", xlab = "Species richness", ylab = "EnvHet" )
# Correlation test
cor.test(BCI_env$EnvHet, BCI_env$sr)  
# p-value is high so the correlation is significant
# Correlation (cor) is close to zero, so it is weak (?)

# Regression model
reg_model <- lm(sr ~ EnvHet, data = BCI_env)
summary(reg_model)
# This gives the intercept and the slope
# Multiple R-squared significance?
# Are the residuals normally distributed?

