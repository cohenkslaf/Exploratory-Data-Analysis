# Business Understanding
# Research: In the wine industry, factors such as alcohol content, acidity, pH, residual sugar, and volatile acidity are important determinants of wine quality. 
# High-quality wines are typically characterized by a balanced combination of these factors. For example, wines with a higher alcohol content and lower acidity tend to have a smoother and more balanced taste. 
# However, personal preference, grape variety, and production methods also play a significant role.

# Data Understanding
# The dataset we are using is from the UCI Wine Quality Data Set, which contains information about various chemical properties of red and white wines.
# The key columns are: fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH, sulphates, alcohol, and quality.
# 'Quality' is the target variable (on a scale from 0 to 10), and 'type' denotes the wine type (red or white).

# Libraries
required_packages <- c("tidyverse", "broom", "corrplot", "ggplot2", "nortest", "car")
to_install <- setdiff(required_packages, installed.packages()[, "Package"])
if (length(to_install) > 0) install.packages(to_install)

# Load libraries
library(tidyverse)
library(dplyr)
library(broom)
library(corrplot)
library(ggplot2)
library(nortest)
library(car)

# Data Loading and Merging
red_url   <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
white_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

red   <- read.csv(red_url, sep = ";")
white <- read.csv(white_url, sep = ";")

# Add 'type' column for wine type
red$type   <- "red"
white$type <- "white"

# Combine red and white wines
data <- bind_rows(red, white)

# Data Exploration
glimpse(data)
summary(data)

# Check for missing values
colSums(is.na(data))

# Exploratory Data Analysis (EDA)
# Quality distribution by wine type
ggplot(data, aes(x = factor(quality), fill = type)) +
  geom_bar(position = "dodge") +
  labs(title = "Quality Score Distribution", x = "Quality Score", y = "Count") +
  theme_minimal()

# Key chemical properties box plots
features <- c("alcohol", "pH", "residual.sugar", "volatile.acidity")
plot_list <- lapply(features, function(feat) {
  ggplot(data, aes(x = type, y = .data[[feat]])) +
    geom_boxplot() +
    labs(title = feat, y = feat) +
    theme_minimal()
})

# Print all plots
for(p in plot_list) print(p)

# Correlation Heatmap
cor_red   <- cor(filter(data, type == "red")   %>% select(-type))
cor_white <- cor(filter(data, type == "white") %>% select(-type))

par(mfrow = c(1,2))
corrplot(cor_red,   main = "Red Wine",   mar = c(0,0,1,0))
corrplot(cor_white, main = "White Wine", mar = c(0,0,1,0))

# Assumption Checking for T-Test
# 1. Normality of the data (Shapiro-Wilk test)
shapiro.test(filter(data, type == "red")$quality)
shapiro.test(filter(data, type == "white")$quality)

# 2. Equal Variance assumption (Levene's Test)
leveneTest(quality ~ type, data = data)

# Statistical Tests
# T-Test for Mean Quality
ttest <- t.test(quality ~ type, data = data)
tidy(ttest)

# Alcohol vs Quality Correlation
cor_red_alc   <- cor.test(filter(data, type == "red")$alcohol, filter(data, type == "red")$quality)
cor_white_alc <- cor.test(filter(data, type == "white")$alcohol, filter(data, type == "white")$quality)

list(
  red   = tidy(cor_red_alc),
  white = tidy(cor_white_alc)
)

# Conclusion
# Based on the exploratory analysis and statistical testing, we conclude that white wine tends to have a higher quality rating than red wine. The analysis also reveals key chemical factors that contribute to wine quality, with alcohol content and volatile acidity playing significant roles.

