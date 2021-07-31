setwd('/...')
library(tidyverse)
library(outliers)
library(PerformanceAnalytics)

data <- read.csv(
  "Final_data_cleaned.csv", 
  sep = ",", 
  dec = ".")

# inspect data
glimpse(data)


#----Measures----
# Conflict Strength & Dilemma Strength Dg
plot(data$DilemmaStrength_Dg,data$ConflictStrength, main="Scatterplot", xlab="Dilemma Strength Dg", ylab="Conflict Strength") 
abline(lm(data$ConflictStrength ~ data$DilemmaStrength_Dg), col="red") # regression line (y~x) 
cor.test(data$DilemmaStrength_Dg, data$ConflictStrength, method=c("pearson", "kendall", "spearman"))

# Conflict Strength & Dilemma Strength Dr
plot(data$DilemmaStrength_Dr,data$ConflictStrength, main="Scatterplot", xlab="Dilemma Strength Dr", ylab="Conflict Strength") 
abline(lm(data$ConflictStrength ~ data$DilemmaStrength_Dr), col="red") # regression line (y~x) 
cor.test(data$DilemmaStrength_Dr, data$ConflictStrength, method=c("pearson", "kendall", "spearman"))

# Dilemma Strength Dg & Dilemma Strength Dr
plot(data$DilemmaStrength_Dg,data$DilemmaStrength_Dr, main="Scatterplot", xlab="Dilemma Strength Dg", ylab="Dilemma Strength Dr") 
abline(lm(data$DilemmaStrength_Dr ~ data$DilemmaStrength_Dg), col="red") # regression line (y~x) 
cor.test(data$DilemmaStrength_Dg, data$DilemmaStrength_Dr, method=c("pearson", "kendall", "spearman"))



#----Outcome----
# Conflict Strength & integrative Potential achieved
plot(data$ConflictStrength,data$Percentage_IntegrativePotential_achieved, main="Scatterplot", xlab="Conflict Strength", ylab="% of integrative Potential achieved") 
abline(lm(data$Percentage_IntegrativePotential_achieved ~ data$ConflictStrength), col="red") # regression line (y~x) 
cor.test(data$ConflictStrength, data$Percentage_IntegrativePotential_achieved, method=c("pearson", "kendall", "spearman"))

# Dilemma Strength Dg & integrative Potential achieved
plot(data$DilemmaStrength_Dg,data$Percentage_IntegrativePotential_achieved, main="Scatterplot", xlab="Dilemma Strength Dg", ylab="% of integrative Potential achieved") 
abline(lm(data$Percentage_IntegrativePotential_achieved ~ data$DilemmaStrength_Dg), col="red") # regression line (y~x) 
cor.test(data$DilemmaStrength_Dg, data$Percentage_IntegrativePotential_achieved, method=c("pearson", "kendall", "spearman"))

# Dilemma Strength Dr & integrative Potential achieved
plot(data$DilemmaStrength_Dr,data$Percentage_IntegrativePotential_achieved, main="Scatterplot", xlab="Dilemma Strength Dr", ylab="% of integrative Potential achieved") 
abline(lm(data$Percentage_IntegrativePotential_achieved ~ data$DilemmaStrength_Dr), col="red") # regression line (y~x) 
cor.test(data$DilemmaStrength_Dr, data$Percentage_IntegrativePotential_achieved, method=c("pearson", "kendall", "spearman"))



#----Correlation Matrix----
cor_data <- data[, c("Percentage_IntegrativePotential_achieved",
                      "ConflictStrength",
                      "DilemmaStrength_Dg",
                      "DilemmaStrength_Dr")]
chart.Correlation(cor_data, histogram=TRUE)
