setwd('/Users/sarah/Uni/Bachelorarbeit/neuesThema/Metaanalysis/Data/final')

data <- read.csv(
  "Final_data.csv", 
  sep = ",", 
  dec = ".")


#---- New Data Set without excluded variables----
# exclude studies with missing values for achieved Integrative Potential, Integrative Potential, and Dilemma Strengths
data <- subset(data, !is.na(Percentage_IntegrativePotential_achieved) &
                 !is.na(IntegrativePotential) &
                 !is.na(DilemmaStrength_Dg) & 
                 !is.na(DilemmaStrength_Dr)) # 174 excluded, n = 267

# exclude experiments with a mediator
data <- subset(data, Mediator == 2) # 7 excluded, n = 260

# exclude experiments with more than two Parties 
data <- subset(data, Number.of.Parties ==2 ) # 12 excluded, n = 248

# Payoff symmetry, exclude unsymmetrical and manipulated payoffs
data <- subset(data, Symmetry.of.Pay.offs != 2 & Symmetry.of.Pay.offs != 3) # 7 excluded, n = 241

# only information about own payoff, exclude manipulated payoffs
data <- subset(data, Information != 4) # 2 excluded, n = 239

# exclude team sizes of more than one person
data <- subset(data, Team.Size == 1) # 14 excluded, n = 225

# exclude studies with uncorrected impasses
cond <- data$Corrected.Joint.Outcomes !=1 & data$Corrected.joint.gains.extrapolated == 2 
cond2 <- data$Corrected.Joint.Outcomes == 2 & data$Corrected.joint.gains.extrapolated == 0 & data$Number.of.partial.Impasses > 0 
cond3 <- data$Corrected.Joint.Outcomes== 3 & data$Corrected.joint.gains.extrapolated == 0 & data$Number.of.partial.Impasses > 0 
data <- subset(data, !cond & !cond2 & !cond3) # 11 excluded, n = 214

# exclude experiments with nonlinear utility functions
data <- subset(data, data$Utility.Function == 1) #8 excluded excluded, n = 206

# exclude experiments that did not reach any integrative Potential
data <- subset(data, Percentage_IntegrativePotential_achieved > 0) # 4 excluded, n = 202


# export to csv 
write.csv(data,"/Users/sarah/Uni/Bachelorarbeit/neuesThema/Metaanalysis/Data/final/Final_data_excluded.csv", row.names = FALSE)

