setwd('/Users/sarah/Desktop/meta-tests')
library(metafor)
library(tidyverse)
library(outliers)
library(PerformanceAnalytics) 

#----PREPARE DATA----
# read the data set
data <- read.csv(
  "meta-dataset.csv", 
  sep = ",", 
  dec = ".")

# inspect data
glimpse(data)

# inspect and visualize important values
summary(data$Percentage_IntegrativePotential_achieved)
hist(data$Percentage_IntegrativePotential_achieved, breaks= 100)
summary(data$SD_meta)
hist(data$SD_meta, breaks = 100)
summary(data$SE)
hist(data$SE,  breaks = 100)

# add impasse scores relative to unit of analysis
data$perc_impasses_total <- data$Number.of.total.Impasses / data$N..unit.of.analysis..total.
data$perc_impasses_partial <-  data$Number.of.partial.Impasses / data$N..unit.of.analysis..total.


#----MULTILEVEL META ANALYSIS - MEAN EFFECT SIZE----
# generate the null model
full.model <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                     V = Variance, 
                     slab = PaperNo,
                     data = data,
                     random = ~ 1 | PaperNo/ID, 
                     test = "t", 
                     method = "REML")

# calculate the amount of heterogeneity
i2 <- var.comp(full.model)
summary(i2)
plot(i2) # plot heterogeneity

# generate a forest plot (caterpillar)
forest(full.model, header=TRUE,
       slab = data$ID,
       order="obs", 
       showweights = TRUE, 
       alim = c(0,2),
        ilab.xpos=-1,  ylim=c(0,119))
# draw points once more to make them more visible
points(sort(full.model$yi), full.model$k:1, pch=19, cex=0.2)



#----MULTILEVEL META ANALYSIS - CONFLICT STRENGTH----
# model with Conflict Strength 
full.model_mod <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                         V = Variance, 
                         slab = PaperNo,
                         data = data,
                         random = ~ 1 | PaperNo/ID, 
                         mods = ~ ConflictStrength,
                         test = "t", 
                         method = "REML")

# plot the model as a regression plot
regplot(full.model_mod, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved")


# summary statistics including goodness of fit
summary.rma(full.model_mod) 
# confidence interval
confint.rma.mv(full.model_mod)
# weights
weights.rma.mv(full.model_mod) 

# pseudo R^2 value compared to the null model
r_squared_mod <- (sum(full.model$sigma2) - sum(full.model_mod$sigma2)) / sum(full.model$sigma2)



#----MULTILEVEL META ANALYSIS - CONFLICT STRENGTH FULL MODEL----
# correlation matrix to investigate multicollinearity
data[,c("ConflictStrength", 
        "Performance.based.incentive", 
        "X..Number.of.distributive.issues", 
        "X..Number.of.issues",
        "perc_impasses_total")] %>% chart.Correlation()


# generate a full Conflict Strenght model 
full.model_all <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                         V = Variance, 
                         slab = PaperNo,
                         data = data,
                         random = ~ 1 | PaperNo/ID, 
                         mods = ~ ConflictStrength *
                           Performance.based.incentive +
                           X..Number.of.issues +
                           perc_impasses_total,
                         test = "t", 
                         method = "REML")

# compare pseudo r^2 from null model and full Conflict Strength model
r_squared_all <- (sum(full.model$sigma2) - sum(full.model_all$sigma2)) / sum(full.model$sigma2)

# plot the regression model as a bubble plot, moderators plotted can be changed via the mod argument
regplot(full.model_all,mod = 2, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved")

# summary statistics including goodness of fit
summary.rma(full.model_all) 
# confidence interval
confint.rma.mv(full.model_all)
# weights
weights.rma.mv(full.model_all) 





#----DILEMMA STRENGTHS----
#model with Dilemma Strength Dg as main moderator
full.model_Dg <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                         V = Variance, 
                         slab = PaperNo,
                         data = data,
                         random = ~ 1 | PaperNo/ID, 
                         mods = ~ DilemmaStrength_Dg,
                         test = "t", 
                         method = "REML")

# plot the model as a regression plot
regplot(full.model_Dg, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved")

# compare pseudo r^2 from null model and Dg model
r_squared_Dg <- (sum(full.model$sigma2) - sum(full.model_Dg$sigma2)) / sum(full.model$sigma2)


# generate a full Dg model 
# control for distributive issues as dilemma strengths do no acocunt for them
full.model_Dg_all <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                        V = Variance, 
                        slab = PaperNo,
                        data = data,
                        random = ~ 1 | PaperNo/ID, 
                        mods = ~ DilemmaStrength_Dg *
                          Performance.based.incentive +
                          X..Number.of.issues +
                          X..Number.of.distributive.issues + 
                          perc_impasses_total,
                        test = "t", 
                        method = "REML")

# compare pseudo r^2 from null model and full Dg model
r_squared_Dg_all <- (sum(full.model$sigma2) - sum(full.model_Dg_all$sigma2)) / sum(full.model$sigma2)


# model with Dilemma Strength Dr as main moderator
full.model_Dr <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                        V = Variance, 
                        slab = PaperNo,
                        data = data,
                        random = ~ 1 | PaperNo/ID, 
                        mods = ~ DilemmaStrength_Dr,
                        test = "t", 
                        method = "REML")

# plot the regression model
regplot(full.model_Dr, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved")

# compare pseudo r^2 from null model and Dr model
r_squared_Dr <- (sum(full.model$sigma2) - sum(full.model_Dr$sigma2)) / sum(full.model$sigma2)


# generate a full Dr model 
full.model_Dr_all <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                            V = Variance, 
                            slab = PaperNo,
                            data = data,
                            random = ~ 1 | PaperNo/ID, 
                            mods = ~ DilemmaStrength_Dr *
                              Performance.based.incentive +
                              X..Number.of.issues +
                              X..Number.of.distributive.issues +
                              perc_impasses_total,
                            test = "t", 
                            method = "REML")

# compare pseudo r^2 from null model and full Dr model
r_squared_Dr_all <- (sum(full.model$sigma2) - sum(full.model_Dr_all$sigma2)) / sum(full.model$sigma2)


#----WITHOUT OUTLIERS----
# identify studies with an outlying standard deviation
boxplot.stats(data$SD_meta)$out # 6 studies identified

# remove outliers and save a new test data set 
testdata_wo <- subset(data, SD_meta < 0.2805700)

# generate a null model without outliers
full.model_wo <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                     V = Variance, 
                     slab = PaperNo,
                     data = testdata_wo,
                     random = ~ 1 | PaperNo/ID, 
                     test = "t", 
                     method = "REML")

# investigate variance 
i2_wo <- var.comp(full.model_wo)
summary(i2_wo)

# plot a forest (caterpillar) to eyeball heterogeneity and effect sizes
forest(full.model_wo, header=TRUE,
       slab = testdata_wo$ID,
       order="obs", 
       showweights = TRUE, 
       ilab.xpos=-1,  ylim=c(0,113))

# compare pseudo r^2 from null models with and without outliers
r_squared_wo <- (sum(full.model$sigma2) - sum(full.model_wo$sigma2)) / sum(full.model$sigma2)


# generate a model with Conflict Strength as the main predictor
full.model_CSwo <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                        V = Variance, 
                        slab = PaperNo,
                        data = testdata_wo,
                        mods = ~ ConflictStrength,
                        random = ~ 1 | PaperNo/ID, 
                        test = "t", 
                        method = "REML")

# plot a regression model
regplot(full.model_CSwo, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved")

# compare pseudo r^2 from null model and CS model without outliers 
r_squared_CSwo <- (sum(full.model$sigma2) - sum(full.model_CSwo$sigma2)) / sum(full.model$sigma2)


# generate a full model 
full.model_all_wo <- rma.mv(yi = Percentage_IntegrativePotential_achieved, 
                         V = Variance, 
                         slab = PaperNo,
                         data = testdata_wo,
                         random = ~ 1 | PaperNo/ID, 
                         mods = ~ ConflictStrength *
                           Performance.based.incentive +
                           X..Number.of.issues +
                           perc_impasses_total,
                         test = "t", 
                         method = "REML")

# plot a regression model
regplot(full.model_all_wo, label = TRUE, labsize= 0.6, offset= 1, 
        ylab = "Percentage of Integrative Potential achieved", mod = 2)

# compare pseudo r^2 from null model and full model without outliers 
r_squared_all_wo <- (sum(full.model$sigma2) - sum(full.model_all_wo$sigma2)) / sum(full.model$sigma2)

