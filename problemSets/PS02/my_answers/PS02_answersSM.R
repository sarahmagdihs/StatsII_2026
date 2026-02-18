##########################
# Title:        Problem Set 2
# Description:  Applied Stats II
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 18.02.2026
###########################

###############################
#### Set Up:
# load libraries
# set wd
# clear global .envir
###############################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Data Preparation
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

#inspect data
head(climateSupport)
str(climateSupport)
summary(climateSupport)

#check levels 
levels(climateSupport$countries)
levels(climateSupport$sanctions)
#i believe RStudio uses the first category as the reference cat. 
#but in case that's actually not true, this is how I would to do it:

#unorder data
climateSupport$countries <- factor(climateSupport$countries, 
                                   ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions,
                                   ordered = FALSE)
#set reference categories 
climateSupport$countries <- relevel(climateSupport$countries, ref = "20 of 192")
climateSupport$sanctions <- relevel(climateSupport$sanctions, ref = "None")

#check again
levels(climateSupport$countries)
levels(climateSupport$sanctions)


#####################
# Problem 1
#####################

##Fitting an additive model 

model_add <- glm(choice ~ countries + sanctions, 
                 family = binomial(link = logit),
                 data = climateSupport)

summary(model_add)


##Global Null-Hypothesis 
#H0: All slopes = 0
#H1: At least one coefficient is different from zero 

#First: need Null Model (=Intercept only)
model_null <- glm(choice ~ 1, 
                  family = binomial,
                 data = climateSupport)

#anova: null model vs full model 
anova_add <- anova(model_null, model_add, test = "LRT")

##Rejection of H0 (p-value ≪ 0.05), meaning that 
##at least one explanatory variable significantly affects the probability of choice.

#Stargazer for Latex Write-Up
stargazer(model_add, 
          title = "Logistic Regression: Support for An Environmental Policy (International)",
          type = "latex")

stargazer(anova_add, 
          title = "Anova: Global Null Hypothesis Testing",
          summary = FALSE,
          type = "latex")


#####################
# Problem 2
#####################

##2a: Change in odds

#check model coefficients 
coef(model_add)

#I need coefficient for sanctions 5% and 15%
beta_5 <- coef(model_add)["sanctions5%"]
beta_15 <- coef(model_add)["sanctions15%"]

#Calculate Odds Ratio
odds_ratio <- exp(beta_15 - beta_5)
odds_ratio

#because I don't want to do the math myself 
decrease <- 1 - odds_ratio
decrease

##2b is the same answer since its an additive model 

##2c: estimated probabilty for 80 countries + no sanctions
est_prob_80 <- predict(model_add, 
                      newdata = data.frame(countries = "80 of 192", sanctions = "None"), 
                      type = "response")
est_prob_80

#####################
# Problem 3
#####################

#need: interaction model 
model_int <- glm(choice ~ countries * sanctions,
                 data = climateSupport,
                 family= binomial(link = "logit"))

summary(model_int)

#Stargazer output
stargazer(model_int,
          title = "Interaction Model",
          type = "latex")

#anova: additive model vs interaction model 
anova_int <- anova(model_add, model_int, test = "Chisq")
anova_int


#stargazer output for Write-up
stargazer(anova_int, 
          summary = FALSE,
          title = "Anova: Additive vs. Interaction",
          type = "latex")



##extra 
coef(model_add)

oddsratio_20_c <- exp(coef(model_add)["(Intercept)"])
oddsratio_80 <- exp(coef(model_add)["countries80 of 192"])
oddsratio_160 <- exp(coef(model_add)["countries160 of 192"])
 oddsratio_5 <- exp(coef(model_add)["sanctions5%"])
oddsratio_15 <- exp(coef(model_add)["sanctions15%"])
oddsratio_20 <- exp(coef(model_add)["sanctions20%"])

