##########################
# Title:        Problem Set 3
# Description:  Applied Stats II
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 23.03.2026
###########################


#####################
#### Set Up:
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c("nnet", "MASS", "stargazer", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

head(gdp_data)
str(gdp_data)

#Recoding of Variables 
#Response: GDPWdiff


gdp_data$GDPWdiff_categ <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                                  ifelse(gdp_data$GDPWdiff < 0, "negative", "no change"))

gdp_data$GDPWdiff_categ <- factor(gdp_data$GDPWdiff_categ,
                                  levels = c("no change", "negative", "positive"))
#no change as first level because then it is treated as the reference category 


#Explanatory: REG, OIL
#cpnvert to factor 
gdp_data$REG_f <- factor(gdp_data$REG, levels = c(0, 1),
                       labels = c("Non-Democracy", "Democracy"))
gdp_data$OIL_f <- factor(gdp_data$OIL, levels = c(0, 1),
                       labels = c("Fuel exports below 50%", "Fuel exports over 50%"))
#converting them to factors is not really necessary. 
#This is mainly for ease concerning interpretation at a later stage/as a reminder for me 

str(gdp_data)

# TASK 1: unordered multinomial logit 
#GDPWdiff as the output and ”no change” as the reference category

multinom_unordered <- multinom(GDPWdiff_categ ~ REG_f + OIL_f, data = gdp_data)
summary(multinom_unordered)


#exponentiate coefs for odds/interpretation
odds_unordered <- exp(coef(multinom_unordered))

# p values
z <- summary(multinom_unordered)$coefficients/summary(multinom_unordered)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

####save output
stargazer(multinom_unordered)

#TASK 2: ordered multinomial logit 
# Need ordered factor: negative --> no change --> positive
gdp_data$GDPWdiff_ordered <- ordered(gdp_data$GDPWdiff_categ,
                                 levels = c("negative", "no change", "positive"))

multinom_ordered <- polr(GDPWdiff_ordered ~ REG_f + OIL_f, data = gdp_data, Hess = TRUE)
summary(multinom_ordered)

#p value
ctable <- coef(summary(multinom_ordered))
p_ordered <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
cbind(ctable, "p value" = round(p_ordered, 3))


#confidence intervals
ci_ordered <- confint(multinom_ordered)

# convert to odds ratio
exp(cbind(OR = coef(multinom_ordered), ci_ordered))

####save output 
stargazer(multinom_ordered)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

#check out the data
str(mexico_elections)
summary(mexico_elections$PAN.visits.06)
table(mexico_elections$PAN.visits.06)
var(mexico_elections$PAN.visits.06)

#TASK 1: Poisson regression 
poisson_reg <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                   data = mexico_elections, 
                   family = poisson(link = "log"))
summary(poisson_reg)

stargazer(poisson_reg)

#p and z for competitive/swing districts
coef_swing <- summary(poisson_reg)$coefficients["competitive.district", ]

coef_swing["z value"]
coef_swing["Pr(>|z|)"]

#exponentiate coefs for interpretation (TASK 2)
poisson_expcoef <- exp(coef(poisson_reg))

#TASK 3: 
#specific case: estimated mean number of visits from the winning PAN presidential candidate 
#for a hypothetical district that was competitive (competitive.district=1), had
#an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1)

predict_case <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1
)

predict(poisson_reg, newdata = predict_case, type = "response")







