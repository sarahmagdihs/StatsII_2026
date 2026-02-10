##########################
# Title:        Problem Set 1
# Description:  Applied Stats II
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 10.02.2026
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
# Problem 1
#####################

### KS Test

##Step 1: Hypotheses
#H_0: The sample follows a specified distribution. 
#H_1: The sample does not follow the specified distribution.

###Step 2: Selection of a Reference Distribution
#Here: Normal Distribution

##Step 3: Calculation of the Test Statistic (D)
# It is a one-sample Kolmogorov-Smirnov test 
# since there is only one empirical distribution

#Always the same numbers
set.seed(123)

#create data
data <- rcauchy(1000, location = 0, scale = 1)

####Option 1: Step-by-Step to undestand what I'm doing 
x <- sort(data) #need ordered values 
n <- length(x)

# HINT from assignment: create empirical distribution of observed data
ECDF <- ecdf(x)
empiricalCDF <- ECDF(x)

# theoretical distribution
theoreticalCDF <- pnorm(x)

# generate test statistic // check both ways 
D_pnorm<- max(abs(empiricalCDF - pnorm(x)))

D_theoretical <- max(abs(empiricalCDF - theoreticalCDF))


####Option 2: Assignment asks for a FUNCTION

ks_test <- function(data) {
  data_sorted <- sort(data) ##still need sorted data
  ECDF_func <- ecdf(data_sorted) ##create empirical distribution of data 
  empiricalCDF_func <- ECDF_func(data_sorted)
  theoreticalCDF_func <- pnorm(data_sorted) # theoretical distribution (normal)
  D_func <- max(abs(empiricalCDF_func - theoreticalCDF_func)) ##KS test stat
  return(D_func)
}

D_stat <- ks_test(data)
print(D_stat)

#according to the HINT the 1/n stuff is not necessary in this case 
#since its a one-sample test

##Step 4 (and 5): Determine P-value and Interpretation
#If the p-value is less than the significance level (0.05): H_0 is rejected
#If rejected: suggests that the sample distribution doesn't match the specified distribution

ks_pvalue <- function(D_stat, terms = 1000) { ##approx. infinity with 1000 
  k <- 1:terms ### take the first X terms 
  sum_calc <- sum(exp(-((2*k - 1)^2 * pi^2) / (8 * D_stat^2)))
  p <- (sqrt(2 * pi) / D_stat) * sum_calc
  return(p)
}

p_value <- ks_pvalue(D_stat)
print(p_value)
if (p_value < 0.05) {
  print("Reject H0: Evidence suggests that data does not follow a normal distribution")
} else {
  print("Fail to reject H0")
}


# check 
ks_check <- ks.test(data, "pnorm", mean = 0, sd = 1)

print(D_stat)
print(ks_check$statistic)

print(p_value)
print(ks_check$p.value)

#difference: ks.test apparently does not use the raw KS but a scaled version?

#####################
# Problem 2
#####################

set.seed (123)
data_2 <- data.frame(x = runif(200, 1, 10))
data_2$y <- 0 + 2.75*data_2$x + rnorm(200, 0, 1.5)


#plot data
pdf("plot.pdf")
ggplot(data=data_2, aes(x=x, y= y)) +
  geom_point() +
  geom_smooth(method = "lm", color="blue") + 
  labs(title = "Scatter Plot with Regression Line", 
       x = "Input (x)",                        
       y = "Outcome (y)") 
dev.off()

#Linear Model using lm (reference)
lm_model <- lm(y~x, data=data_2)
summary(lm_model)
print(lm_model)


#Estimate an OLS regression by hand; optimisation with BFGS
lin_likelihood <- function(y, X, parameter) {
  n      <- nrow(X)                   # number of observations
  k      <- ncol(X)                   # number of coefficients (including intercept)
  beta   <- parameter[1:k]            # first of k parameters is beta
  sigma2 <- parameter[k+1]^2          # last parameter is sigma (variance squared)
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)                       # return negative log-likelihood because optim() minimizes
}

results_mle_calc <- optim(fn=lin_likelihood, y=data_2$y, X=cbind(1, data_2$x), par=c(0,1,1), hessian=TRUE, method="BFGS")
print(results_mle_calc$par)

coef(lm_model)

# check with glm function
glm_model <- glm(y~x, data=data_2, family = "gaussian")
print(glm_model)

#final check
coef(lm_model)
print(results_mle_calc$par)
coef(glm_model)

#printing 
lm_coef <- coef(lm_model)       
glm_coef <- coef(glm_model)     
mle_coef <- results_mle_calc$par[1:2]  


stargazer(lm_coef, glm_coef, mle_coef,
          title = "Comparison of Intercept and Slope Estimates",
          column.labels = c("LM", "GLM", "MLE (method=BFGS)"),
          single.row = TRUE)

#manually combined in TexStudio because at this point I simply do not want to deal with this anymore. 


