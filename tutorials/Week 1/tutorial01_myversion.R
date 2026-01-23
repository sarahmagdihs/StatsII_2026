##################
#### Stats II ####
##################

###############################
#### Tutorial 1: Refresher ####
###############################

##check working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##in whatever folder i have 
getwd()
# Today's tutorial is a refesher of the R skills we learnt in the first semester.
#     1. Importing data
#     2. Wrangling data
#     3. Analysing data
#     4. Communicating

#### Case study
# A fictional think-tank, the Alliance of Wealthy People who Dislike Tax, has asked
# you to study the relationship between tax, regulation and GDP per capita. They believe
# that countries with low tax and light regulation are wealthier, and they want you to 
# prove it using statistics!

# We're going to use variables such as: 
# Ease of doing business rank (1=most business-friendly regulations): IC.BUS.EASE.XQ
# GDP per capita (current US$): NY.GDP.PCAP.CD
# "tax revenue (% of GDP)": GC.TAX.TOTL.GD.ZS

#### Importing the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in our libraries.

library(tidyverse) 
library(stargazer)

## loading the data
data <- read_csv("tutorial01_data.csv",
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations)` = col_double(),
                   `Tax revenue (% of GDP)` = col_double(),
                   `GDP per capita (current US$)` = col_double()
                 )) ##remember file names in quotation marks; change format of certain variables/colums 

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 

# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 
view(data)
#for our analysis, we need data to be in numerical format 
str(data) #structure of the data 
head(data) #can use to copy and paste the variable names for code in line 37-42
ls(data) #list your dataset/object
summary(data) #summary statistics for numeric variables; only works on numeric 

# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.

data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`)) 
#selects variables, and by adding the minus in front of it, I delete the variables from the dataset
#starts_with deletes all variables that start with the word "Time"
ls(data) #list your dataset/object to check 

# 3. Let's also get rid of the variable code in square brackets
#hint: try using the function sub() with the regexp " \\[.*"
names(data) <- sub("\\[.*","", names(data)) ##not needed here 
  
#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.
pdf("name.pdf") ## can add width = 12, height = 7
ggplot(data=data, aes(x=`Tax revenue (% of GDP)`, y= `GDP per capita (current US$)`)) +
  geom_point() +
    geom_smooth(method = "lm", color="blue") 
dev.off()
##do not need to add the x= and y= but for clarity i feel like it helps 
#OR
data %>%
ggplot(aes(`Tax revenue (% of GDP)`, `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm", color="maroon")

# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.
data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`,`GDP per capita (current US$)` )) +
  geom_point() +
  geom_smooth(method = "lm", color="red")


# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.
data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`,`Tax revenue (% of GDP)` )) +
  geom_point() +
  geom_smooth(method = "lm", color="darkgreen")


# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?

#how is GDP affected by Tax revenue and ease of doing business?

# 5. Now let's run a regression!

formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`

reg1 <- lm(formula, data) ##good to know, could write the formula as an extra object and then just do this
summary(reg1)

# How do we interpret these results?
#Interpretation:
#Intercept: When Rank is lowest and Tax revenue is =0, then the expected GDP value is  9256.4
#beta 1: For every one-unit increase in tax-revenue is associated with an 1458.2 Dollar increase in GDP, holding everything else constant. It is significant at significance level alpha = 0.01 
#beta 2: A one-unit increase in ease of doing business is associated with a -223 Dollar decrease in GDP, holding all else constant. However, that relationship is not statistically significant 
#R^2 is 0.13 --> how much variance expalins 
#F-Statistic: It is a non-zero relationship, so there is some sort of relationship between (at least one) IV and DV???
# in other words: There is sufficient evidence to conclude that at least one of your independent variables (or groups) has a non-zero effect on the dependent variable

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  
###look at this in the solution file later -- we didnt have time to finish the code for this regression?

  # 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.
stargazer(reg1)

# Now all we need is to update the latex template and upload the pdf to github!

###how to plot a graph 
pdf("name.pdf") ## can add width = 12, height = 7
#run graph
dev.off()

