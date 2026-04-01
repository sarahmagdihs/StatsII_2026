#redistributive attitudes

# load in waves 8 and 9
BES8 <- read.dta("BES2015_W8_v2.3.dta")
BES9 <- read.dta("BES2015_W9_v1.7.dta")

# make race variable
BES8$white <- ifelse(BES8$profile_ethnicity=="White British" |BES8$profile_ethnicity=="Any other white background",1,0)
library(dplyr)




BES8$partyId8 <- BES8$partyId
BES9$partyId9 <- BES9$partyId




BES8notcons<- BES8[BES8$partyId!="Conservative",] 
merge <- merge(BES8notcons, BES9,by="id")


merge$switchtocons <- ifelse(merge$partyId9=="Conservative",1,0)




#merge with BES7
BES7 <- read.dta("BES2015_W7_v2.2-1.dta")
BES7 <- BES7 %>%
  mutate(redistSelf = case_when(redistSelf== "Government should try to make incomes equal" ~ 0,
                                redistSelf == "1" ~ 1, redistSelf== "2" ~ 2,redistSelf == "3" ~ 3,redistSelf == "4" ~ 4,redistSelf == "5" ~ 5,redistSelf == "6" ~ 6,
                                redistSelf == "7" ~ 7,redistSelf == "8" ~ 8,redistSelf == "9" ~ 9,redistSelf == "Government should be less concerned about equal incomes" ~ 10))



BES7$redistSelf7 <- BES7$redistSelf


merge <- merge(merge, BES7, by="id")

#load in BES 11

BES11 <- read.dta("BES2015_W11_v1.5.dta")

BES11 <- BES11 %>%
  mutate(redistSelf = case_when(redistSelf== "Government should try to make incomes equal" ~ 0,
                                redistSelf == "1" ~ 1, redistSelf== "2" ~ 2,redistSelf == "3" ~ 3,redistSelf == "4" ~ 4,redistSelf == "5" ~ 5,redistSelf == "6" ~ 6,
                                redistSelf == "7" ~ 7,redistSelf == "8" ~ 8,redistSelf == "9" ~ 9,redistSelf == "Government should be less concerned about equal incomes" ~ 10))



BES11$redistSelf11 <- BES11$redistSelf

merge <- merge(merge, BES11, by="id")






#Regressions for Table 4

merge$redistchange <- merge$redistSelf11- merge$redistSelf7
Reg <- lm(redistchange~switchtocons+ white + age.x +gender.x+ as.factor(country.x), data=merge)



summary(Reg)



#people who were not part of UKIP

RegUKIP <- lm(redistchange~switchtocons+ white + age.x +gender.x+ as.factor(country.x), data=merge, partyId8=="United Kingdom Independence Party (UKIP)")



summary(RegUKIP)


RegnonUKIP <- lm(redistchange~switchtocons+ white + age.x +gender.x+ as.factor(country.x), data=merge, partyId8!="United Kingdom Independence Party (UKIP)")



summary(RegnonUKIP)

#Table 4

stargazer(Reg,RegnonUKIP,RegUKIP, title="Joining the Conservatives and Opposition to Redistribution",no.space=TRUE, star.cutoffs = c(0.05, 0.01,0.001))
