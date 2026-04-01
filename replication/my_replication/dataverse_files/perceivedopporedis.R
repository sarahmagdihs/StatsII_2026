##Perceptions of Conservative Opposition to Redistribution

#load in wave 7
BES7 <- read.dta("BES2015_W7_v2.2-1.dta")

BES7 <- BES7 %>%
  mutate(redistCon = case_when(redistCon== "Government should try to make incomes equal" ~ 0,
                               redistCon == "1" ~ 1, redistCon== "2" ~ 2,redistCon == "3" ~ 3,redistCon == "4" ~ 4,redistCon == "5" ~ 5,redistCon == "6" ~ 6,
                               redistCon == "7" ~ 7,redistCon == "8" ~ 8,redistCon == "9" ~ 9,redistCon == "Government should be less concerned about equal incomes" ~ 10))
BES7 <- BES7 %>%
  mutate(redistGreen = case_when(redistGreen== "Government should try to make incomes equal" ~ 0,
                                 redistGreen == "1" ~ 1, redistGreen== "2" ~ 2,redistGreen == "3" ~ 3,redistGreen == "4" ~ 4,redistGreen == "5" ~ 5,redistGreen == "6" ~ 6,
                                 redistGreen == "7" ~ 7,redistGreen == "8" ~ 8,redistGreen == "9" ~ 9,redistGreen == "Government should be less concerned about equal incomes" ~ 10))
BES7 <- BES7 %>%
  mutate(redistLab = case_when(redistLab== "Government should try to make incomes equal" ~ 0,
                               redistLab == "1" ~ 1, redistLab== "2" ~ 2,redistLab == "3" ~ 3,redistLab == "4" ~ 4,redistLab == "5" ~ 5,redistLab == "6" ~ 6,
                               redistLab == "7" ~ 7,redistLab == "8" ~ 8,redistLab == "9" ~ 9,redistLab == "Government should be less concerned about equal incomes" ~ 10))
BES7 <- BES7 %>%
  mutate(redistLD = case_when(redistLD== "Government should try to make incomes equal" ~ 0,
                              redistLD == "1" ~ 1, redistLD== "2" ~ 2,redistLD == "3" ~ 3,redistLD == "4" ~ 4,redistLD == "5" ~ 5,redistLD == "6" ~ 6,
                              redistLD == "7" ~ 7,redistLD == "8" ~ 8,redistLD == "9" ~ 9,redistLD == "Government should be less concerned about equal incomes" ~ 10))

BES7 <- BES7 %>%
  mutate(redistPC = case_when(redistPC== "Government should try to make incomes equal" ~ 0,
                              redistPC == "1" ~ 1, redistPC== "2" ~ 2,redistPC == "3" ~ 3,redistPC == "4" ~ 4,redistPC == "5" ~ 5,redistPC == "6" ~ 6,
                              redistPC == "7" ~ 7,redistPC == "8" ~ 8,redistPC == "9" ~ 9,redistPC == "Government should be less concerned about equal incomes" ~ 10))

BES7 <- BES7 %>%
  mutate(redistSNP = case_when(redistSNP== "Government should try to make incomes equal" ~ 0,
                               redistSNP == "1" ~ 1, redistSNP== "2" ~ 2,redistSNP == "3" ~ 3,redistSNP == "4" ~ 4,redistSNP == "5" ~ 5,redistSNP == "6" ~ 6,
                               redistSNP == "7" ~ 7,redistSNP == "8" ~ 8,redistSNP == "9" ~ 9,redistSNP == "Government should be less concerned about equal incomes" ~ 10))

BES7 <- BES7 %>%
  mutate(redistUKIP = case_when(redistUKIP== "Government should try to make incomes equal" ~ 0,
                                redistUKIP == "1" ~ 1, redistUKIP== "2" ~ 2,redistUKIP == "3" ~ 3,redistUKIP == "4" ~ 4,redistUKIP == "5" ~ 5,redistUKIP == "6" ~ 6,
                                redistUKIP == "7" ~ 7,redistUKIP == "8" ~ 8,redistUKIP == "9" ~ 9,redistUKIP == "Government should be less concerned about equal incomes" ~ 10))


#find average perceived opposition to redistribution by party for Wave 7
mean(BES7$redistCon,na.rm=TRUE)
mean(BES7$redistGreen,na.rm=TRUE)
mean(BES7$redistLab,na.rm=TRUE)
mean(BES7$redistLD,na.rm=TRUE)
mean(BES7$redistPC,na.rm=TRUE)
mean(BES7$redistSNP,na.rm=TRUE)
mean(BES7$redistUKIP,na.rm=TRUE)


##load in wave 11

BES11 <- read.dta("BES2015_W11_v1.5.dta")
BES11 <- BES11 %>%
  mutate(redistCon = case_when(redistCon== "Government should try to make incomes equal" ~ 0,
                               redistCon == "1" ~ 1, redistCon== "2" ~ 2,redistCon == "3" ~ 3,redistCon == "4" ~ 4,redistCon == "5" ~ 5,redistCon == "6" ~ 6,
                               redistCon == "7" ~ 7,redistCon == "8" ~ 8,redistCon == "9" ~ 9,redistCon == "Government should be less concerned about equal incomes" ~ 10))
BES11 <- BES11 %>%
  mutate(redistGreen = case_when(redistGreen== "Government should try to make incomes equal" ~ 0,
                                 redistGreen == "1" ~ 1, redistGreen== "2" ~ 2,redistGreen == "3" ~ 3,redistGreen == "4" ~ 4,redistGreen == "5" ~ 5,redistGreen == "6" ~ 6,
                                 redistGreen == "7" ~ 7,redistGreen == "8" ~ 8,redistGreen == "9" ~ 9,redistGreen == "Government should be less concerned about equal incomes" ~ 10))
BES11 <- BES11 %>%
  mutate(redistLab = case_when(redistLab== "Government should try to make incomes equal" ~ 0,
                               redistLab == "1" ~ 1, redistLab== "2" ~ 2,redistLab == "3" ~ 3,redistLab == "4" ~ 4,redistLab == "5" ~ 5,redistLab == "6" ~ 6,
                               redistLab == "7" ~ 7,redistLab == "8" ~ 8,redistLab == "9" ~ 9,redistLab == "Government should be less concerned about equal incomes" ~ 10))
BES11 <- BES11 %>%
  mutate(redistLD = case_when(redistLD== "Government should try to make incomes equal" ~ 0,
                              redistLD == "1" ~ 1, redistLD== "2" ~ 2,redistLD == "3" ~ 3,redistLD == "4" ~ 4,redistLD == "5" ~ 5,redistLD == "6" ~ 6,
                              redistLD == "7" ~ 7,redistLD == "8" ~ 8,redistLD == "9" ~ 9,redistLD == "Government should be less concerned about equal incomes" ~ 10))

BES11 <- BES11 %>%
  mutate(redistPC = case_when(redistPC== "Government should try to make incomes equal" ~ 0,
                              redistPC == "1" ~ 1, redistPC== "2" ~ 2,redistPC == "3" ~ 3,redistPC == "4" ~ 4,redistPC == "5" ~ 5,redistPC == "6" ~ 6,
                              redistPC == "7" ~ 7,redistPC == "8" ~ 8,redistPC == "9" ~ 9,redistPC == "Government should be less concerned about equal incomes" ~ 10))

BES11 <- BES11 %>%
  mutate(redistSNP = case_when(redistSNP== "Government should try to make incomes equal" ~ 0,
                               redistSNP == "1" ~ 1, redistSNP== "2" ~ 2,redistSNP == "3" ~ 3,redistSNP == "4" ~ 4,redistSNP == "5" ~ 5,redistSNP == "6" ~ 6,
                               redistSNP == "7" ~ 7,redistSNP == "8" ~ 8,redistSNP == "9" ~ 9,redistSNP == "Government should be less concerned about equal incomes" ~ 10))

BES11 <- BES11 %>%
  mutate(redistUKIP = case_when(redistUKIP== "Government should try to make incomes equal" ~ 0,
                                redistUKIP == "1" ~ 1, redistUKIP== "2" ~ 2,redistUKIP == "3" ~ 3,redistUKIP == "4" ~ 4,redistUKIP == "5" ~ 5,redistUKIP == "6" ~ 6,
                                redistUKIP == "7" ~ 7,redistUKIP == "8" ~ 8,redistUKIP == "9" ~ 9,redistUKIP == "Government should be less concerned about equal incomes" ~ 10))
#find average perceived opposition to redistribution by party for Wave 11
mean(BES11$redistCon,na.rm=TRUE)
mean(BES11$redistGreen,na.rm=TRUE)
mean(BES11$redistLab,na.rm=TRUE)
mean(BES11$redistLD,na.rm=TRUE)
mean(BES11$redistPC,na.rm=TRUE)
mean(BES11$redistSNP,na.rm=TRUE)
mean(BES11$redistUKIP,na.rm=TRUE)
## load in perception scores (self-constructed dataset based on above code)
percep <- read.csv("perceivedredistribution.csv")
##Make Figure 2
##plot Wave 7 values of opposition to redistribution by party
g7 <- ggplot(percep,aes(y=Perceived.Opposition.to.Redistribution.Wave.7,x=Party))+geom_bar(stat="identity", width=0.5, color="black", fill="blue")+stat_smooth(method="lm",se=FALSE) +theme_bw() +xlab("Party") + ylab("Perceived Opposition to Redistribution (Wave 7)") 
g7



##plot Wave 11 values of opposition to redistribution by party
g11 <- ggplot(percep,aes(y=Perceived.Opposition.to.Redistribution.Wave.11,x=Party))+geom_bar(stat="identity", width=0.5, color="black", fill="blue")+stat_smooth(method="lm",se=FALSE) +theme_bw() +xlab("Party") + ylab("Perceived Opposition to Redistribution (Wave 11)") 
g11

