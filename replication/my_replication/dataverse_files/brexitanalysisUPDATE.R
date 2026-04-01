##load in British Election Study datasets``

library(foreign)
library(stargazer)
BES6 <-  read.dta("BES2015_W6_v3.9.dta")
BES8 <- read.dta("BES2015_W8_v2.3.dta")
BES9 <- read.dta("BES2015_W9_v1.7.dta")
BES13 <- read.dta("BES2017_W13_v1.5.dta")
library(dplyr)

##recode EU integration values for Waves 8 and 9 

BES8 <- BES8 %>%
  mutate(EUIntegrationCon = case_when(EUIntegrationCon == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationCon == "1" ~ 1, EUIntegrationCon == "2" ~ 2,EUIntegrationCon == "3" ~ 3,EUIntegrationCon == "4" ~ 4,EUIntegrationCon == "5" ~ 5,EUIntegrationCon == "6" ~ 6,
                                      EUIntegrationCon == "7" ~ 7,EUIntegrationCon == "8" ~ 8,EUIntegrationCon == "9" ~ 9,EUIntegrationCon == "Protect our independence" ~ 10))
BES8 <- BES8 %>%
  mutate(EUIntegrationLab = case_when(EUIntegrationLab == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationLab == "1" ~ 1, EUIntegrationLab == "2" ~ 2,EUIntegrationLab == "3" ~ 3,EUIntegrationLab == "4" ~ 4,EUIntegrationLab == "5" ~ 5,EUIntegrationLab == "6" ~ 6,
                                      EUIntegrationLab == "7" ~ 7,EUIntegrationLab == "8" ~ 8,EUIntegrationLab == "9" ~ 9,EUIntegrationLab == "Protect our independence" ~ 10))


BES8 <- BES8 %>%
  mutate(EUIntegrationSelf = case_when(EUIntegrationSelf == "Unite fully with the European Union" ~ 0,
                                       EUIntegrationSelf == "1" ~ 1, EUIntegrationSelf == "2" ~ 2,EUIntegrationSelf == "3" ~ 3,EUIntegrationSelf == "4" ~ 4,EUIntegrationSelf == "5" ~ 5,EUIntegrationSelf == "6" ~ 6,
                                       EUIntegrationSelf == "7" ~ 7,EUIntegrationSelf == "8" ~ 8,EUIntegrationSelf == "9" ~ 9,EUIntegrationSelf == "Protect our independence" ~ 10))








BES9 <- BES9 %>%
  mutate(EUIntegrationCon = case_when(EUIntegrationCon == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationCon == "1" ~ 1, EUIntegrationCon == "2" ~ 2,EUIntegrationCon == "3" ~ 3,EUIntegrationCon == "4" ~ 4,EUIntegrationCon == "5" ~ 5,EUIntegrationCon == "6" ~ 6,
                                      EUIntegrationCon == "7" ~ 7, EUIntegrationCon == "8" ~ 8,EUIntegrationCon == "9" ~ 9,EUIntegrationCon == "Protect our independence" ~ 10))

BES9 <- BES9 %>%
  mutate(EUIntegrationLab = case_when(EUIntegrationLab == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationLab == "1" ~ 1, EUIntegrationLab == "2" ~ 2,EUIntegrationLab == "3" ~ 3,EUIntegrationLab == "4" ~ 4,EUIntegrationLab == "5" ~ 5,EUIntegrationLab == "6" ~ 6,
                                      EUIntegrationLab == "7" ~ 7,EUIntegrationLab == "8" ~ 8,EUIntegrationLab == "9" ~ 9,EUIntegrationLab == "Protect our independence" ~ 10))


BES9 <- BES9 %>%
  mutate(EUIntegrationSelf = case_when(EUIntegrationSelf == "Unite fully with the European Union" ~ 0,
                                       EUIntegrationSelf == "1" ~ 1, EUIntegrationSelf == "2" ~ 2,EUIntegrationSelf == "3" ~ 3,EUIntegrationSelf == "4" ~ 4,EUIntegrationSelf == "5" ~ 5,EUIntegrationSelf == "6" ~ 6,
                                       EUIntegrationSelf == "7" ~ 7,EUIntegrationSelf == "8" ~ 8,EUIntegrationSelf == "9" ~ 9,EUIntegrationSelf == "Protect our independence" ~ 10))

##Examine perceived Euroskepticism of Conservatives and Labour in Waves 8 and 9
mean(BES8$EUIntegrationCon,na.rm=TRUE)
mean(BES9$EUIntegrationCon,na.rm=TRUE)
mean(BES8$EUIntegrationLab,na.rm=TRUE)
mean(BES9$EUIntegrationLab,na.rm=TRUE)



#create race variable
BES8$white <- ifelse(BES8$profile_ethnicity=="White British" |BES8$profile_ethnicity=="Any other white background",1,0)
BES8$EUIntegrationCon8 <- BES8$EUIntegrationCon
BES8$partyId8 <- BES8$partyId
BES8$EUIntegrationSelf8 <- BES8$EUIntegrationSelf 
##partyid-conservative
BES8$Con <- ifelse(BES8$partyId=="Conservative",1,0)
BES8$Con8 <- BES8$Con 
BES9$Con <- ifelse(BES9$partyId=="Conservative",1,0)
BES9$Con9 <- BES9$Con 
BES9$EUIntegrationCon9 <- BES9$EUIntegrationCon


BES9$EUIntegrationSelf9 <- BES9$EUIntegrationSelf 
BES9$partyId9 <- BES9$partyId



##subset Wave 8 to conservatives
BES8subcons<- BES8[BES8$partyId=="Conservative",] 

##party switchers
merge <- merge(BES8subcons, BES9,by="id")

merge$partyswitcher  <- ifelse(merge$partyId8!=merge$partyId9,1,0)
# Table 1
Reg1 <- lm(merge$partyswitcher~merge$EUIntegrationSelf8)
summary(Reg1)
merge$Conchange <- merge$EUIntegrationCon9 - merge$EUIntegrationCon8
intreg1<- lm(merge$partyswitcher~merge$EUIntegrationSelf8 * merge$Conchange)
summary(intreg1)

intreg1a <- lm(merge$partyswitcher~merge$EUIntegrationSelf8 * merge$Conchange + merge$age.x + merge$gender.x + merge$white + as.factor(merge$country.x))
summary(intreg1a)

stargazer(Reg1, intreg1, intreg1a, title="Euroskepticism and Defection from the Conservatives",no.space=TRUE,star.cutoffs = c(0.05, 0.01,0.001)) 
#percent switch
merge45 <- merge[merge$EUIntegrationSelf8=="5" | merge$EUIntegrationSelf8=="4",]
merge23 <- merge[merge$EUIntegrationSelf8=="3" | merge$EUIntegrationSelf8=="2",]
merge01 <- merge[merge$EUIntegrationSelf8=="1" | merge$EUIntegrationSelf8=="0",]
mean(merge45$partyswitcher,na.rm=TRUE)
mean(merge23$partyswitcher,na.rm=TRUE)
mean(merge01$partyswitcher,na.rm=TRUE)


##subset to non-conservatives
BES8notcons<- BES8[BES8$partyId!="Conservative",] 


merge2 <- merge(BES8notcons, BES9,by="id")


merge2$switchtocons <- ifelse(merge2$partyId9=="Conservative",1,0)
mean(merge2$switchtocons)
Reg2 <- lm(merge2$switchtocons~  merge2$EUIntegrationSelf8)
summary(Reg2)
merge2$Conchange <- merge2$EUIntegrationCon9 - merge2$EUIntegrationCon8
intreg2 <- lm(merge2$switchtocons~merge2$EUIntegrationSelf8*merge2$Conchange)
summary(intreg2)
intreg2a <- lm(merge2$switchtocons~merge2$EUIntegrationSelf8*merge2$Conchange+ merge2$age.x + merge2$gender.x + merge2$white + as.factor(merge2$country.x))
summary(intreg2a)

# Table 2
stargazer(Reg2, intreg2, intreg2a, title="Euroskepticism and Joining the Conservatives",no.space=TRUE,star.cutoffs = c(0.05, 0.01,0.001)) 


#percent switched
merge2910 <- merge2[merge2$EUIntegrationSelf8=="9" | merge2$EUIntegrationSelf8=="10",]
merge278 <- merge2[merge2$EUIntegrationSelf8=="7" | merge2$EUIntegrationSelf8=="8",]
merge256 <- merge2[merge2$EUIntegrationSelf8=="5" | merge2$EUIntegrationSelf8=="6",]
mean(merge2910$switchtocons,na.rm=TRUE)
mean(merge278$switchtocons,na.rm=TRUE)
mean(merge256$switchtocons,na.rm=TRUE)




# Table 7 (appendix)
intreg2Lab <- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="Labour")
summary(intreg2Lab)
intreg2LD <- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="Liberal Democrat")
summary(intreg2LD)
intreg2UKIP<- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="United Kingdom Independence Party (UKIP)")
summary(intreg2UKIP)
intreg2Green<- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="Green Party")
summary(intreg2Green)
intreg2SNP<- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="Scottish National Party (SNP)")
summary(intreg2SNP)
intreg2none<- lm(switchtocons~EUIntegrationSelf8*Conchange+ age.x + gender.x + white + as.factor(country.x), data=merge2,partyId8=="No - none")
summary(intreg2none)

stargazer(intreg2Lab,intreg2LD,intreg2UKIP,intreg2Green,intreg2SNP, intreg2none,title="Euroskepticism and Joining the Conservatives (by Pre-Referendum Partisan Affiliation)",no.space=TRUE,star.cutoffs = c(0.05, 0.01,0.001))


##look at whether Conservative partisans shift policy views
merge$Euroskepticismchange <- merge$EUIntegrationSelf9 - merge$EUIntegrationSelf8

mergestrong <- merge[merge$partyIdStrength.x=="Very strong",]
mergenotstrong <- merge[merge$partyIdStrength.x!="Very strong",]


   
#Table 3

intreg3B <- lm(Euroskepticismchange~Conchange+gender.x + age.x +white+ as.factor(country.x),data=mergestrong)
summary(intreg3B) 
intreg3A <- lm(Euroskepticismchange~Conchange+gender.x + age.x +white+ as.factor(country.x),data=mergenotstrong)
summary(intreg3A) 




stargazer(intreg3A,intreg3B, title="Individual Shifts in Euroskepticism",no.space=TRUE,star.cutoffs = c(0.05, 0.01,0.001))

#merge waves 8 and 9
bigmerge <- merge(BES8,BES9,by="id")
bigmerge$Euroskepticismchange <- bigmerge$EUIntegrationSelf9 - bigmerge$EUIntegrationSelf8
bigmerge$Conchange <- bigmerge$EUIntegrationCon9 - bigmerge$EUIntegrationCon8

bigmerge$Partisan[bigmerge$partyId8!="Conservative"] <- "Non-Conservative"

bigmerge$Partisan[bigmerge$partyId8=="Conservative" & bigmerge$partyIdStrength.x != "Very strong"] <- "Moderate Conservative"
bigmerge$Partisan[bigmerge$partyId8=="Conservative" & bigmerge$partyIdStrength.x == "Very strong"] <- "Very Strong Conservative"



bigmergenon <- bigmerge[bigmerge$Partisan=="Non-Conservative",]
bigmergemod<- bigmerge[bigmerge$Partisan=="Moderate Conservative",]
bigmergever <- bigmerge[bigmerge$Partisan=="Very Strong Conservative",]
mean(bigmergenon$Conchange,na.rm=TRUE)
mean(bigmergemod$Conchange,na.rm=TRUE)
mean(bigmergever$Conchange,na.rm=TRUE)
mean(bigmergenon$Euroskepticismchang,na.rm=TRUE)
mean(bigmergemod$Euroskepticismchang,na.rm=TRUE)
mean(bigmergever$Euroskepticismchang,na.rm=TRUE)
bigmerge$Partisan <- factor(bigmerge$Partisan, levels = c("Non-Conservative", "Moderate Conservative", "Very Strong Conservative"))
library(ggplot2)
require(ggplot2)
  #make figure 1
g <- ggplot(bigmerge,aes(y=Euroskepticismchange,x=Conchange, color=Partisan))+geom_point(size = 0.1)+stat_smooth(method="lm",se=TRUE) +theme_bw() +xlab("Perceived Change in Conservative Euroskepticism") + ylab("Change in Personal Euroskepticism") 



g


### Wave 6 (2015 post-election) and Wave 13 (2017 post-election) 
#make race variable
BES6$white <- ifelse(BES6$profile_ethnicity=="White British" |BES6$profile_ethnicity=="Any other white background",1,0)
#recode EU integration
BES6 <- BES6 %>%
  mutate(EUIntegrationCon = case_when(EUIntegrationCon == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationCon == "1" ~ 1, EUIntegrationCon == "2" ~ 2,EUIntegrationCon == "3" ~ 3,EUIntegrationCon == "4" ~ 4,EUIntegrationCon == "5" ~ 5,EUIntegrationCon == "6" ~ 6,
                                      EUIntegrationCon == "7" ~ 7,EUIntegrationCon == "8" ~ 8,EUIntegrationCon == "9" ~ 9,EUIntegrationCon == "Protect our independence" ~ 10))
BES6 <- BES6 %>%
  mutate(EUIntegrationLab = case_when(EUIntegrationLab == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationLab == "1" ~ 1, EUIntegrationLab == "2" ~ 2,EUIntegrationLab == "3" ~ 3,EUIntegrationLab == "4" ~ 4,EUIntegrationLab == "5" ~ 5,EUIntegrationLab == "6" ~ 6,
                                      EUIntegrationLab == "7" ~ 7,EUIntegrationLab == "8" ~ 8,EUIntegrationLab == "9" ~ 9,EUIntegrationLab == "Protect our independence" ~ 10))



BES6$generalElectionVote15 <- BES6$generalElectionVote

BES6$EUIntegrationLab15 <- BES6$EUIntegrationLab
BES6$EUIntegrationCon15 <- BES6$EUIntegrationCon



##
BES6 <- BES6 %>%
  mutate(EUIntegrationSelf = case_when(EUIntegrationSelf == "Unite fully with the European Union" ~ 0,
                                       EUIntegrationSelf == "1" ~ 1, EUIntegrationSelf == "2" ~ 2,EUIntegrationSelf == "3" ~ 3,EUIntegrationSelf == "4" ~ 4,EUIntegrationSelf == "5" ~ 5,EUIntegrationSelf == "6" ~ 6,
                                       EUIntegrationSelf == "7" ~ 7,EUIntegrationSelf == "8" ~ 8,EUIntegrationSelf == "9" ~ 9,EUIntegrationSelf == "Protect our independence" ~ 10))

BES6$EUIntegrationSelf6 <- BES6$EUIntegrationSelf



#conservative vote variable
BES6$Convote <- ifelse(BES6$generalElectionVote15=="Conservative",1,0)


##conservative subset
BES6con <- BES6[BES6$Convote==1,]



##non conservative subset
BES6notcon <- BES6[BES6$Convote==0,]


## vote choice variable
BES13$generalElectionVote17 <- BES13$generalElectionVote
#recode integration values
BES13 <- BES13 %>%
  mutate(EUIntegrationSelf = case_when(EUIntegrationSelf == "Unite fully with the European Union" ~ 0,
                                       EUIntegrationSelf == "1" ~ 1, EUIntegrationSelf == "2" ~ 2,EUIntegrationSelf == "3" ~ 3,EUIntegrationSelf == "4" ~ 4,EUIntegrationSelf == "5" ~ 5,EUIntegrationSelf == "6" ~ 6,
                                       EUIntegrationSelf == "7" ~ 7,EUIntegrationSelf == "8" ~ 8,EUIntegrationSelf == "9" ~ 9,EUIntegrationSelf == "Protect our independence" ~ 10))

BES13$EUIntegrationSelf13 <- BES13$EUIntegrationSelf

BES13 <- BES13 %>%
  mutate(EUIntegrationCon = case_when(EUIntegrationCon == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationCon == "1" ~ 1, EUIntegrationCon == "2" ~ 2,EUIntegrationCon == "3" ~ 3,EUIntegrationCon == "4" ~ 4,EUIntegrationCon == "5" ~ 5,EUIntegrationCon == "6" ~ 6,
                                      EUIntegrationCon == "7" ~ 7,EUIntegrationCon == "8" ~ 8,EUIntegrationCon == "9" ~ 9,EUIntegrationCon == "Protect our independence" ~ 10))

BES13 <- BES13 %>%
  mutate(EUIntegrationLab = case_when(EUIntegrationLab == "Unite fully with the European Union" ~ 0,
                                      EUIntegrationLab == "1" ~ 1, EUIntegrationLab == "2" ~ 2,EUIntegrationLab == "3" ~ 3,EUIntegrationLab == "4" ~ 4,EUIntegrationLab == "5" ~ 5,EUIntegrationLab == "6" ~ 6,
                                      EUIntegrationLab == "7" ~ 7,EUIntegrationLab == "8" ~ 8,EUIntegrationLab == "9" ~ 9,EUIntegrationLab == "Protect our independence" ~ 10))



BES13$generalElectionVote17 <- BES13$generalElectionVote

BES13$EUIntegrationLab17 <- BES13$EUIntegrationLab
BES13$EUIntegrationCon17 <- BES13$EUIntegrationCon


##people who voted Con in 2015
BES13con <- merge(BES6con,BES13, by="id")
BES13con$Conchange <- BES13con$EUIntegrationCon17 - BES13con$EUIntegrationCon15

BES13con$voteswitcher  <- ifelse(BES13con$generalElectionVote17!=BES13con$generalElectionVote15,1,0)
# Table 5
GG <- lm(voteswitcher~EUIntegrationSelf6, data=BES13con)
summary(GG)
HH <- lm(voteswitcher~EUIntegrationSelf6*Conchange, data=BES13con)

II <- lm(voteswitcher~EUIntegrationSelf6*Conchange + age + gender.x + white+as.factor(country.x), data=BES13con)
stargazer(GG,HH,II, title="Euroskepticism and Defecting from Conservatives",no.space=TRUE,star.cutoffs = c(0.05, 0.01,0.001))
##people who voted not Con in 2015
BES13notcon <- merge(BES6notcon,BES13, by="id")
BES13notcon$Conchange <- BES13notcon$EUIntegrationCon17 - BES13notcon$EUIntegrationCon15
BES13notcon$switchtocon <- ifelse(BES13notcon$generalElectionVote17=="Conservative",1,0)
# Table 6
G <- lm(switchtocon~EUIntegrationSelf6, data=BES13notcon)
H <- lm(switchtocon~EUIntegrationSelf6*Conchange, data=BES13notcon)
I <-  lm(switchtocon~EUIntegrationSelf6*Conchange + age + gender.x + white+as.factor(country.x), data=BES13notcon)

stargazer(G,H,I, title="Euroskepticism and Switching Vote to Conservatives",no.space=TRUE, star.cutoffs = c(0.05, 0.01,0.001))

