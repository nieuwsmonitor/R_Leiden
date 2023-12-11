library(readxl)

mydata = read_excel("C:\\Users\\Aletta Meinsma\\surfdrive2\\Documents\\Research\\Project 3\\Data\\NLOZ23-05_Aletta_back up.xlsx")

#Clean up the data
mydata <- subset(mydata,Duration__in_seconds_ > 90)
mydata[is.na(mydata)] <- 0

#raw data descriptives
median(mydata$Duration__in_seconds_)
table(mydata$sex_rec)
mean(mydata$age_cont)
sd(mydata$age_cont)


Spooky = mydata$FL_32_DO_Enigmaticframe + mydata$FL_32_DO_enigmaticframeexpl + mydata$FL_32_DO_enigmaticbenefit + mydata$FL_32_DO_enigmaticrisk + mydata$FL_32_DO_enigmaticbalanced + mydata$FL_32_DO_enigmaticexplbenef + mydata$FL_32_DO_Enigmaticexplrisk + mydata$FL_32_DO_Enigmaticexplanationbalanced
Explanation = mydata$FL_32_DO_Explquantumconcept + mydata$FL_32_DO_enigmaticframeexpl + mydata$FL_32_DO_explanationbenefit + mydata$FL_32_DO_explanationrisk + mydata$FL_32_DO_explanationbalanced + mydata$FL_32_DO_enigmaticexplbenef + mydata$FL_32_DO_Enigmaticexplrisk + mydata$FL_32_DO_Enigmaticexplanationbalanced
Benefit = mydata$FL_32_DO_benefitframe + mydata$FL_32_DO_enigmaticbenefit + mydata$FL_32_DO_explanationbenefit + mydata$FL_32_DO_enigmaticexplbenef
Risk = mydata$FL_32_DO_riskframe + mydata$FL_32_DO_enigmaticrisk + mydata$FL_32_DO_explanationrisk + mydata$FL_32_DO_Enigmaticexplrisk
Both = mydata$FL_32_DO_balancedframe + mydata$FL_32_DO_enigmaticbalanced + mydata$FL_32_DO_explanationbalanced + mydata$FL_32_DO_Enigmaticexplanationbalanced


#prepare the data (sum scores of all variables, with general interest 5 reverse coded)
Info_sum = mydata$Info.seeking_1 + mydata$Info.seeking_2 + mydata$Info.seeking_3
IE_sum = mydata$internal.efficacy_1 + mydata$internal.efficacy_2 + mydata$internal.efficacy_3 + mydata$internal.efficacy_4
GI_sum = mydata$general.interest_1 + mydata$general.interest_2 + mydata$general.interest_3 + mydata$general.interest_4 + (6 - mydata$general.interest_5) + mydata$general.interest_6
PK_sum = mydata$perceived.knowledge_1 + mydata$perceived.knowledge_2 + mydata$perceived.knowledge_3 + mydata$perceived.knowledge_4


Weights = mydata$weight_trimmed_0.980

mynicedata = data.frame(Weights = Weights, Info_sum, IE_sum, GI_sum, PK_sum, Spooky, Risk, Explanation, Benefit, Both)

#Both = RiskB * BenefitB
mynicedata$RiskB <- mynicedata$Risk
mynicedata$BenefitB <- mynicedata$Benefit
mynicedata$RiskB[mynicedata$Both == 1] <- 1 
mynicedata$BenefitB[mynicedata$Both == 1] <- 1 

#descriptive statistics
table(mynicedata$Spooky)
table(mynicedata$Explanation)
table(mynicedata$Benefit)
table(mynicedata$Risk)
table(mynicedata$Both)

table(mynicedata$BenefitB)
table(mynicedata$RiskB)

#taking weights into account
aggregate(Weights ~ Spooky, data = mynicedata, FUN = sum)
aggregate(Weights ~ Explanation, data = mynicedata, FUN = sum)
aggregate(Weights ~ BenefitB, data = mynicedata, FUN = sum)
aggregate(Weights ~ RiskB, data = mynicedata, FUN = sum)

# weighted models
# Information Seeking
mywmodel = lm(Info_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = mynicedata, weights = Weights)
summary(mywmodel)
# Internal Efficacy 
mywmodel2 = lm(IE_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = mynicedata, weights = Weights)
summary(mywmodel2)
# General Interest
mywmodel3 = lm(GI_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = mynicedata, weights = Weights)
summary(mywmodel3)
# Perceived Knowledge
mywmodel4 = lm(PK_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = mynicedata, weights = Weights)
summary(mywmodel4)



#cronbach's alpha
install.packages("ltm")
library(ltm)
#Info_seeking
Info_seeking <- data.frame(mydata$Info.seeking_1, mydata$Info.seeking_2, mydata$Info.seeking_3)
cronbach.alpha(Info_seeking)
#internal_efficacy
Internal_efficacy <- data.frame(mydata$internal.efficacy_1, mydata$internal.efficacy_2, mydata$internal.efficacy_3, mydata$internal.efficacy_4)
cronbach.alpha(Internal_efficacy)
#General_interest
General_interest <- data.frame(mydata$general.interest_1, mydata$general.interest_2, mydata$general.interest_3, mydata$general.interest_4, (6 - mydata$general.interest_5), mydata$general.interest_6)
cronbach.alpha(General_interest)                               
#Perceived_knowledge
Perceived_knowledge <- data.frame(mydata$perceived.knowledge_1, mydata$perceived.knowledge_2, mydata$perceived.knowledge_3, mydata$perceived.knowledge_4)
cronbach.alpha(Perceived_knowledge)
