library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(psych)
library(ggthemes)
library(viridis)
library(sjPlot)
library(ggcorrplot)


mydata = read_csv("data/aletta.csv")|>
  filter(Duration__in_seconds_ > 90)|>
  mutate(across(everything(), ~replace_na(.x, 0)))


#### verschillen tussen groepen
mydata|>
  select(FL_32_DO_Control.group:FL_32_DO_Enigmaticexplanationbalanced, Duration__in_seconds_, weight_trimmed_0.980)|>
  pivot_longer(FL_32_DO_Control.group:FL_32_DO_Enigmaticexplanationbalanced, names_to = 'groups')|>
  mutate(value2=value * weight_trimmed_0.980*Duration__in_seconds_)|>
  filter(value2 !=0)|>
  group_by(groups)|>
  summarise(duration=median(value2))

####hercoderen
mydata = mydata|>
  mutate(Spooky = FL_32_DO_Enigmaticframe + FL_32_DO_enigmaticframeexpl + FL_32_DO_enigmaticbenefit + FL_32_DO_enigmaticrisk + FL_32_DO_enigmaticbalanced + FL_32_DO_enigmaticexplbenef + FL_32_DO_Enigmaticexplrisk + FL_32_DO_Enigmaticexplanationbalanced,
         Explanation = FL_32_DO_Explquantumconcept + FL_32_DO_enigmaticframeexpl + FL_32_DO_explanationbenefit + FL_32_DO_explanationrisk + FL_32_DO_explanationbalanced + FL_32_DO_enigmaticexplbenef + FL_32_DO_Enigmaticexplrisk + FL_32_DO_Enigmaticexplanationbalanced,
         Benefit = FL_32_DO_benefitframe + FL_32_DO_enigmaticbenefit + FL_32_DO_explanationbenefit + FL_32_DO_enigmaticexplbenef,
         Risk = FL_32_DO_riskframe + FL_32_DO_enigmaticrisk + FL_32_DO_explanationrisk + FL_32_DO_Enigmaticexplrisk,
         Both = FL_32_DO_balancedframe + FL_32_DO_enigmaticbalanced + FL_32_DO_explanationbalanced + FL_32_DO_Enigmaticexplanationbalanced,
         Info_sum = Info.seeking_1 + Info.seeking_2 + Info.seeking_3,
         IE_sum = internal.efficacy_1 + internal.efficacy_2 + internal.efficacy_3 + internal.efficacy_4,
         GI_sum = general.interest_1 + general.interest_2 + general.interest_3 + general.interest_4 + (6 - general.interest_5) + general.interest_6,
         PK_sum = perceived.knowledge_1 + perceived.knowledge_2 + perceived.knowledge_3 + perceived.knowledge_4)|>
  mutate(RiskB=Risk + Both,
         BenefitB=Benefit + Both)

###FRAMES

frames = mydata|>
  dplyr::select(ResponseId, weight=weight_trimmed_0.980, Spooky:Both, RiskB, BenefitB)|>
  pivot_longer(Spooky:BenefitB, names_to = 'frame')|>
  group_by(frame)|>
  summarise(n = sum(value), 
            nweight=sum(value*weight))
  
frames

effect = mydata|>
  dplyr::select(ResponseId, edu_clean,sex_rec, age_cont, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))

effect|>
  mutate(sex_rec= ifelse(sex_rec==1,'man','vrouw'))|>
  group_by(sex_rec)|>
  summarise(n=sum(weight),
            minfo = mean(Info_sum),
            mie = mean(IE_sum),
            mgi = mean(GI_sum),
            mpk = mean(PK_sum))




effect_long = mydata|>
  dplyr::select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))|>
  rename(`Information Seeking`=Info_sum,
         `General Interest`=GI_sum,
         `Internal Efficiacy`= IE_sum,
         `Perceived Knowledge`= PK_sum)|>
  pivot_longer(`Information Seeking`:`Perceived Knowledge`, names_to = "Effects")



stat.test <- effect_long %>%
  group_by(Effects) %>%
  t_test(value ~ sex_rec) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test



effect_long %>%
  ggplot( aes(x=Effects, y=value, fill=Effects)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")



spooky=mydata|>
  filter(Spooky==1)|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                               age_4_rec==2 ~ "35-59 year",
                               age_4_rec==3 ~ "50-64 year",
                               T ~ "65+"))|>
  select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))|>
  rename(`Information Seeking`=Info_sum,
         `General Interest`=GI_sum,
         `Internal Efficiacy`= IE_sum,
         `Perceived Knowledge`= PK_sum)|>
  pivot_longer(`Information Seeking`:`Perceived Knowledge`, names_to = "Effects")|>
  group_by(age_4_rec,Effects)|>
  summarise(m=mean(value))

spooky

ggplot(spooky, aes(x = age_4_rec, y=m, fill=Effects, label=m))+
  geom_col(position="dodge")+
  geom_text(aes(label = round(m,1)), position= position_dodge(.9),size=3.5, vjust = 1.5, colour = "white")+
  ggtitle("Effects for respondents confronted with enigmatic framing")+
  ylab("Mean score")+
  xlab("")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")+
  theme_classic2()

  

stat.test <- effect_long %>%
  group_by(Effects) %>%
  anova_test(value ~ age_4_rec) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test

####  REGRESSION
  
library(sjmisc)

mydata|>
  dplyr::select(ResponseId, weight=weight_trimmed_0.980, Spooky: BenefitB)|>
  pivot_longer(c(Spooky:Both, RiskB, BenefitB), names_to = 'Frames', values_to = "nframes")|>
  pivot_longer(Info_sum:PK_sum, names_to = 'Effects', values_to = "neffects")|>
  mutate(nframes2=weight*nframes,
         neffects2=weight*neffects)|>
  filter(nframes==1)|>
  group_by(Frames)|>
  ggplot(aes(x=Effects, y=neffects2, fill=Effects)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin box plots per frame") +
  xlab("")+
  facet_wrap(~ Frames )



frames = mydata|>
    dplyr::select(ResponseId, weight=weight_trimmed_0.980, Spooky: BenefitB)
# weighted models
# Information Seeking
mywmodel = lm(Info_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel)
plot_model(mywmodel)
# Internal Efficacy 
mywmodel2 = lm(IE_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = Weights)
summary(mywmodel2)
# General Interest
mywmodel3 = lm(GI_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = Weights)
summary(mywmodel3)
# Perceived Knowledge
mywmodel4 = lm(PK_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = mynicedata, weights = Weights)
summary(mywmodel4)



#cronbach's alpha
#install.packages("ltm")
library(ltm)
mydata|>
  select(Info.seeking_1:perceived.knowledge_4)|>
  pivot_longer(Info.seeking_1:perceived.knowledge_4, names_to = "effects")|>
  mutate(effects2 = case_when(str_detect(effects,"Info") ~ 'Info',
                              str_detect(effects,"efficacy") ~ 'Efficacy',
                              str_detect(effects,"interest") ~ 'Interest',
                              str_detect(effects,"knowledge") ~ 'Knowledge'))|>
  group_by(effects2)|>
  summarise(alpha=cronbach.alpha(effects2))



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
