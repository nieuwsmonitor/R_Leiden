
library(tidyverse)
library(psych)
library(ggthemes)
library(sjPlot)
library(ggcorrplot)

mydata = read_csv("data/aletta.csv")|>
  dplyr::filter(Duration__in_seconds_ > 90)|>
  mutate(across(everything(), ~replace_na(.x, 0)))

mydata = read_csv("data/aletta.csv")|>
  dplyr::filter(Duration__in_seconds_ > 90)|>
  mutate(across(where(is.numeric), 
                ~replace_na(.x, 0)))



#DESCRIPTIVES

#### verschillen tussen groepen
colnames(mydata)

mydata|>
  select(FL_32_DO_Control.group:FL_32_DO_Enigmaticexplanationbalanced, Duration__in_seconds_, weight_trimmed_0.980)|>
  pivot_longer(FL_32_DO_Control.group:FL_32_DO_Enigmaticexplanationbalanced, names_to = 'groups')|>
  mutate(duration=value * weight_trimmed_0.980*Duration__in_seconds_)|>
  filter(duration !=0)|>
  group_by(groups)|>
  summarise(aantal = n(),
            mean_duration=mean(duration),
            median_duration=median(duration),
            sd_duration=sd(duration))

#WAT IS PER LEEFTIJDSGROEP DE GEMIDDELDE LEEFTIJD ALS JE WEEGT PER PERSOON?

mydata|>
  select(age_4_rec, age_cont, weight_trimmed_0.980)|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                               age_4_rec==2 ~ "35-59 year",
                               age_4_rec==3 ~ "50-64 year",
                               T ~ "65+"))|>
  group_by(age_4_rec)|>
  summarise(aantal = n(),
            gewogen_n=sum(weight_trimmed_0.980),
            gem_leeftijd = sum(age_cont*weight_trimmed_0.980)/gewogen_n)


mydata|>
  select(age_4_rec, age_cont, weight_trimmed_0.980)|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                               age_4_rec==2 ~ "35-59 year",
                               age_4_rec==3 ~ "50-64 year",
                               T ~ "65+"))|>
  group_by(age_4_rec)|>
  summarise(aantal = n(),
            gem_leeftijd = mean(age_cont))





#####Uitdaging: Dit zijn ongewogen cijfers. 
#Kun je uitrekenen wat de gemiddelde leeftijd is per age groep als je weegt?



###ONDERZOEKSVRAGEN Wat is het effect van diverse frames op information seeking gedrag?
####hercoderen
mydata2 = mydata|>
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
#number of participants per frame non-weighted and weighted
 
frames = mydata2|>
  select(ResponseId, weight=weight_trimmed_0.980, Spooky, Explanation, RiskB, BenefitB)|>
  pivot_longer(Spooky:BenefitB, names_to = 'frame', values_to = "aantal")|>
  group_by(frame)|>
  summarise(n = sum(aantal), 
            nweight=sum(aantal*weight))
  
frames

####EFFECTEN

effect = mydata2|>
  select(ResponseId, edu_clean,sex_rec, age_cont, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))

effect|>
  mutate(sex_rec= ifelse(sex_rec==1,'man','vrouw'))|>
  group_by(sex_rec)|>
  summarise(n=sum(weight),
            minfo = mean(Info_sum),
            mie = mean(IE_sum),
            mgi = mean(GI_sum),
            mpk = mean(PK_sum))


##UITDAGING: kun je deze mean ook even maken voor de verschillende leeftijdsgroepen?
#er is een variabele die heeft age_4_rec en die kennen de volgende indeling:
#1 staat voor 18-34 jaar
#2 staat voor 35-59 jaar
#3 staat voor 50-64 jaar
#3 staat voor 65+



###BOXPLOT PER EFFECT
library(viridis)
library(tidyverse)

effect_long = mydata2|>
  dplyr::select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))|>
  rename(`Information Seeking`=Info_sum,
         `General Interest`=GI_sum,
         `Internal Efficiacy`= IE_sum,
         `Perceived Knowledge`= PK_sum)|>
 pivot_longer(`Information Seeking`:`Perceived Knowledge`, names_to = "Effects")



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


###INTERNAL CONSISTENCY
library(ltm)

mydata2|>
  dplyr::select(starts_with("Info"))|>
  cronbach.alpha()

mydata|>
  dplyr::select(starts_with("internal"))|>
  cronbach.alpha()

mydata|>
  dplyr::select(starts_with("general"))|>
  cronbach.alpha()


mydata|>
  dplyr::select(starts_with("perceived"))|>
  cronbach.alpha()

#DAT KAN EFFICIENTER

variables = c("Info","internal", "general","perceived")

for (v in variables){
    mydata3=mydata|>dplyr::select(starts_with(v))
    ca = cronbach.alpha(mydata3)
    message("Cronbach alpha voor: ",v)
    print(ca)
}


###REGRESSION
library(sjmisc)

mydata2|>
  dplyr::select(weight=weight_trimmed_0.980, Spooky: BenefitB)|>
  pivot_longer(c(Spooky:Explanation, RiskB, BenefitB), names_to = 'Frames', values_to = "nframes")|>
  pivot_longer(Info_sum:PK_sum, names_to = 'Effects', values_to = "neffects")|>
  mutate(nframes2=weight*nframes,
         neffects2=weight*neffects)|>
  filter(nframes==1)|>
  group_by(Frames)|>
  ggplot(aes(x=Effects, y=neffects2, fill=Effects)) +
  geom_jitter(color="black", size=0.04, alpha=0.9) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Jitter box plots per frame") +
  xlab("")+
  facet_wrap(~ Frames )





# weighted models
frames = mydata2|>
  dplyr::select(ResponseId, weight=weight_trimmed_0.980, Spooky: BenefitB)

# Information Seeking
mywmodel = lm(Info_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel)

library(sjPlot)
tab_model(mywmodel)

sjPlot::plot_model(mywmodel)
sjPlot::plot_model(mywmodel, type="pred", terms=c("RiskB", "BenefitB"))

# Internal Efficacy 
mywmodel2 = lm(IE_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel2)

tab_model(mywmodel, mywmodel2, mywmodel3,mywmodel4)
sjPlot::plot_model(mywmodel2)
sjPlot::plot_model(mywmodel2, type="pred", terms=c("RiskB", "BenefitB"))



# General Interest
mywmodel3 = lm(GI_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel3)
# Perceived Knowledge
mywmodel4 = lm(PK_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel4)


######
library(ggthemes)

#Explanation, BenefitB en RiskB

spooky=mydata2|>
  filter(Spooky==1)|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                               age_4_rec==2 ~ "35-59 year",
                               age_4_rec==3 ~ "50-64 year",
                               T ~ "65+"))|>
  dplyr::select(age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
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
  geom_text(aes(label = round(m,1)), position= position_dodge(.9),size=3.5, vjust = 4, colour = "white")+
  ggtitle("Effects for respondents confronted with enigmatic framing")+
  ylab("Mean score")+
  xlab("")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")+
  theme_economist()

  

###T-testen
library(car)

effect2 = mydata2|>
  dplyr::select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))|>
  rename(`Information Seeking`=Info_sum,
         `General Interest`=GI_sum,
         `Internal Efficiacy`= IE_sum,
         `Perceived Knowledge`= PK_sum)





variables = c("Information Seeking","General Interest","Internal Efficiacy","Perceived Knowledge")

for (v in variables){
  lt = leveneTest(effect2$sex_rec, effect2[[v]])
  message("levene-test voor: ", v)
  print(lt)
}

boxplot(`Information Seeking` ~ sex_rec,
        data = effect2,
        main = "Box plot Information Seeking",
        xlab = "Sekse",
        ylab = "Information Seeking",
        col = "steelblue",
        border = "black")


for (v in variables){
  t = t.test(effect2$sex_rec, effect2[[v]], var.equal = F)
  message("t-test voor: ", v)
  print(t)
}





