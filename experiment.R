
library(tidyverse)
library(psych)
library(ggthemes)
library(sjPlot)
library(ggcorrplot)


###Hieronder lezen we de data in.
#met de mutate kunnen we alle missing values in een keer omzetten tot 0-en
#oude versie van tidyverse kon dat nog niet voor niet-numerieke variabelen en dan ging hij bokken.
#dat kun je dus oplossen met de extra where(is.numeric) dan pakt R alleen die variabelen die numeriek zijn

mydata = read_csv("data/aletta.csv")|>
  dplyr::filter(Duration__in_seconds_ > 90)|>
  mutate(across(everything(), ~replace_na(.x, 0)))

mydata = read_csv("data/aletta.csv")|>
  dplyr::filter(Duration__in_seconds_ > 90)|>
  mutate(across(where(is.numeric), 
                ~replace_na(.x, 0)))



#DESCRIPTIVES

#### verschillen tussen groepen. In totaal zijn dat er 16 en omdat ze keurig na elkaar staan kunnen we ze selecteren met :
#hier dus met FL_32_DO_Control.group:FL_32_DO_Enigmaticexplanationbalanced.
#Daarna maken we met pivot longer er 1 variabele van en die noemen we groups
#LET OP: als je niet values_to invult om aan te geven hoe de variabele moet heten waar de waardes in komen te staan, dan maakt hij er automatisch value van.

#vervolgens filteren we alle duration == 0 eruit. Immers voor iedere respondent hebben we nu alle groepen en met deze filter houd je alleen de groep over waar de respondent ook echt in zat.
#daarna kunnen we groeperen per group en fijne gemiddeldes uitrekenen
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

#hieronder doen we wat descriptives per leeftijdsgroep.
#eerst selecteren we de variabelen die we willen hebben, age_4_rec, age_cont en de weging
#als we daarna groeperen op age_4_rec kunnen we met summary de aantallen respondenten en leeftijd berekenen

mydata|>
  select(age_4_rec, age_cont, weight_trimmed_0.980)|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                               age_4_rec==2 ~ "35-59 year",
                               age_4_rec==3 ~ "50-64 year",
                               T ~ "65+"))|>
  group_by(age_4_rec)|>
  summarise(aantal = n(),
            gem_leeftijd = mean(age_cont))


#WAT IS PER LEEFTIJDSGROEP DE GEMIDDELDE LEEFTIJD ALS JE WEEGT PER PERSOON?
#Met gewogen data moet je altijd de weging meenemen in je berekening.
#soms kan dat via een andere variabele en dan gebruik je mutate, maar veel vaker neem je het in een keer mee in de summary
#Hieronder willen we eerst het aantal mensen met n()
#het gewogen aantal is niets anders dan de sum van de weight want iedere respondent telt mee voor de waarde van de weight
#Als je dan de gemiddelde leeftijd wilt weten kun je per respondent uitreken wat de gewogen leeftijd is (age_cont*weight)
#vervolgens kun je die optellen en delen door het aantal gewogen respondenten

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



###ONDERZOEKSVRAGEN Wat is het effect van diverse frames op information seeking gedrag?
####Om te komen tot de antwoorden gaan we eerst hercoderen en zorgen ervoor dat de verschillende frames bestaan uit de diverse variabelen
#Dit doen we ook voor de Effecten die worden gemeten (Info_sum tm PK_sum)
#op het einde maken we nog twee nieuwe frames, RiskB en BenefitB omdat die bestaan uit eerder gemaakte frames.

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
#we selecteren de frames die nodig zijn. Dat zijn Spooky, Explanation, RiskB, BenefitB.
#daarnaast nemen we weight_trimmed_0.980 mee en deze hernoemen we ook direct even tot weight
#Ook nemen we ResponseID mee al is dat niet nodig (dat vind ik altijd fijn om een uniek ID te hebben)

frames = mydata2|>
  select(ResponseId, weight=weight_trimmed_0.980, Spooky, Explanation, RiskB, BenefitB)|>
  pivot_longer(Spooky:BenefitB, names_to = 'frame', values_to = "aantal")|>
  group_by(frame)|>
  summarise(n = sum(aantal), 
            nweight=sum(aantal*weight))
  
frames

####EFFECTEN
#We kunnen ook even kijken naar de effecten en verschillen zien die er wellicht zijn per opleiding, gender en age_group.
#we selecteren eerst alleen de benodigde kolommen
#Om direct alles te wegen, kunnen we met mutate across aangeven dat we de effect variabelen willen vervangen door de waarde * de weight

effect = mydata2|>
  select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))

#hieronder zie je dat de waardes van de frames zijn veranderd
effect

#hieronder kijken we of er verschil zit tussen gender
effect|>
  mutate(sex_rec= ifelse(sex_rec==1,'man','vrouw'))|>
  group_by(sex_rec)|>
  summarise(n=sum(weight),
            minfo = mean(Info_sum),
            mie = mean(IE_sum),
            mgi = mean(GI_sum),
            mpk = mean(PK_sum))


#UITDAGING: kun je deze mean ook even maken voor de verschillende leeftijdsgroepen?
#er is een variabele die heeft age_4_rec en die kennen de volgende indeling:
#1 staat voor 18-34 jaar
#2 staat voor 35-59 jaar
#3 staat voor 50-64 jaar
#3 staat voor 65+

effect|>
  mutate(age_4_rec = case_when(age_4_rec==1 ~ "18-34 year",
                             age_4_rec==2 ~ "35-59 year",
                             age_4_rec==3 ~ "50-64 year",
                             T ~ "65+"))|>
  group_by(age_4_rec)|>
  summarise(n=sum(weight),
            minfo = mean(Info_sum),
            mie = mean(IE_sum),
            mgi = mean(GI_sum),
            mpk = mean(PK_sum))



###BOXPLOT PER EFFECT
#Hieronder maken we een figuur met boxplots per effect
#we beginnen met data2 omdat daar de effecten al zijn gedefinieerd
#selectie van alle varaibelen die we nodig hebben (in dit geval zijn niet alle groups variabelen  nodig maar kan ook geen kwaad)
#we mutaten weer even de weging over de variabelen waar het om draaig, namelijk Info_sum tm PK_sum
#we renamen de variabelen omdat we deze gewoon mooi in het plaatje willen hebben...
#vervolgens maken we met pivot long van alle kolommen naast elkaar een kolom onder elkaar en die noemen we Effects
#LET OP, de values komen nu in de kolom value omdat we die geen naam hebben gegeven
library(viridis)


effect_long = mydata2|>
  dplyr::select(ResponseId, edu_clean,sex_rec, age_4_rec, weight=weight_trimmed_0.980, Info_sum:PK_sum)|>
  mutate(across(c(Info_sum:PK_sum), function(x) x*weight))|>
  rename(`Information Seeking`=Info_sum,
         `General Interest`=GI_sum,
         `Internal Efficiacy`= IE_sum,
         `Perceived Knowledge`= PK_sum)|>
 pivot_longer(`Information Seeking`:`Perceived Knowledge`, names_to = "Effects")

#Hieronder kunnen we vervolgens een leuk plotje maken

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

#om te achterhalen of de verschillende elementen waaruit de effectvariabele bestaat ook consistent meten wat je wilt meten kun je kijken naar de interne consistentie van de variabele
#dat meet je met een cronbachs alfa. 
#Hieronder doen we dat voor alle vier de effecten. WE gaan uit van data2 en pakken dan met select starts with alle variabelen die beginnen met een bepaald woord
#dit werkt heel goed als alle variabelen beginnen met hetzelfde woord (of getal of deel van een woord) en er geen andere variabelen zijn die met het zelfde getal/woord beginnen
#Kortom: let op de boekhouding van de data!


#LET OP het pakket ltm overschrijft delen van tidyverse (kun je zin in de console, daar zegt R:The following object is masked from ‘package:dplyr’:select 
#Dit betekent dat wanneer je select wil gebruiken uit tidyverse pakket dplyr dan moet je dat ervoor zetten zoals hieronder
#als R een onbegrijpelijke foutmelding geeft terwijl je zeker weet dat de variabelen die je wilt selecteren bestaan dan is dit meestal aan de hand...

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
#Allemaal leuk en aardig, je kunt dat per variabele doen maar dat kan ook in een for loop.

variables = c("Info","internal", "general","perceived")

for (v in variables){ #hier pak je steeds een variabele uit de lijst variabelen en die noemen we v
    mydata3=mydata|>dplyr::select(starts_with(v)) #hier vult ie dan steeds de naam in
    ca = cronbach.alpha(mydata3) #berekening van de cronbachs alpha
    message("Cronbach alpha voor: ",v) #dit is een boodschap die je zelf kunt geven en dan vult R steeds v in
    print(ca) #pring
}


###REGRESSION
library(sjmisc)
#hieronder per Frame het effect gemeten in een plot
#LET OP we doen 2x pivot longer omdat we twee aparte variabelen willen maken, Frames en Effects.
#NU is het ook belangrijk om dan wel values_to mee te nemen zodat je weet welke value waarbij hoort.
#WE doen nu nframes en neffects
#vervolgens vermenigvuldigen we nog even met de weight en gaan we grouperen op Frames om vervolgens plotten te maken per effect
#en met een facet wrap maken we er een figuur van

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


###REGRESSIE ANALYSE
# weighted models
#we selecteren alle variabelen en noemen het bestand frames

frames = mydata2|>
  dplyr::select(ResponseId, weight=weight_trimmed_0.980, Spooky: BenefitB)

# Information Seeking
mywmodel = lm(Info_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
summary(mywmodel)

mywmodel2 = lm(IE_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
mywmodel3 = lm(GI_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)
mywmodel4 = lm(PK_sum ~ RiskB * BenefitB + Spooky  + Explanation, data = frames, weights = weight)

library(sjPlot)
#Dit is een heel cool pakket zie https://cran.r-project.org/web/packages/sjPlot/index.html
tab_model(mywmodel)
tab_model(mywmodel, mywmodel2, mywmodel3,mywmodel4)

sjPlot::plot_model(mywmodel)
sjPlot::plot_model(mywmodel, type="pred", terms=c("RiskB", "BenefitB"))



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





