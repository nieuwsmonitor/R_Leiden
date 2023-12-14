library(psych)
library(ggthemes)
library(tidyverse)
library(viridis)
library(sjPlot)
library(ggcorrplot)


####INSTELLINGEN 
getOption("tibble.view_max")
options(pillar.print_max=10,
        pillar.print_min=6)




#we bespreken summarize, recoding, select, en pivot
#rename en recode
d = read_csv("data/impact_lab.csv")|>as_tibble()|>
  rename(age=DEMOGR_1,
         gender=DEMOGR_2,
         education = DEMOGR_3,
         residence = DEMOGR_4,
         literacy = WK_1,
         attitude = WK_2,
         activity = WK_3,
         peers = WK_4,
         happy = EM_P1,
         relaxt = EM_P2,
         hope = EM_P3,
         satisfied = EM_P4,
         thrilling = EM_I1,
         energetic = EM_I2,
         excited = EM_I3,
         interested = EM_I4,
         know_more = GENERAL_1,
         want_more = GENERAL_2,
         opinion_change = GENERAL_3,
         do_more = GENERAL_4,
         child_know_more = GENERAL_1_kind,
         child_want_more = GENERAL_2_kind,
         child_opinion_change = GENERAL_3_kind,
         child_do_more = GENERAL_4_kind
  )|>mutate(PROJECT = case_when(PROJECT == 1 ~ "next_kids",
                                PROJECT == 2 ~ "media_literacy",
                                PROJECT == 3 ~ "girls_day",
                                PROJECT == 4 ~ "next_adult",
                                PROJECT == 5 ~ "lsdf_kids",
                                PROJECT == 6 ~ "lsdf_adult",
                                PROJECT == 7 ~ "stoepplantjes",
                                PROJECT == 8 ~ "sanne",
                                PROJECT == 9 ~ "next_nioz",
                                PROJECT == 10 ~ "rekenen",
                                PROJECT == 11 ~ "zwarte_cross"),
            soort_project= ifelse(PROJECT %in% c("rekenen", "next_kids", "lsdf_kids", "girls_day"), "kids", "adults"),
            leeftijdgroep = ifelse(age<18 , "kids","volwassenen"),
            gender = case_when(gender == 1 ~ "man",
                               gender == 2 ~ "vrouw",
                               T ~ "overig"))
  


#Overzicht aantal resondenten
head(d)
table(d$PROJECT)

projects= d|>
  group_by(PROJECT)|>
  summarise(n=n())|>
  mutate(perc = n/sum(n)*100)|>
  arrange(-n)

projects

##GGPLOT
#https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

ggplot(data=projects, aes(x=reorder(PROJECT, -n), y=n, fill=PROJECT)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=round(n,0)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10))+
  labs(y="Aantal respondenten")+
  labs(x="")+
  ggtitle("Aaantal respondenten per project")+
  theme(legend.position = 'none')

#UITDAGING
#Kun je Per groep, kids en volwassenen, een pie chart maken van het aantal respondenten per project?

###Hoeveel mensen per leeftijdsgroep kids versus volwassenen?
describe(d$age)

leeftijd= d|>
  filter(! is.na(age), ! is.na(gender))|>
  group_by(leeftijdgroep,gender)|>
  summarise(n=n())|>
  mutate(perc = n/sum(n)*100)



mycols <- c("yellow","green","red","blue")

ggplot(leeftijd, aes(x = "", y = perc, fill = gender)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = round(perc,0)), color = "black", position=position_stack(vjust=0.5))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  facet_grid(~leeftijdgroep)


#alleen volwassenen
leeftijd1 = d|>
  filter(age>18)

boxplot(age~gender,
        data = leeftijd1,
        main="Boxplots voor leeftijdsgroepen",
        xlab="Groepen respondenten",
        ylab="Leeftijd",
        col="orange",
        border="brown"
)

###significante verschillen tussen gender?

res.aov <- aov(age ~ gender, data = leeftijd1)
summary(res.aov)
TukeyHSD(res.aov)

leeftijd2 = leeftijd1|>
  filter(gender != "overig")

t.test(age ~ gender, data=leeftijd2)



###EMOTIES

emoties = d|>
  select(happy:interested)

cor(emoties)

emoties = na.omit(emoties)

cor(emoties)
class(emoties)

cor_emoties= cor(emoties)
class(cor_emoties)

cor.plot(emoties, numbers=T, upper=FALSE, main = "Pearson Correlation", show.legend = FALSE)

ggcorrplot(cor_emoties,
           type = 'lower',
           hc.order =TRUE,
           lab=TRUE)


emoties = d|>
  select(PROJECT, age, gender, education, happy:interested)

emoties


emoties = emoties|>
  pivot_longer(happy:interested, names_to = 'emoties', values_drop_na=TRUE)


emoties

boxplot(value~emoties,
        data = emoties,
        main="Boxplots voor emoties",
        xlab="Soort emotie",
        ylab="Score",
        col="orange",
        border="brown"
)

##met GGPLOT kan het mooier...
#ZIE: https://r-graph-gallery.com/index.html

emoties %>%
  ggplot( aes(x=emoties, y=value, fill=emoties)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")



emoties2= emoties|>
  group_by(emoties)|>
  summarise(n=n(),
            m=mean(value),
            sd = sd(value),
            min = min(value),
            max = max(value))


emoties2

####Verschil in emoties

res.aov <- aov(value ~ emoties, data = emoties)
summary(res.aov)
TukeyHSD(res.aov)


happy = emoties|>
  filter(emoties=="happy")|>
  group_by(gender)|>
  summarise(n=n(),
            m=mean(value),
            sd = sd(value))


happy2 = emoties|>
  filter(emoties=="happy" & gender %in% c("man","vrouw"))
  
  
t.test(value ~ gender, data = happy2)

#UITDAGING:
#Kun je op basis van de factoranalyse die is gedaan berekenen of er een verschil is tussen gender 
# en wetenschapskapitaal, plezierbeleving, intensiteit en effect? 
# zie voor indeling https://impactlab.sites.uu.nl/wp-content/uploads/sites/764/2023/05/Statistische-analyses.pdf

####LIKERT SCALES

head(d)

l = d|>
  select(PROJECT, age, gender,literacy:peers)

table(l$literacy, useNA = 'always')

likert_recode <- function(x) {
  y <- case_when(is.na(x) ~ NA,
                 x == 1 ~ "Strongly disagree",
                 x == 2 ~ "Disagree",
                 x == 3 ~ "Neutral",        
                 x == 4 ~ "Agree", 
                  T ~ "Strongly agree")
    y <- factor(y, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
  return(y)
}


likert_recode <- function(x) {
  case_match(x, 
                  1 ~ "Strongly disagree",
                  2 ~ "Disagree",
                  3 ~ "Neutral",        
                  4 ~ "Agree", 
                  5 ~ "Strongly agree")|>
  factor(levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
}


likert_recode <- function(x, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")) {
  factor(levels[x], levels)
}



table(d$literacy)


l2 = d|>
  select(PROJECT, gender, literacy:peers)|>
  mutate(across(literacy:peers, ~ likert_recode(.x)))|> #mutate meerdere kolommen
  filter(if_all(literacy:peers, ~ !is.na(.)))|>
  pivot_longer(literacy:peers, names_to = "variabele")|>
  group_by(variabele, value)|>
  summarise(n=n())|>
  mutate(perc=n/sum(n)*100) |>
  mutate(label=case_match(variabele,
 "literacy" ~ "Ik ben in het algemeen op de hoogte over wetenschappelijke ontwikkelingen.", 
           "attitude"~"Ik heb interesse in de inzichten en methodes uit de wetenschap.", 
           "activity" ~ "Ik doe soms dingen waarbij ik iets kan leren over wetenschap.",
           "peers" ~ "Ik spreek regelmatig over wetenschap met anderen.")) 
  

ggplot(l2, aes(y = label, x=perc, fill=value, label=round(perc, 1)))+
  geom_col(position="stack")+
  geom_text(data = filter(l2, perc>5),aes(color=value), position=position_stack(vjust=.5, reverse = FALSE), size=3) +
  ggtitle("Wetenschapskapitaal")+

  ylab("Percentage")+
  xlab("Wetenschapskapitaal ")+
  scale_color_manual(values = c("Agree"='black', "Neutral"='black', "Disagree"='black'), na.value="white", guide="none")+
  scale_fill_brewer(palette="PRGn", breaks=rev)+
  theme(legend.position="bottom")



###EIGEN VARIABELE NAMEN
#STEL de vragen gingen over iets heel anders, namelijk een schaal verveeld tot ontspanne, ontevreden tot tevreden, ongelukkig tot gelukkig en hopeloos tot hoopvol
#Als je dan de waarden op twee assen mee wilt geven dan kan dat op de volgende manier. 
#hier maak je eerst de twee labels

labels1 <- c("Verveeld", "Ontevreden","Ongelukkig", "Hopeloos")
labels2 <- c("Ontspannen","Tevreden","Gelukkig","Hoopvol")

# We maken een nieuwe klasse die erft van de normale guide_axis en die een transformatie functie opslaat
# En vervolgens overschrijven we de guide_train functie om de labels te transformeren met de opgeslagen functie
# Zie http://adv-r.had.co.nz/S3.html voor meer info over wat in R doorgaat voor object orientatie
guide_axis_label_trans <- function(transform_function, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$transform_function <- rlang::as_function(transform_function)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}
guide_train.guide_axis_trans <- function(x, ...) {
  guide <- NextMethod()
  guide$key$.label <- x$transform_function(guide$key$.label)
  guide
}

l2 |>
  ggplot(aes(y = label, x=perc, fill=value, label=round(perc, 1)))+
  geom_col(position="stack")+
  geom_text(data = filter(l2, perc>5),aes(color=value), position=position_stack(vjust=.5, reverse = FALSE), size=3) +
  ggtitle("Wetenschapskapitaal")+
  ylab("Percentage")+
  xlab("Wetenschapskapitaal ")+
  scale_y_discrete(labels = labels1)+
  guides(y.sec = guide_axis_label_trans(~labels2[match(.x, labels1)])) +
  scale_color_manual(values = c("Agree"='black', "Neutral"='black', "Disagree"='black'), na.value="white", guide="none")+
  scale_fill_brewer(palette="PRGn", breaks=rev)+
  theme(legend.position="bottom")




