
library(Gifi)
library(knotR)
library(tidyverse)
library(psych)
library(FactoMineR)
library(vcd)
library(factoextra)
library("FactoMineR")
library(corrplot)
library("ggcorrplot")
library('corrr')


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



#####
dkids = d|>
  filter(PROJECT %in% c("rekenen","next_kids","lsdf_kids"))

mean(dkids$age, na.rm = T)
describe(dkids$age)
des = describe(dkids[11:26])

###Correlation
features = d|>
  dplyr::select(literacy:do_more)
  
features=na.omit(features)

pear_cor = cor(features)
cor.plot(pear_cor, numbers=T, upper=FALSE, main = "Pearson Correlation", show.legend = FALSE)
ggcorrplot(pear_cor,
           type = 'lower',
           hc.order =TRUE,
           lab=TRUE)



pear_eigen = eigen(pear_cor)
sum(pear_eigen$values)
cumsum(pear_eigen$values)
cumsum(pear_eigen$values)/16
plot(pear_eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

abline(h=1, lty=2)

###FACTOR ANALYSE

fa = dkids |>
  dplyr::select(literacy:do_more) |>
  mutate(across(literacy:do_more, as.factor)) |>
  mutate(rowname=row_number()) |>
  column_to_rownames() |>
  Gifi::princals(ndim=4, degrees=2)

loadings = fa$loadings |> varimax()

###UITDAGING: op basis van deze vier factoren kun je verder rekenen.
#STappenplan:
#1. maak een nieuwe variabele op basis van de vier variabelen literacy: peers en die noemen we Wetenschapskapitaal
#1a maak een nieuwe variabele op basis van de vier variabelen know_more: do_more en die noemen we Effect
#2. Maak een aantal descriptives per factor
#3. Bereken of er significante verschillen zijn tussen de waarden Wetenschapskapitaal en Effect 




