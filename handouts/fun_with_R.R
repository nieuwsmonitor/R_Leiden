
#### Some basics
#running code

3 + 3
2 * 5
6 / 2
"some text"
"some more text"
sum(1,2,3,4,5)

#assigning values to names

x = 2
y <- "some text"

x
y

x*5
y*5
num = x*5
class(num)
class(y)
x+3

x/2
log(x)
log(num)
sqrt(num)

y
class(y)

sum(y)

z= '99'
class(z)
2*z
z = as.numeric(z)
class(z)


v1 = c(1, 2, 10, 15)    ## a numeric vector of length 4
v2 = c("a", "b", "b")   ## a character vector of length 3
v3 = 1:10               ## a numeric vector of length 10 with the values 1 to 10. 
class(3)

v4=v1+10
v5= v3+10
v5
v4
class(v3)
class(v2)

# WE start with the data of the Impact lab.
# We need to activate some packages. 
library(tidyverse)
library(sf)

getwd() #check in welke working directory je eigenlijk aan het werk bent

d = read_csv("data/impact_lab.csv")|>as_tibble()

head(d)# hiermee bekijk je even de eerst tien rijen
tail(d)# hiermee de laatste 10

class(d) #check wat voor soort data het eigenlijk is

#hieronder rename ik de variabele met eigen namen die duidelijker zijn.
#ik kies altijd voor een naam in een woord of met _ zodat je niet ` ` hoeft te gebruiken (dat moet als je variabele naam uit twee woorden bestaat)
d = d|>
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
         excited = EM_I4,
         know_more = GENERAL_1,
         want_more = GENERAL_2,
         opinion_change = GENERAL_3,
         do_more = GENERAL_4,
         child_know_more = GENERAL_1_kind,
         child_want_more = GENERAL_2_kind,
         child_opinion_change = GENERAL_3_kind,
         child_do_more = GENERAL_4_kind
         )


table(d$PROJECT) #even tabel met aantal respondenten per project

mean(d$age)#zo kun je even snel een mean berekenen
#je krijgt nu alleen een warning omdat er missing values zijn!
leeftijd = d|>
  filter(! is.na(age)) #zo filter je op alle variabelen die NIET missing zijn

mean(leeftijd$age) #nu werkt het wel!
min(leeftijd$age) #minimum leeftijd
max(leeftijd$age) # maximum leeftijd


#Hieronder hernoemen we de variabele PROJECT met de namen
#dit is anders dan rename want hier wil je rijen hernoemen geen kolommen
#in feite overschrijf je de variabele PROJECT met zichzelf en geef je per nummer een andere waarde

d = d|>mutate(PROJECT = case_when(PROJECT == 1 ~ "next_kids",
                             PROJECT == 2 ~ "media_literacy",
                             PROJECT == 3 ~ "girls_day",
                             PROJECT == 4 ~ "next_adult",
                             PROJECT == 5 ~ "lsdf_kids",
                             PROJECT == 6 ~ "lsdf_adult",
                             PROJECT == 7 ~ "stoepplantjes",
                             PROJECT == 8 ~ "sanne",
                             PROJECT == 9 ~ "next_nioz",
                             PROJECT == 10 ~ "rekenen",
                             PROJECT == 11 ~ "zwarte_cross"))

table(d$PROJECT)


table(d$age)
table(d$age, useNA = 'always')
table(is.na(d$age))
hist(d$age, breaks = 100)#histogram in Base R



#### Het zou leuk zijn om eens te kijken waar al die respondenten wonen!


table(d$residence, useNA = 'always') #we kijken even hoeveel missing values er zijn
hist(d$residence, breaks=10) #histogram waardoor je ziet dat er iemand een rare postcode heeft ingevuld
class(d$residence) #check wat voor soort variabele het is (of het niet stiekem een string - character - is)

d_pc = d|>filter(residence < 10000) #hier selecteren we even alleen de postcodes die we willen houden
hist(d_pc$residence, breaks = 20) #nieuwe histogram

#We willen een plaatje maken van Nederland en dat kan met het pakket SF maar dan moeten we wel data hebben per gemeente welke geolocaties die hebben
#die data halen we van Wouter z'n github
#LET OP je ziet dat het een rds file is, dat is een formaat van R (iets compacter dan csv) maar maakt verder geen verschil.
#het wordt ook een dataframe

shapes <- read_rds("httpes://github.com/vanatteveldt/ccslearnr/raw/master/data/sf_nl.rds")
shapes

#Deze data is allemaal leuk en aardig maar je moet dan nog wel weten welke gemeente welke postcode heeft.
#dat staat in onderstaande data (deze heb ik obv openbare data ook gemaakt)
gemeenten = read_csv("data/gemeenten.csv")

#hier check je even of alle gemeente in gemeenten ook staan in shapes
#we willen deze namelijk aan elkaar koppelen en dan is het wel zo handig
table(gemeenten$gemeente %in% shapes$gemeente)
table(shapes$gemeente %in% gemeenten$gemeente)

#je ziet dat ze niet allemaal overeenkomen dus gaan we even checken welke wel en niet kloppen.
#ik noem dat altijd even check oid maar dat kun je ook gewoon laten en is soms zels handiger als je niet teveel onnodige bestanden rechts wilt heben staan in Environment
check = gemeenten|>
  filter(! gemeente %in% shapes$gemeente )

check2 <- shapes|>
  filter(! gemeente %in% gemeenten$gemeente )

#Een aantal gemeenten bleek anders te zijn gespeld. DAT IS ALTIJD GEDOE!!
#Hieronder lossen we dat op door de namen in gemeenten aan te passen aan die van shapes 
#dat kan natuurlijk ook andersom, hangt af van wat je wilt...
#Als we dat hebben gedaan koppelen we de bestanden gemeenten en shapes aan elkaar en dat noemen we dan shapes2
#Je ziet dat shapes 2 nu 2 extra kolommen erbij heeft, namelijk Gemcode en postcode (gemcode was niet nodig maar kan ook geen kwaad)

# We INNER JOINEN omdat we alle data willen houden. We hebben ALLE gemeenten van shapes nodig omdat we een plaatje van HEEL Nederland willen.

shapes2 = gemeenten|>
  mutate(gemeente = case_when(gemeente == "Bergen (NH)" ~ "Bergen (NH.)",
                              gemeente == "Bergen (L)" ~ "Bergen (L.)",
                              gemeente == "Nuenen" ~ "Nuenen, Gerwen en Nederwetten",
                              gemeente == "Hengelo (O)" ~ "Hengelo",
                              T ~ gemeente))|>
  inner_join(shapes)

#Hieronder maken we een nieuw bestand. We willen namelijk per postcode het aantal mensen weten die daar vandaan komen.
#we selecteren daarom residence
# die hernoemen we naar postcode (omdat we hierna gaan joinen met shapes 2 en daar heet de variabele ook postcode)
# we RIGHT joinen hieronder omdat we ALLE gemeentes willen houden
#daarna grouperen we per postcode en tellen het aantal respondenten
#uiteindleijk heb je dan een bestand met per postcode in Nederland hoeveel respondenten er waren.
#de meeste postcodes hebben waarde 1, maar sommige postcodes hebben er meer waardoor die plekken rood kleuren in het kaartje

#dit is een hele grappige manier om te visualiseren maar pas op (dat realiseer ik me nu ook pas) dat het nu wel lijkt alsof er minstens 1 persoon uit iedere gemeente komt.
#Ik heb het hieronder netjes opgelost, door eerst de survey data van het impactlab allemaal een 1 te geven voor inwoner variabele
#vervolgens hebben we alle postcodes (shapes2) eraan gejoind en gezegd dat deze bij de variabele 0 moeten krijgen als die er niet is.
#oftewel, ieder postcode wordt nu 0 tenzij er iemand het heeft ingevuld in de survey.
#dan ga ik daar de sum van nemen, ipv te tellen.
#de n2 telt wel alle postcodes zoals we in eerste instantie deden


postcode=d|>
#  filter(! is.na(residence), age<18)|> #dit is de selectie voor kids die je ook in de code kunt uit commenten met #
  select(residence)|>
  rename(postcode=residence)|>
  mutate(inwoner=1)|>
  right_join(shapes2)|>
  mutate(inwoner=ifelse(is.na(inwoner),0, inwoner))|>
  group_by(postcode)|>
  mutate(n=sum(inwoner),
         n2=n())


#hier doe ik ze even na elkaar eerst met n dan met n2
#omdat er bij n teveel 0en zijn wordt het veel witter.
#bij n2 zijn de meeste gewoon 1 waardoor de paar die 2 of meer zijn veel sneller opvallen...

ggplot(postcode) +
  geom_sf(aes(geometry = geom, fill = n)) +
  scale_fill_gradient(low = "white", high = "red", guide = "none") +
  ggtitle("Waar komen de bezoekers vandaan?") +
  theme_void()

ggplot(postcode) +
  geom_sf(aes(geometry = geom, fill = n2)) +
  scale_fill_gradient(low = "white", high = "red", guide = "none") +
  ggtitle("Waar komen de bezoekers vandaan?") +
  theme_void()



####KIJKEN NAAR LITERACY

# We hadden bedacht om te kijken naar de verschillende variabelen die iets zeggen over de voorkennis van de respondenten.
# Een variabele gaat over de mate waarin men al kennis heeft van wetenschappelijke ontwikkelingen en die hebben we even literacy genoemd

table(is.na(d$literacy))#we zien dat 741 mensen die hebben ingevuld

table(d$literacy, useNA = 'always')

#Om daar een histogram van te maken kun je idd ggplot gebruiken met histogram
# om verschillen tussen de groepen kids vs adults te krijgen maken we een aparte variabele die dat aangeeft.
#age2 is of kids of adults
#vervolgens plotten we deze naast elkaar in een histogram.
#dit kan nog veel mooier natuurlijk...doen we volgende keer!
d|>
  filter(! is.na(literacy), !is.na(age))|>
  mutate(age2= ifelse(age<18,"kids","adults"))|>
  select(PROJECT, literacy, age2)|>
  ggplot(aes(x=literacy, color=age2)) +
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins = 5, binwith=3)


#we kunnen het ook op een andere manier doen.
#we kunnen ook de age groepen maken en dan per age groep kijken hoeveel mensen dat hebben ingevuld om vervolgens een pie chart te maken.
#dat doen we door gebruik te maken van group_by en summarize (zie tidy-summarise als handout)
#hieronder kijken we per group naar de mate van literacy dat men heeft ingevuld
#de aantallen tellen we op met summarise en daarna maken we daar nog een percentage van
#LETOP: omdat de data nog is gegroepeerd per age, literacy maakt R percentages per Age groep en niet voor alles
lit=d|>
  filter(! is.na(literacy), !is.na(age))|>
  mutate(age2= ifelse(age<18,"kids","adults"))|>
  select(PROJECT, literacy, age2)|>
  group_by(age2, literacy)|>
  summarise(n=n())|>
  mutate(perc=n/sum(n)*100)

#VErvolgens kunnen we hieronder dan een pie chart maken.
#en met de laatste regel kunnen we deze splitten in de groepen binnen age2, adults en kids.

ggplot(lit, aes(x = "", y = perc, group=literacy, fill = literacy)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(perc,1)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme(legend.position = "none")+
  facet_wrap(~age2)
