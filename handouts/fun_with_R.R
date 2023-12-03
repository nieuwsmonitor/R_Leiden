
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

x*5

num = x*5
class(num)

x+3
x/2
log(x)
log(num)
sqrt(num)

y
class(y)

sum(y)

z= "99"
class(z)
z = as.numeric(z)
class(z)


v1 = c(1, 2, 10, 15)    ## a numeric vector of length 4
v2 = c("a", "b", "b")   ## a character vector of length 3
v3 = 1:10               ## a numeric vector of length 10 with the values 1 to 10. 


# WE start with the data of the Impact lab.
# We need to activate some packages. 

library(tidyverse)
library(sf)


d = read_csv("data/impact_lab.csv")

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

d

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

table(is.na(d$PROJECT))


table(d$age)
table(d$age, useNA = 'always')
table(is.na(d$age))
hist(d$age, breaks = 100)



#### Het zou leuk zijn om eens te kijken waar al die respondenten wonen!


table(d$residence, useNA = 'always')
hist(d$residence, breaks=10)
class(d$residence)
d_pc = d|>filter(residence < 10000)
hist(d_pc$residence, breaks = 20)

shapes <- read_rds("https://github.com/vanatteveldt/ccslearnr/raw/master/data/sf_nl.rds")
shapes

gemeenten = read_csv("data/gemeenten.csv")

table(gemeenten$gemeente %in% shapes$gemeente)
table(shapes$gemeente %in% gemeenten$gemeente)

check = gemeenten|>
  filter(! gemeente %in% shapes$gemeente )

check2 = shapes|>
  filter(! gemeente %in% gemeenten$gemeente )


shapes2 = gemeenten|>
  mutate(gemeente = case_when(gemeente == "Bergen (NH)" ~ "Bergen (NH.)",
                              gemeente == "Bergen (L)" ~ "Bergen (L.)",
                              gemeente == "Nuenen" ~ "Nuenen, Gerwen en Nederwetten",
                              gemeente == "Hengelo (O)" ~ "Hengelo",
                              T ~ gemeente))|>
  inner_join(shapes)
  

postcode = d|>
  filter(! is.na(residence))|>
  select(PROJECT, residence)|>
  rename(postcode=residence)|>
  right_join(shapes2)|>
  group_by(postcode)|>
  mutate(n=n())


ggplot(postcode) +
  geom_sf(aes(geometry = geom, fill = n)) +
  scale_fill_gradient(low = "white", high = "red", guide = "none") +
  ggtitle("Waar komen de bezoekers vandaan?") +
  theme_void()


