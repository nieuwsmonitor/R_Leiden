library(tidyverse)
library(sf)
library(DBI)
library(RSQLite)
library(cbsodataR)


corop = cbs_get_data("84929NED")|>
  select(Naam_2, Naam_13)|>
  rename(gemeentenaam = Naam_2)|>
  mutate(gemeentenaam = trimws(gemeentenaam))|>
  rename(corop=Naam_13)|>
  mutate(corop = trimws(corop))
  
corop


con2 <- dbConnect(SQLite(), "~/Downloads/cbsgebiedsindelingen2023.gpkg")
as.data.frame(dbListTables(con2))
corop <- dbReadTable(con2, 'coropgebied_labelpoint')
corop2 <- dbReadTable(con2, 'coropgebied_gegeneraliseerd')

  
con <- dbConnect(SQLite(), "~/Downloads/wijkenbuurten_2022_v1.gpkg")
as.data.frame(dbListTables(con))
gemeenten <- dbReadTable(con, 'gemeenten')|>
  select(gemeentenaam, aantal_inwoners)|>
  filter(aantal_inwoners != -99999999)

shapes <- read_rds("https://github.com/vanatteveldt/ccslearnr/raw/master/data/sf_nl.rds")|>
  select(gemeentenaam=gemeente, geom)


gemeenten = gemeenten|>
  select(gemeentenaam, aantal_inwoners)|>
  filter(gemeenten$gemeentenaam %in% shapes$gemeente)

shapes2=shapes|>
  left_join(gemeenten)

total_votes= 10475139
votes_per_seat=total_votes/150


write_csv(votes, "~/Dropbox/eur/tk2023.csv")|>
  rename(gemeentenaam=gemeente)

votes_max = votes|>
  rename(gemeentenaam=gemeente)|>
  mutate(gemeentenaam = case_when(gemeentenaam=="Bergen" & provincie=="Noord-Holland" ~ "Bergen (NH.)",
                                  gemeentenaam=="Bergen" & provincie=="Limburg" ~ "Bergen (L.)",
         T ~ gemeentenaam))|>
  group_by(gemeentenaam)|>
  slice_max(votes)
  


shapes3= shapes2|>
  mutate(gemeentenaam = case_when(gemeentenaam=="'s-Hertogenbosch"~ 'Den Bosch',
                                  gemeentenaam=="'s-Gravenhage"~ 'Den Haag',
                                  gemeentenaam == "Weesp" ~ "Amsterdam",
                                  gemeentenaam %in% c("Brielle", "Westvoorne", "Hellevoetsluis") ~ 'Voorne aan Zee',
         T ~ gemeentenaam))



votes_per_gemeente = shapes3|>
  left_join(votes_max)|>
  filter(! is.na(votes))


colors = votes |> 
  select(partij, color) |> 
  unique() |>
  na.omit() 

colors = setNames(object = colors$color, nm = colors$partij)


votes_per_gemeente|>as_tibble()|>
  select(-geom)|>
  group_by(partij)|>
  summarise(ninwoners=sum(aantal_inwoners))|>
  mutate(zetels=round(ninwoners/sum(ninwoners)*150))

ggplot(votes_per_gemeente) +
  geom_sf(aes(geometry = geom, fill=partij)) +
  ggtitle("Winner takes all per gemeente") +
  scale_fill_manual(values=colors) + 
  theme_void()

votes_per_provincie = votes|>
  filter(! is.na(votes))|>
  group_by(provincie, partij)|>
  summarise(n=sum(votes))|>
  slice_max(n)|>
  select(provincie, grootste=partij) 

votes_per_provincie=votes_per_gemeente|>
  left_join(votes_per_provincie)


ggplot(votes_per_provincie) +
  geom_sf(aes(geometry = geom, fill=grootste)) +
  ggtitle("Winner takes all per gemeente") +
  theme_void() 

inner_join(votes_max, gemeenten) |>
  group_by(partij) |>
  summarize(x=n()) |>
  mutate(nzetels=x/sum(x) * 150)



zetels_provincie = shapes3|>as_tibble()|>
  left_join(votes)|>
  filter(! is.na(votes))|>
  group_by(gemeentenaam)|>
  slice_head()|>
  group_by(provincie)|>
  summarise(inwoners_provincie=sum(aantal_inwoners))|>
  mutate(nzetels=round(inwoners_provincie/sum(inwoners_provincie)*150,0))


deler = tibble(deler=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

class(corop$gemeentenaam)
class(shapes3$gemeentenaam)

table(shapes3$gemeentenaam %in% corop$gemeentenaam)
table(corop$gemeentenaam %in% shapes3$gemeentenaam)

zetels_corop = shapes3|>as_tibble()|>
  left_join(corop)|>
  group_by(corop)|>
  summarise(inwoners_corop=sum(aantal_inwoners))|>
  na.omit()|>
  mutate(nzetels=round(inwoners_corop/sum(inwoners_corop)*150,0),
         nzetels=ifelse(nzetels==0,1,nzetels))

sum(zetels_corop$nzetels)




per_prov = shapes3|>as_tibble()|>
  left_join(votes)|>
  filter(! is.na(votes))|>
  left_join(zetels_provincie)|>
  group_by(provincie, partij)|>
  summarise(nvotes=sum(votes))|>
  cross_join(deler)|>
  left_join(zetels_provincie)|>
  mutate(quotient= nvotes/deler)|>
  arrange(provincie, -quotient)|>
  mutate(nrow = row_number())|>
  filter(nrow<=nzetels)|>
  group_by(provincie, partij)|>
  summarise(totzetels=sum(max(deler)))|>
  group_by(partij)|>
  summarise(zetels=sum(totzetels))





per_corop = shapes3|>as_tibble()|>
  left_join(votes)|>
  filter(! is.na(votes))|>
  left_join(corop)|>
  left_join(zetels_corop)|>
  group_by(corop, partij)|>
  summarise(nvotes=sum(votes))|>
  cross_join(deler)|>
  left_join(zetels_corop)|>
  mutate(quotient= nvotes/deler)|>
  arrange(corop, -quotient)|>
  mutate(nrow = row_number())|>
  filter(nrow<=nzetels)|>
  group_by(corop, partij)|>
  summarise(totzetels=sum(max(deler)))|>
  group_by(partij)|>
  summarise(zetels=sum(totzetels))



