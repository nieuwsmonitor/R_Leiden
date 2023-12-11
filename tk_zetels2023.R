library(tidyverse)
library(sf)
library(DBI)



corop = read_csv("data/corop_gebieden.csv")
shapes3 = readRDS("data/geo_shapes.rds")
shapes4 = shapes3|>
  left_join(corop)


total_votes= 10475139
votes_per_seat=total_votes/150

votes = read_csv("data/tk2023.csv")|>
  rename(gemeentenaam=gemeente)

votes_max = votes|>
  mutate(gemeentenaam = case_when(gemeentenaam=="Bergen" & provincie=="Noord-Holland" ~ "Bergen (NH.)",
                                  gemeentenaam=="Bergen" & provincie=="Limburg" ~ "Bergen (L.)",
         T ~ gemeentenaam))|>
  group_by(gemeentenaam)|>
  slice_max(votes)
  


votes_per_gemeente = shapes4|>
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


####PER PROVINCIE

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


zetels_provincie = shapes4|>as_tibble()|>
  left_join(votes)|>
  filter(! is.na(votes))|>
  group_by(gemeentenaam)|>
  slice_head()|>
  group_by(provincie)|>
  summarise(inwoners_provincie=sum(aantal_inwoners))|>
  mutate(nzetels=round(inwoners_provincie/sum(inwoners_provincie)*150,0))


deler = tibble(deler=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

zetels_corop = shapes4|>as_tibble()|>
  group_by(corop)|>
  summarise(inwoners_corop=sum(aantal_inwoners))|>
  na.omit()|>
  mutate(nzetels=round(inwoners_corop/sum(inwoners_corop)*150,0),
         nzetels=ifelse(nzetels==0,1,nzetels))

sum(zetels_corop$nzetels)


###toepassen 


per_prov = shapes4|>as_tibble()|>
  left_join(votes)|>
  filter(! is.na(votes))|>
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



per_corop = shapes4|>as_tibble()|>
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





total_votes= 10475139+69047
votes_per_seat=total_votes/150

votes23=votes|>
  group_by(partij)|>
  summarise(votes=sum(votes))|>
  mutate(perc = votes/sum(votes)*100)|>
  mutate(zetels2=floor(perc*1.5))|>
  filter(zetels2>0)

while(sum(votes23$zetels2)<150){
  votes23 = votes23|>
    mutate(zetels3=zetels2+1,
    deler = votes/zetels3)|>
    arrange(-deler)|>
    mutate(zetels2=zetels2+ifelse(row_number()==1,1,0))
  message(votes23$partij[1])
}
