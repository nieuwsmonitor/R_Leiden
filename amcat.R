install.packages("remotes")
remotes::install_github("ccs-amsterdam/amcat4r")
remotes::install_github('ccs-amsterdam/annotinder-r')

library(annotinder)
library(amcat4r)
library(tidyverse)
library(lubridate)

amcat4r::amcat_login("https://amcat4.labs.vu.nl/amcat")


query = c("biodiversiteit*")

data = amcat4r::query_documents("dutch_news_media", queries = query,
                         fields = c('_id', 'publisher','date','section','title', 'text'),
                         max_pages = Inf, scroll='5m')



data2 = data|>
  select(.id, publisher, date)|>
  mutate(medtype=case_when(publisher %in% c("ad","telegraaf") ~ "Populaire dagbladen",
                           publisher %in% c("NU.nl", "NOS.nl") ~ "Internet",
                           T ~ "Kwaliteitskranten"))|>
  mutate(month = floor_date(date, 'month'),
         week = floor_date(date, 'week'))


data2|>
  group_by(medtype,month)|>
  summarise(n=n())|>
  ggplot( aes(x=month, y=n, group=medtype, color=medtype)) +
  geom_line()
  
###UITDAGING: allemaal leuk en aardig maar logisch dat kwaliteitskranten meer zijn, dat zijn ook meer verschillende soorten kranten.
#Kun je een figuur maken waar het beter representatief is?
#Ook leuk, kun je een figuur maken voor aantal per mediumtype
#en misschien ook nog een per medium



#### CREATING CODINGJOBS

data$newrow <- sample(100, size = nrow(data), replace = TRUE)
table(data$newrow)

data3=data|>
  filter(newrow==2)

data3

units = create_units(data3, id = '.id', set_text('title', title), set_text('text', text)) 
class(units)


uncertainty = question('Scientific uncertainty', 'Gaat deze tekst over Scientific uncertainty?', codes = c('Ja', 'Nee'))
economic = question('Economic consequence', 'Gaat deze tekst over Economic consequencey?', codes = c('Ja', 'Nee'))
pandora = question('Pandora’s box', 'Gaat deze tekst over Pandora’s box?', codes = c('Ja', 'Nee'))
accountability = question('Public accountability', 'Gaat deze tekst over Public accountabilityy?', codes = c('Ja', 'Nee'))
biocentric = question('Biocentric', 'Gaat deze tekst over biocentric?', codes = c('Ja', 'Nee'))

codebook = create_codebook(uncertainty=uncertainty,economic=economic,pandora=pandora,accountability=accountability,biocentric=biocentric)

# Job uploaden naar de server
annotinder::backend_connect("https://uva-climate.up.railway.app", username="nelruigrok@nieuwsmonitor.org", .password = "test")
jobid = annotinder::upload_job("biodiversity", units, codebook)

# Coderen
url = glue::glue('https://uva-climate.netlify.app/?host=https%3A%2F%2Fuva-climate.up.railway.app&job_id={jobid}')
print(url)
browseURL(url)


todo = seq(189,189,1)
artcodings = list()

for(id in todo) {
  message("* Getting job ",id, " (", length(artcodings)+1, "/", length(todo), ")")
  c = download_annotations(id)
  if (is.null(c)) next
  artcodings[[as.character(id)]] = c
}

#hieronder wordt alles aan elkaar gekoppeld
artcodings = dplyr::bind_rows(artcodings)

table(artcodings$variable)


table(artcodings$unit_id %in% data$.id)

data2=data2|>
  mutate(doc_id = as.character(.id))

arts = artcodings|>
  rename(doc_id = unit_id)|>
  left_join(data2)

arts

#UITDAGING: Kun je leuk figuur maken van de gecodeerde data?