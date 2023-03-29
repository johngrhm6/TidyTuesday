library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggnetwork)
library(network)


font_families_google()
font_add_google(family = "Dokdo",name = "Dokdo")
showtext_auto()


colours<-met.brewer("Austria")
colours[1]

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')


net<-languages %>% 
  filter(title=="R") %>% 
  select(title,wikipedia_related) %>% 
  separate_longer_delim(cols = wikipedia_related,delim = " ") %>% 
  left_join(languages,by = c("wikipedia_related"="pldb_id")) %>% 
  separate_longer_delim(cols = wikipedia_related.y,delim = " ") %>% 
  select(wikipedia_related,wikipedia_related.y) %>%
  filter(!is.na(wikipedia_related)& !is.na(wikipedia_related.y)) %>% 
  network(loops = TRUE,directed = FALSE,multiple = TRUE) %>% 
  ggnetwork()

ggplot(data=net,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_edges()+
  geom_nodes()+
  theme_void()+
  geom_nodetext_repel(aes(label=vertex.names))







  

