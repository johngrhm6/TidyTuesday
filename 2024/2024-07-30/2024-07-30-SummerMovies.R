library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(showtext)
library(ggbeeswarm)
library(ggtext)
library(paletteer)
library(ggdist)
library(colorspace)
library(fontawesome)
library(ggridges)


### Palette
cols <- paletteer::paletteer_c("grDevices::PinkYl",n = 10)


### Fonts
font <- font_add_google('Crimson Pro','Crimson Pro')

showtext_opts(dpi=320)
showtext_auto()

### Data

data <- tidytuesdayR::tt_load('2024-07-30')

movies <- data$summer_movies |> 
  left_join(data$summer_movie_genres)

movies <- movies |> 
  separate_longer_delim(genres,delim = ",")

medians <- movies |> 
  group_by(genres) |> 
  summarise(median = median(average_rating)) |> 
  arrange(median)

counts <- movies |> 
  group_by(genres) |> 
  summarise(count=n()) |> 
  arrange(desc(count))

movies <- movies |> 
  mutate(genres = factor(genres,levels=unique(medians$genres)))


movies <- movies |> 
  filter(!is.na(genres)) |> 
  filter(num_votes>50) 


cols
movies |> 
  filter(!genres %in% c("Film-Noir","Western")) |> 
  ggplot()+
  ggridges::geom_density_ridges_gradient(panel_scaling = T,
                                         aes(x=average_rating,
                                             y=genres,
                                             fill = ..x..),
                                         color="black" |> lighten(0.5))+
  scale_x_continuous(breaks = rep(0:10))+
  scale_fill_gradient(low = cols[6],high = cols[1])+
  labs(title = paste0("Distribution of Ratings per Genres"),
       subtitle = paste0("*Low level genres removed <br> Ordered by Median*"),
       caption = paste0("**Data**: IMDB<br>TidyTuesday: 30-07-2024"))+
  theme_minimal(base_family = "Crimson Pro",base_size = 16)+
  theme(plot.margin = margin(10,10,10,10),
        panel.spacing = unit(1,"lines"),
        legend.position = "none",
        axis.text = element_text(size = 10,face = "bold"),
        plot.title = element_textbox_simple(halign = 0.5,margin = margin(t=10,b=10),face = "bold",size = 16),
        plot.caption = element_textbox_simple(size = 10,margin = margin(t=10,b=10),lineheight = 1.2),
        plot.subtitle = element_textbox_simple(halign = 0.5,margin = margin(t=2,b=10),size = 12),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#FAF9F6",color = "#FAF9F6"),
        panel.background = element_rect(fill = "#FAF9F6",color = "#FAF9F6"))

ggsave(filename = 'TidyTuesday//2024-07-30/plot.png',width = 10,
       height = 7,units = "in",dpi = 320,device = "png")

