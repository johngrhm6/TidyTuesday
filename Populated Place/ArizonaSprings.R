library(tidyverse)
library(rnaturalearth)
library(ggmap)
library(sf)
library(ggtext)
library(mapdata)
library(showtext)





#Add Fonts
font_add_google("Major Mono Display","mon")

showtext_auto()

map_bg<-"#ccd5ae"
panel_bg<-"#edede9"
text_col<-"#dda15e"

# Filter Map for AZ

az<-map_data("state") %>% 
  filter(region=="arizona")

# Tidytuesday Data

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')


# Wrangle

az_data<-us_place_names %>% filter(state_name=="Arizona") %>%
  drop_na() %>% 
  filter(grepl("spring",feature_name,ignore.case = TRUE)) %>% 
  arrange(desc(prim_lat_dec)) %>% 
  rowid_to_column(var = "id") %>% 
  mutate(y_text=seq(37,min(prim_lat_dec),length= length(id)))



# Plot


map<-az_data %>% 
  ggplot()+
  geom_polygon(aes(long,lat),az,fill=map_bg,color="black",linewidth=0.2)+
  geom_point(data = az_data,aes(prim_long_dec,prim_lat_dec),alpha=0.6,shape=4,color=text_col,size=8)+
  geom_text(data = az_data,aes(-118,y_text,label=paste(id,feature_name)),size=16,vjust=1,hjust=-0.001,color=text_col)+
  geom_text(data = az_data,aes(prim_long_dec,prim_lat_dec,label=id),size=16,vjust=-1,hjust=0.5,check_overlap = TRUE,color=text_col,fontface="bold")+
  annotate("text", x = -118, y = 30, label = "subtitle", family = "mon", size = 24, colour = "black", hjust = 0, vjust = 1, lineheight = 0.35) +
  theme_void()+
  coord_map()+
  labs(title = "Arizona")+
  theme(text = element_text(family = "mon"),
        plot.background = element_rect(fill = panel_bg,color = NA),
        panel.background = element_rect(fill=panel_bg,colour = NA),
        plot.title =element_text(family = "mon",face = "bold",hjust = 0.5,vjust = -1,size=100))



ggsave("./ArizonaSprings.png",dpi = 300,width = 12,height = 12)


