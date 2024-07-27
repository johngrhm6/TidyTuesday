library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(geomtextpath)
library(shadowtext)
library(sysfonts)
library(paletteer)
library(colorspace)

### Palette

cols <- paletteer_d("palettesForR::Pastels")

### Font

font_add_google(name = "Lato",family = "Lato")
showtext::showtext_auto()
showtext::showtext_opts(dpi=600)

### Data

data <- tidytuesdayR::tt_load(tidytuesdayR::last_tuesday())
ratings <- data$ratings
ratings <- ratings |> rowid_to_column(var = "id")

### Wrangling
min_max <-  ratings |> 
  group_by(season) |> 
  filter(viewers_in_millions==min(viewers_in_millions,na.rm = T)) |> 
  select(season,min_viewers=viewers_in_millions) |> 
  bind_cols(ratings |> 
              group_by(season) |> 
              filter(viewers_in_millions==max(viewers_in_millions,na.rm = T)) |> 
              select(max_viewer=viewers_in_millions) |> 
              distinct()) |> 
  mutate(season=season...1) |> 
  select(-season...1,-season...3) |> 
  mutate(season=as.factor(season))

min_max <- min_max |> 
  mutate(middle=(max_viewer+min_viewers)/2)



### Plot
  ggplot()+
    geom_segment(data = min_max,aes(x=season,xend = season,y = min_viewers,yend =  max_viewer),alpha = 0.3)+
    geom_point(data = min_max,aes(x=season,y = min_viewers,color = season),shape = 17,size = 3)+
    scale_color_manual(values = cols)+
    geom_point(data = min_max,aes(x=season,y = max_viewer,color = season),shape = 18,size = 3)+
    scale_color_manual(values = cols)+
    annotate(geom = "shadowtext",x = 9,y=35,label="Simon Cowell Leaves",size=3,hjust=0,
             bg.color=cols[5],color="black")+
    scale_x_discrete()+
    scale_y_continuous(breaks = c(5,10,15,20,25,30,35),
                       name = paste0("Viewers in Millions"))+
    theme_minimal(base_family = "Lato")+
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x  = element_blank(),
        axis.title.y = element_text(margin = margin(t=10,b=10,l = 10,r=10),face = "bold"),
        plot.margin = margin(l=20,r = 20,t = 20,b = 20),
        plot.title = element_text(hjust = 0.5,size = 20,margin = margin(b=20,t=20)),
        panel.background = element_rect(fill = "#FAF9F6",color = NA),
        plot.background = element_rect(fill = "#FAF9F6",color = NA),
        plot.caption = element_markdown(family = "Lato",margin = margin(b = 10,t=10,r = 10,l=10),
                                        size = 8,lineheight = 1.4,linewidth = 5,halign =0 ,hjust = 0))+
  labs(title = "Season Viewers",
       caption = paste0("Source:**TidyTuesday** <br>Data: **American Idol**"))

ggsave(filename = "tt.png",dpi = 600,width = 9,height = 7,units = "in")
