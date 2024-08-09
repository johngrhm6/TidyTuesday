library(tidyverse)
library(countrycode)
library(ggtext)
library(showtext)
library(ggforce)
library(fontawesome)



font_add_google(name = "Rubik",family = "Rubik")
font_add("fa",regular = 'TidyTuesday/fontawesome-free-6.6.0-web/webfonts/fa-regular-400.ttf')
git  <- "&#xf09b"

showtext_auto()
showtext_opts(dpi=320)




data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv')

# Data Prep
codes <- countrycode::codelist 
codes <- codes |> 
  select(genc3c,country.name.en)

data <- data |> 
  filter(str_detect(games,"Summer"),!is.na(medal)) |> 
  left_join(codes ,by=c("noc"="genc3c")) |> 
  mutate(country.name.en=case_when(is.na(country.name.en)&noc=="GER"~"Germany",
                                   is.na(country.name.en)&noc=="GRE"~"Greece",
                                   is.na(country.name.en)&noc=="DEN"~"Denmark",
                                   is.na(country.name.en)&noc=="SUI"~"Switzerland",
                                   is.na(country.name.en)&noc=="MAS"~"Malaysia",
                                   is.na(country.name.en)&noc=="NED"~"Netherlands",
                                   is.na(country.name.en)&noc=="BAH"~"Bahamas",
                                   is.na(country.name.en)&noc=="POR"~"Portugal",
                                   TRUE~country.name.en)) |> 
  mutate(country.name.en =ifelse(is.na(country.name.en),team,country.name.en)) |> 
  mutate(country.name.en=str_remove_all(country.name.en,"-[:digit:]{1}"))
  
## Top 10 Countries

top_countries <- data |> 
  filter(str_detect(games,"Summer"),!is.na(medal)) |> 
  group_by(country.name.en) |> 
  summarise(n=n()) |> 
  arrange(desc(n)) |> 
  slice_head(n = 10) |> 
  select(country.name.en) |> 
  pull()

## Merge Russia & Soviet Union
top_countries <- c(top_countries,"Russia")  

## Get Summer Years
summer_games_years <- data |> filter(str_detect(games,"Summer")) |> select(year) |> distinct()

plot_data <- data |> 
  filter(str_detect(games,"Summer"),!is.na(medal),country.name.en %in% top_countries) |> 
  mutate(country.name.en=ifelse(country.name.en %in% c("Russia","Soviet Union"),"Russia/Soviet Union",
                                country.name.en)) |> 
  group_by(country.name.en,
           year,
           medal) |> 
  summarise(n=n(),.groups = "drop") |> 
  ungroup() |> 
  arrange(year) |> 
  group_by(country.name.en) |> 
  mutate(cum_sum_medal=cumsum(n)) |> 
  ungroup() |> 
  mutate(top_three=ifelse(country.name.en %in% c("Russia/Soviet Union","United Kingdom","United States"),TRUE,FALSE)) 


  ggplot()+
  geom_step(data=plot_data, aes(x=year,
                y=cum_sum_medal,
                colour = country.name.en,size=top_three,alpha = top_three),
            show.legend = F)+
  
  
  geom_text(data = plot_data %>%
              group_by(country.name.en) %>%
              filter(cum_sum_medal == max(cum_sum_medal)) %>%
              ungroup() |> 
              arrange(desc(cum_sum_medal)) |> 
              slice_head(n=3),
            aes(x = year, 
                y = cum_sum_medal, 
                label = paste(country.name.en,cum_sum_medal,sep = "\n")),
            hjust = 0.6, vjust = 0.7,nudge_y = 500,check_overlap = T)+
  labs(title = "Olympic Medal per Country",
       subtitle = "Data filtered for the top 10 countries <br> Top 3 highlighted",
       caption = paste0("**Data**: Sports Reference | TidyTuesday |",
                        "<span style='font-family:\"fa\";'>{git};</span>"))+
  
  
  scale_x_continuous(limits = c(min(summer_games_years$year),
                                max(summer_games_years$year)+4),
                     breaks = summer_games_years$year)+
  scale_y_continuous(position = "right")+
  scale_color_manual(values = c("Russia/Soviet Union"="#FFD700",
                                "United Kingdom"="#012169",
                                "United States"="#B31942"),
                     na.value = "#cce3de")+
  scale_size_manual(values = c("TRUE"=0.7,"FALSE"=0.5),
                    na.value = 0.1)+
  scale_alpha_manual(values = c("TRUE"=1,"FALSE"=0.4))+
  theme_minimal(base_family = "Rubik")+
  theme(axis.title  = element_blank(),
        plot.title = element_textbox(hjust = 0.5,size = rel(2),maxwidth = 0.8),
        plot.background = element_rect(fill = '#fafafa'),
        plot.caption = element_textbox(hjust  = 0,margin = margin(20,10,10,10)),
        plot.subtitle = element_textbox(
          hjust = 0.5,
          halign = 0.5,
          maxwidth = 0.6,
          size = rel(1)),
        panel.grid = element_blank(),
        plot.margin = margin(20,20,20,20),
        axis.text.x = element_text(angle = 90))

  
  ggsave(filename = 'TidyTuesday/2024-08-06/Olympics.png',dpi = 320,height = 7,width = 10,units = "in")
  