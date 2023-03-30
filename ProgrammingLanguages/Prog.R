library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggnetwork)
library(network)
library(ggdark)
library(gghighlight)


font_families_google()
font_add_google(family = "Dokdo",name = "Dokdo")
showtext_auto()


colours<-met.brewer("Austria")

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')


file<-languages %>% 
  filter(github_repo_subscribers>100) %>% 
  filter(github_repo_issues>1 & github_repo_subscribers>1) %>% 
  ggplot()+
  geom_point(aes(github_repo_subscribers,github_repo_issues),color="#39ff14",size=1)+
  scale_x_log10()+
  scale_y_log10()+
  dark_theme_minimal(base_family = "Dokdo",base_size = 40)+
  gghighlight(github_repo_issues> quantile(github_repo_issues,0.75)+1.5*IQR(github_repo_issues)|
                github_repo_issues< quantile(github_repo_issues,0.25)-1.5*IQR(github_repo_issues)|
                github_repo_issues> quantile(github_repo_issues,0.75)+1.5*IQR(github_repo_issues),
              label_key = title,label_params = list(family="Dokdo",fill="#39ff14",size=15),max_highlight = 20)+
  theme(panel.grid = element_blank(),
        axis.title = element_text(color = "#39ff14"))+
  labs(title = "Github Subscribers & Github Repo Issues Relationship")


ggsave(filename = "Programming-Languages.png",width =3000,height = 1600,units = "px",dpi = 320,plot = file)
?ggsave()



  

