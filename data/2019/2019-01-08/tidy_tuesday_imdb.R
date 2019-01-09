library(tidyverse)
library(here)
library(ggforce)
library(ggthemes)
library(paletteer)
imdb_ratings_econ <- read_csv(here("tidytuesday","data","2019","2019-01-08","IMDb_Economist_tv_ratings.csv"))
imdb_ratings_econ %>% 
  map_if(is.character,~unique(.x))

crime_shows <- imdb_ratings_econ %>% 
  filter(str_detect(genres , pattern = "Crime")) 

crime_shows%>% 
  ggplot(aes(x = date, y = av_rating))+
  geom_point(aes(col = genres),show.legend = T)+
  guides(col = guide_legend(title = "Genre"))+
  geom_hline(aes(yintercept = quantile(av_rating,(1/4))),linetype = "dashed",col = "red")+
  geom_hline(aes(yintercept = quantile(av_rating,(3/4))),linetype = "dashed",col = "green")+
  scale_color_manual(values = paletteer_d("ggsci","category20_d3")[1:14])+
  labs(title = "Average Ratings of Crime-Related Shows",
       subtitle =  "During the Golden Age of TV",
       x = "Average Rating",
       y = "Date Aired")+
  theme_minimal()+
  geom_text(aes(x= lubridate::mdy("01-01-1990"),y = quantile(av_rating,(1/4))-.1,label = "Shows with an Average Rating below the 25th Perrcentile"),
            col = "red",check_overlap = T)+
  geom_text(aes(x= lubridate::mdy("01-01-1990"),y = quantile(av_rating,(3/4))+.1,label = "Shows with an Average Rating Above the 75th Perrcentile"),
            col = "green",check_overlap = T)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,face = "bold",size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.background = element_rect(fill = "grey20"),
        text = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(colour = "white"),
        panel.grid.major.y = element_line(linetype = "longdash"))


