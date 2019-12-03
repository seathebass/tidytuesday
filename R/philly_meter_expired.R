library(ggspatial)
library(tidyverse)
library(paletteer)
library(rayshader)
library(sf)
library(tidycensus)
library(gganimate)
library(lubridate)
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
philly <- get_acs(geography = "tract",county = "Philadelphia",state = "PA",variables = "B19013_001",
                  geometry = T)
# just getting an overview of what we got in terms of categorical data
tickets %>% 
  select_if(is.character)%>% 
  map_if(is.character,~tibble(x = .) %>% 
           count(.,x) %>% 
           arrange(desc(n)))
# getting all data which contains an entry of meter expired
meter_expired <- tickets %>% 
  filter(str_detect(violation_desc,"METER EXPIRED")) %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(4269)
philly_meter <- st_join(philly,meter_expired)
philly_meter_counts <- philly_meter %>% 
  count(NAME)
# get the bounding box
box <- st_bbox(philly_meter_counts) %>% 
  enframe() %>% 
  spread(name,value)

philly_meter_counts %>%
  ggplot() +
  geom_sf(aes(fill = n),color = "white",size = 0.2) +
  scale_fill_paletteer_c("scico", "tokyo") +
  geom_curve(
    data = data.frame(
      x = -75.1,
      y = 39.95,
      x2 = -75.15,
      y2 = 39.95
    ),
    aes(
      x = x,
      y = y,
      xend = x2,
      yend = y2
    ),
    arrow = arrow(length = unit(0.25, "cm")),color = "grey20"
  ) +
  theme_void() +
  geom_text(
    data = data.frame(x = -75.05, y = 39.943),
    aes(x, y, label = "Area around Center City has the \n highest  number of tickets issued"),
    size = 2.5,lineheight = 0.75
  )+
  guides(fill = guide_legend(title = "Total"))+
  geom_rect(data= box,aes(xmin = xmin - .05,ymin = ymin-.01,xmax = xmax+ .01,ymax = ymax+.01),fill= "transparent",color = "black")+
  coord_sf(expand = T)+
  labs(title = "Total number of Meter Expiration Tickets in Philadelphia",
       subtitle = "In 2017")+
  annotation_north_arrow(style = north_arrow_fancy_orienteering(),location = "tl",pad_x = unit(0.5,"cm"),
                         pad_y = unit(0.75,"cm"))+
  theme(plot.title = element_text(hjust = 0.5,family = "Times"),
        plot.subtitle = element_text(hjust= 0.5,family = "Times"),
        legend.position = c(.87,0.2),
        plot.background = element_rect(fill = "grey80"),
        panel.background = element_rect(fill = "grey80"),
        strip.background = element_rect(fill= "grey80"))

ggsave("all_meter.png")


# Day 2 of code -----------------------------------------------------------
364 *12
philly_tib <- philly %>% as_tibble()
philly_meter_month <- philly_meter %>% 
  mutate(months1 = fct_explicit_na(lubridate::month(issue_datetime,label = T,abbr = FALSE))) %>% 
  count(months1,NAME,.drop = F) %>% 
  complete(months1,NAME,fill = list(n = 0)) %>% 
  st_as_sf() %>% 
  mutate(months1 = fct_recode(months1, "January" ="(Missing)")) 



test <- full_join(philly_meter_month,philly_tib,by = "NAME") 
palettes_c_names
test %>% 
  as_tibble() %>% 
  select(-geometry.x) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n),color = "white",size = 0.2) +
  scale_fill_paletteer_c("gameofthrones", "lannister",labels = scales::comma) +
  theme_void() +
  geom_rect(data= box,aes(xmin = xmin - .05,ymin = ymin-.01,xmax = xmax+ .01,ymax = ymax+.01),fill= "transparent",color = "black")+
  coord_sf(expand = T)+
  labs(title = "Number of Meter Expiration Tickets in Philadelphia",
       subtitle = "In {current_frame}",
       fill = "Total")+
  annotation_north_arrow(style = north_arrow_fancy_orienteering(),location = "tl",pad_x = unit(0.5,"cm"),
                         pad_y = unit(0.75,"cm"))+
  theme(plot.title = element_text(hjust = 0.5,family = "Times",face = "bold"),
        plot.subtitle = element_text(hjust= 0.5,family = "Times"),
        legend.position = c(.87,0.2),
        plot.background = element_rect(fill = "grey80"),
        panel.background = element_rect(fill = "grey80"),
        strip.background = element_rect(fill= "grey80"),
        legend.title = element_text())+
  transition_manual(months1)
anim_save("monthly_meter.gif")
