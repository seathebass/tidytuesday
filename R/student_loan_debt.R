library(tidyverse)
library(paletteer)
library(glue)
library(ggridges)
library(patchwork)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
cols_to_select <- loans %>% 
  summarise_all(list(missing = ~sum(is.na(.))/length(.))) %>% 
  select_if(vars(. < .01)) %>% 
  names()
loans_to_viz <- loans %>% 
  select(str_remove_all(cols_to_select,"_missing"))

l_to_viz <- loans_to_viz %>% 
  mutate(year1 = glue("20{year}")) 

l_to_viz %>%
  ggplot(aes(consolidation, wage_garnishments)) +
  geom_point(alpha = 0.3) +
  theme_minimal()


p1 <- l_to_viz %>%
  ggplot(aes(x = total, y = fct_rev(year1))) +
  geom_density_ridges(fill = paletteer_d("ghibli", "KikiLight")[4],
                      size = 0.25) +
  theme_minimal() +
  geom_curve(
    data = data.frame(x = 11 * 1e7, y = 4.5),
    aes(x, y, xend = 1e8, yend = 4.25),
    inherit.aes = F,
    curvature = .15,
    arrow = arrow(length = unit(3, "mm"))
  ) +
  annotate("text",
    x = 12 * 1e7, y = 4.5,
    label = glue(
      "The spread of 2015 is visibly different \n from other years"
    ),
    size = 2.5,
    lineheight = 0.7
  )+
scale_x_continuous(labels = scales::dollar) +
  labs(x = "Total Dollars Repaid",
       y = "",
       title = "Distribution of Total Dollars repaid \n by Students") +
  theme(plot.title = element_text(hjust = 0.5,size= 12),
        axis.title.x = element_text(size = 9))


p2 <- l_to_viz%>% 
  ggplot(aes(fct_rev(year1),wage_garnishments))+
  geom_violin(draw_quantiles = 0.5,fill = paletteer_d("ghibli","TotoroLight")[3],
              color = "black",size= 0.25)+
  geom_jitter(alpha = 0.5,width = 0.1,color = paletteer_d("ghibli","TotoroLight")[7])+
  scale_y_continuous(labels = scales::dollar)+
  labs(y = "Amount in Dollars",
       x = "",
        title = "Wage Garnishment by year")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size = 12),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 7))


plots <- p1 / 
  p2 +
  plot_annotation(title = "Exploring the Student Loan Crisis",
                          theme = theme(title = element_text(face = "bold")))
ggsave("tidy_tuesday_submission.jpg",plots)
