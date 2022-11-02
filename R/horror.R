library(tidyverse)
library(showtext)
library(ggtext)


#custom fonts
font_add_google('Jolly Lodger', 'jolly')
font_add_google('Creepster', 'creepster')
font_add_google('Reenie Beanie', 'reanie')
showtext_auto()

#data import
tuesdata <- tidytuesdayR::tt_load('2022-11-01')
horror_movies <- tuesdata$horror_movies

#dataframe
df_plot <-
  horror_movies %>% 
  filter(vote_count >0,
         budget >0)
  

#build plot
ggplot() +
  geom_point(data = df_plot,
             aes(x = vote_average,
                 y = budget,
                 size = revenue),
             color = '#880808') +
  
#reverse axis for drip affect & sensible to read numbers
  scale_y_reverse(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) +
  
  #labels
  labs(
    x = 'average vote',
    y = 'budget ($)',
    size = 'revenue ($)',
    title = 'Horror Movies',
    subtitle = "The realm of the low budget horror movie isn't \nas popular or profitable as expected",
    caption = "Source: Tanya Shapiro's Horror Movies | #TidyTuesday week 44 | @nicci_potts"
  ) +
  
  #customise theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'black', color = 'black'),
    plot.background = element_rect(fill = 'black', color = 'black'),
    axis.ticks = element_line(color = 'white'),
    
    
    text = element_text(
      color = 'white',
      family = 'jolly',
      size = 16
    ),
    axis.text = element_text(color = 'white', size = 16, family = 'reanie'),
    axis.title = element_text(hjust = 1, size = 20),
    plot.title = element_text(
      family = 'creepster',
      color = '#880808',
      size = 40
    ),
    plot.subtitle = element_text(
      family = 'jolly',
      color = 'white',
      size = 18
    ),
    legend.position = 'bottom',
    legend.background = element_blank(),
    legend.key = element_blank()
  )
