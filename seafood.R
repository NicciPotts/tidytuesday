library(tidyverse)
library(janitor)
library(showtext)


#custom fonts
font_add_google('Staatliches', 'Staatliches')
font_add_google('Jost', 'jost')
showtext_auto()


#data import
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')


#cleaning column names, filtering ouot world
stock_df1 <- stock %>%
  clean_names() %>%
  group_by(year, entity) %>%
  filter(entity != 'World') %>%
  mutate(year = as.factor(year))

#2017 data
stock_df1_17 <- stock_df1 %>% filter(year == '2017')

#2015 data
stock_df1_15 <- stock_df1 %>% filter(year == '2015')

#Building plot
plot <- ggplot() +
  geom_col(data = stock_df1_15,
           aes(
             y=share_of_fish_stocks_that_are_overexploited, 
             x = reorder(str_wrap(entity, 10), share_of_fish_stocks_that_are_overexploited),
             fill= year
           ),
           position = "dodge2",
           show.legend = TRUE,
           width= 0.65,
           alpha = .9
  ) +
  geom_col(data = stock_df1_17,
    aes(
      y=share_of_fish_stocks_that_are_overexploited,
      x = reorder(str_wrap(entity, 10), share_of_fish_stocks_that_are_overexploited),
      fill= year
    ),
    position = "dodge2",
    show.legend = TRUE,
    width = 0.45,
    alpha = .9
  ) +
  scale_y_continuous(
    limits = c(-10, 70),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("#1E3F66", "#73A5C6"))


#Adding y axis labels
plot <-
  plot + 
  annotate(
    x = 1, 
    y = 20, 
    label = "20 %", 
    geom = "text", 
    color = "gray12", 
    family = "jost",
    size = 3
  ) +
  annotate(
    x = 1, 
    y = 40, 
    label = "40 %", 
    geom = "text", 
    color = "gray12", 
    family = "jost",
    size = 3
  ) +
  annotate(
    x = 1, 
    y =60, 
    label = "60 %", 
    geom = "text", 
    color = "gray12", 
    family = "jost",
    size = 3
  ) 

#Setting theme
plot +
   labs(
    x= NULL,
    y= NULL,
    fill = NULL,
    title = "\nShare of fish stocks that are overexploited",
    subtitle = paste(
      "\nRegional breakdown of fish stocks that are overexploited (with the remaining percentage 
      representing sustainable fishing). Data for 2015 and 2017. The most overexploited
        regions are the Mediterranean and Black Sea, Southeast Pacific, and Southwest 
       Atlantic. The regions to show the biggest increases in overexploitation are the 
       Northwest Pacific, and Northwest Central Atlantic.
      "
    ),
    caption = "Source: OurWorldinData.org  |  #TidyTuesday |  @nicci_potts") +
  theme_minimal() +
  theme(
    text = element_text(family = 'jost', color = "gray12", size=12),
    plot.title = element_text(face = "bold", size = 18, hjust = 1, family= 'Staatliches'),
    plot.subtitle = element_text(size = 10, hjust = 1),
    plot.caption = element_text(size = 10, hjust = 1),
    panel.background = element_rect(fill = "#FFFEF2", color = "#FFFEF2"),
    plot.background = element_rect(fill = "#FFFEF2", color = "#FFFEF2"),
    axis.text.y = element_blank(),
    legend.position = 'top'

  ) +
  coord_polar() 






