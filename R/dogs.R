library(tidyverse)
library(ggridges)
library(showtext)


#loading custom font
font_add_google('Reenie Beanie', 'reenie')
font_add_google('Shadows Into Light', 'shadows')
showtext_auto()


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

#removing white space from breed column
breed_traits <- breed_traits %>% mutate(Breed = str_squish(Breed))
breed_rank_all  <- breed_rank_all %>% mutate(Breed = str_squish(Breed)) %>% select(Breed, `2020 Rank`)


#arranging and slicing bottom 50 ranked dogs in 2020
dog_df_desc1 <- breed_rank_all %>%  
  arrange(desc(`2020 Rank`)) %>%
  slice(1:25) 

#arranging and slicing top 50 ranked dogs in 2020
dog_df_asc1 <- breed_rank_all %>%  
  arrange(`2020 Rank`) %>%
  slice(1:25) 


dog_df_desc2a <- dog_df_desc1 %>%  
  left_join(breed_traits, by='Breed') %>%
  select(-'Breed', -'Coat Type', -'Coat Length') %>%
  pivot_longer(-'2020 Rank',
               names_to = "trait", values_to ="value",values_drop_na = TRUE)


dog_df_asc2a <- dog_df_asc1 %>%  
  left_join(breed_traits, by='Breed') %>%
  select(-'Breed', -'Coat Type', -'Coat Length') %>%
  pivot_longer(-'2020 Rank',
               names_to = "trait", values_to ="value",values_drop_na = TRUE) %>%
  mutate(value = -value)
  

ggplot() +
  geom_boxplot(
    data = dog_df_desc2a,
    aes(y = trait, x = value),
    color = '#67605f',
    fill = '#d67573'
  ) +
  geom_boxplot(
    data = dog_df_asc2a,
    aes(y = trait, x = value),
    color = '#d67573',
    fill = '#67605f'
  ) +
  scale_x_continuous(
    breaks = c(-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5),
    labels = c(5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5),
    limits = c(-5, 5)
  ) +
  geom_vline(xintercept = 0,
             linetype = 'dotted') +
  annotate(
    'text',
    x = -4.75,
    y = 15,
    family = "shadows",
    color = "#67605f",
    size = 32,
    lineheight = 0.3,
    hjust = 0,
    vjust = 1,
    label = "Ranked top 25"
  ) +
  annotate(
    'text',
    x = 0.5,
    y = 15,
    family = "shadows",
    color = "#d67573",
    size = 32,
    lineheight = 0.3,
    hjust = 0,
    vjust = 1,
    label = "Ranked bottom 25"
  ) +
  labs(
    y = NULL,
    x = "trait ranking [1 (lowest) to 5 (highest)]",
    caption = "Source: American Kennel Club  |  #TidyTuesday week 5 |  @nicci_potts",
    title = "What makes an underdog?",
    subtitle = str_wrap(
      "Trait characteristics of the top and bottom 25 ranked dog breeds in 2020.
                      Where 1 is the worst ranking of the trait and 5 the best (except for 'drooling level'
                      and 'coat grooming frequency'). The top ranked dogs score highest for 'affectionate 
                      with family' and 'playfulness levels' with lower variablity in these categories. There 
                      are very little differenes in the other traits, suggesting affection and playfulness 
                      are the two most important factors in determining 'top dog'.",95
    )
  ) +
  theme_classic() +
  theme(
    text = element_text(
      face = "bold",
      family = 'shadows',
      color = 'black',
      size = 60
    ),
    plot.title = element_text(
      family = "reenie",
      hjust = 1,
      size = 200
    ),
    plot.subtitle  = element_text(
      #size = 50,
      lineheight = 0.3),
    axis.title = element_text(),
    panel.background = element_rect(fill = "#d0c8b6"),
    plot.background = element_rect(fill = "#d0c8b6")
  )

ggsave("dog.png", height = 17, width = 12)
