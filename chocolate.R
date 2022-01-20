library(tidyverse)
library(ggalt)
library(showtext)


#loading custom font
font_add_google('Reenie Beanie', 'reenie')
font_add_google('Shadows Into Light', 'shadows')
showtext_auto()

#The data
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate

#Dataframe for the number of ratings for each country of origin
ratings <-
  chocolate %>%
  group_by(country_of_bean_origin) %>%
  separate(
    col = ingredients,
    into = c("number_ingredients",
             "ingredients"),
    sep = "-"
  ) %>%
  count(number_ingredients) %>%
  tally(n) %>%
  rename("country" = country_of_bean_origin,
         "num_ratings" = n)


#Dataframe for ingredients and adding ratings number
choc_df1a <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  separate(
    col = ingredients,
    into = c("number_ingredients",
             "ingredients"),
    sep = "-"
  )  %>%
  mutate(number_ingredients = as.numeric(number_ingredients)) %>%
  summarise(
    ingredients = mean(number_ingredients, na.rm = TRUE),
    ingredients_max = max(number_ingredients, na.rm = TRUE),
    ingredients_min = min(number_ingredients, na.rm = TRUE)
  ) %>%
  rename("country" = country_of_bean_origin) %>%
  left_join(ratings, by = 'country') %>%
  mutate(country_new = paste0(country, " (", num_ratings, ")"),
         num_rat = num_ratings * 2) %>%
  filter(num_ratings > 1)




#The plot
ggplot(choc_df1a, aes(y = reorder(country, ingredients))) +
  geom_point(aes(x = ingredients_min), size = 6.2, fill = 'black') +
  geom_point(aes(x = ingredients_max), size = 6.2, fill = 'black') +
  geom_dumbbell(
    aes(x = ingredients_min, xend = ingredients_max),
    color = 'white',
    colour_x = '#715f83',
    size_x = 5,
    colour_xend = '#985954',
    size_xend = 5,
    inherit.aes = TRUE,
    show.legend = TRUE,
    alpha = 0.6
  ) +
  geom_point(aes(x = ingredients, size = num_rat),  color = 'black') +
  geom_point(aes(x = ingredients, size = num_ratings),  color = '#cebcc6') +
  #scale_size(name = 'number of ratings') +
  scale_size_continuous(name = 'number of ratings', range = c(4, 10)) +
  labs(
    x = 'number of ingredients',
    y = NULL,
    fill = NULL,
    title = "When is a bean not enough?",
    subtitle = str_wrap(
    "The number of ingredients added to cacao beans, showing minimum, maximum, and mean.
    The nunmber of ratings are shown as the mean radius size. Only beans from 3
    countries are used for pure cocoa, while all other beans have at least one 
    ingredient added to them.",
      100
    ),
    caption = "Source: Flavors of Cacao  |  #TidyTuesday week 3 |  @nicci_potts"
  ) +
  theme_classic() +
  theme(
    text = element_text(
      face = "bold",
      family = 'shadows',
      colour = "white",
      size = 60
    ),
    axis.text = element_text(size = 60, color = 'white'),
    axis.title = element_text(hjust = 1, size = 75),
    axis.line = element_line(color = 'white'),
    axis.ticks = element_line(color = 'white'),
    plot.title = element_text(
      size = 200,
      family = "reenie",
      hjust = 1
    ),
    plot.subtitle  = element_text(size = 50, lineheight = 0.3),
    panel.background = element_rect(fill = "#59353a"),
    plot.background = element_rect(fill = "#59353a"),
    legend.background = element_rect(fill = "transparent"),
    legend.position = 'bottom'
  )

ggsave("chocolate.png", height = 15, width = 12)
