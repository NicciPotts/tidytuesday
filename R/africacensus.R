remotes::install_github("afrimapr/afrilearndata")
library(afrilearndata)
library(tidyverse)
library(tmap)
library(showtext)
library(paletteer) 


#loading custom font
font_add_google('Poiret One', 'poiret')
showtext_auto()



#Changing year into a date but there was zero point!
africa_poly <- africountries %>%
  mutate(lastcensus = strptime(as.character(lastcensus), "%Y"),
         year = as.numeric(format(lastcensus, '%Y'))) %>%
  filter(!is.na(lastcensus))

#Picking out countries where census was before 2000 to label on map
africa_poly_name <- africa_poly %>%
  filter(year < 2000) %>%
  mutate(name2 = str_wrap(name, 5))



#Building polygons
map <- tm_shape(africa_poly) +
  tm_polygons(
    "year",
    labels = c(
      '1970 - 1980',
      '1980 - 1990',
      '1990 - 2000',
      '2000 - 2010',
      '2010 - 2020'
    ),
    palette = as.character(paletteer_d("fishualize::Ostracion_whitleyi")),
    lwd = .1,
    title = '',
    alpha = 0.6,
    border.col = 'gray30',
    border.alpha = 0.3
  ) 

#Adding text labels
map <-
  map + tm_shape(africa_poly_name) +
  tm_text(
    "name2",
    col = 'white',
    auto.placement = FALSE,
    #just = c('left', 'center'),
    size = 0.9
  ) 

#Making pretty
map +  tm_layout(
    frame = FALSE,
    bg.color = 'grey50',
    main.title = "Year of last census",
    main.title.color = 'white',
    main.title.size = 2,
    main.title.position = 'left',
    legend.bg.color = FALSE,
    legend.frame = FALSE,
    legend.title.color = 'white',
    legend.title.size = 1.3,
    fontfamily = 'poiret',
    legend.position = c("left", "bottom"),
    legend.text.color = 'white',
    legend.text.size = 0.9
  ) +
  tm_credits(
    "#TidyTuesday | @afrimapr | @nicci_potts",
    position = c("right", "bottom"),
    col = 'white',
    size = 0.8,
    align = 'right'
  )

