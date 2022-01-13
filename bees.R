library(tidyverse)
library(broom)
library(rgeos)
library(showtext)
library(wesanderson)
library(MetBrewer)
library(scales)

#loading custom font
font_add_google('Mali', 'mali')
showtext_auto()

# hex map

# file from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
hex_outline <-
  geojson_read("us_states_hexgrid.geojson",  
               what = "sp")

hex_outline@data <- 
  hex_outline@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

hex_outline_fortified <- 
  tidy(hex_outline, region = "google_name")

centers <-
  cbind.data.frame(data.frame(gCentroid(hex_outline, byid = TRUE), 
                              id = hex_outline@data$iso3166_2))


# USDA data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')

stressor <- tuesdata$stressor


Mites <- stressor %>%
  group_by(year, state, stressor) %>%
  summarise(mean = mean(stress_pct, na.rm = TRUE)) %>%
  spread(stressor, mean) %>%
  select(`Varroa mites`) %>%
  ungroup() %>%
  spread(year, `Varroa mites`) %>%
  group_by(state) %>%
  mutate(diff = round(`2020` - `2015`, 1)) %>%
  select(state, diff, `2020`) %>%
  left_join(tibble(state = state.name, state_abb = state.abb), by = "state") %>%
  left_join(centers, by = c("state_abb" = "id"))

#Joining map and dataframe together
hex_mites_fortified <- hex_outline_fortified %>%
  left_join(. , Mites, by = c("id" = "state")) %>%
  mutate(
    `2020` = replace_na(`2020`, 0),
    `2020` = `2020` / 100,
    diff = replace_na(diff, 0),
    diff_pct = paste(diff, "%")
  )


#palette
honey_pal <-
  c("#ffffff",
    "#f9c901",
    "#f6e000",
    "#985b10",
    "#6b4701",
    "#896800")


#Making plot
ggplot() +
  geom_polygon(
    data = hex_mites_fortified,
    aes(
      fill =  `2020`,
      x = long,
      y = lat,
      group = id
    ),
    size = 2,
    color = 'black'
  ) +
  geom_polygon(
    data = hex_mites_fortified,
    aes(x = long, y = lat, group = id),
    size = 1,
    color = "#E3D7C1",
    fill = NA
  ) +
  
  
  scale_fill_gradientn(colours = honey_pal,
                       labels = percent) +
  
  geom_text(
    data = centers,
    aes(x = x, y = y, label = id),
    family = "mali",
    size = 24,
    fontface = "bold"
  ) +
  
  geom_text(
    data = hex_mites_fortified,
    aes(x = x, y = y - 1, label = diff_pct),
    family = "mali",
    size = 18,
    fontface = "bold"
  ) +
  
  geom_segment(
    aes(
      x = -129,
      y = 28,
      xend = -133.5,
      yend = 28
    ),
    color = "white",
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  
  annotate(
    "text",
    x = -128.5,
    y = 30,
    family = "mali",
    fontface = "italic",
    color = "white",
    size = 18,
    lineheight = 0.3,
    hjust = 0,
    vjust = 1,
    label = str_wrap("Change from 2015 to 2020", 8)
  ) +
  
  geom_segment(
    aes(
      x = -127,
      y = 53.5,
      xend = -129.5,
      yend = 53.5
    ),
    color = "white",
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  
  annotate(
    "text",
    x = -126.5,
    y = 54,
    family = "mali",
    fontface = "italic",
    color = "white",
    size = 18,
    lineheight = 0.3,
    hjust = 0,
    vjust = 1,
    label = str_wrap("No data available", 8)
  ) +
  
  labs(x = NULL,
       y = NULL,
       fill = str_wrap("2020 affected colonies",5),
       title= "Bee-aware of a mite-y problem", 
       subtitle= str_wrap("The plot shows the mean percentage of bee colonies affected by Varroa Mites
      in 2020 (hexagon color), and how this has changed since 2015 (number in hexagon). Most of the 
      states that have a high percentage of colonies affected by Varroa Mites have also seen an increase
      in the colony stress since 2015. ", 100),
       caption = "Source: USDA  |  #TidyTuesday Week 2 |  @nicci_potts") +

  theme_void() +
  
  theme(
    #axis.text = element_blank(),
        #panel.grid = element_blank(),
        #axis.ticks = element_blank(),
        text = element_text(family = "mali", colour = "white"),
        #strip.text = element_blank(),
        plot.title = element_text(size = 200),
        plot.subtitle  = element_text(size = 64, lineheight = 0.3),
        plot.caption = element_text(size = 64),
        panel.background = element_rect(colour = "#222222", fill = "#222222"),
        plot.background = element_rect(colour = "#222222", fill = "#222222"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        
        legend.position = "left",
        legend.text = element_text(size = 58, vjust = -1),
        legend.title = element_text(size = 64, face = "bold", lineheight = 0.3),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.size = unit(1.5, 'cm')
  ) +
  coord_map() +
  ggsave("bees.png", height = 12.25, width = 18)








