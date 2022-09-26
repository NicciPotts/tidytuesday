library(tidyverse)
library(janitor)
library(ggtext)
library(showtext)

#Custom font loading
font_add_google('Mulish', 'mulish')
font_add_google(name = "Baloo 2", family = "baloo2")
showtext_auto()

#loading data
tuesdata <- tidytuesdayR::tt_load('2022-09-20')
HydroWASTE_v10 <- tuesdata$HydroWASTE_v10


# filtering to UK only
uk_df1 <-
  HydroWASTE_v10 %>% 
  clean_names() %>% 
  filter(country == 'United Kingdom',
         !is.na(river_dis))

# dilution factor below 10
uk_df2 <-
  uk_df1 %>% 
  filter(df <10)

# dilution factor between 10 and 20
uk_df3 <-
  uk_df1 %>% 
  filter(df >= 10 & df <= 20)

# getting UK polygon for plotting
world <- map_data("world") %>% 
  filter(region == "UK") 

# title for plot
plot_title <- "Wastewater treatment plants"
# subtitle for plot
plot_subtitle <- "All <span style='color:#5D7A64'><b>wasterwater treatment plants</b></span> in the UK. Plants with a dilution factor<br>
<span style='color:#571415'><b>below 10</b></span> are considered of environmental concern. Where dilution factor<br> is the 
ratio of the natural discharge of the receiving waterbody to the plant<br> effluent discharge.
Plants with a dilution factor <span style='color:#FF5201'><b>between 10 and 20</b></span> are <br> also highlighted as a factor of waste discharge."

# plot
p <-
  ggplot() +
  # UK outline
  geom_polygon(data = world,
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = '#E2C896',
               color = 'white') +
  # all WWTP
  geom_point(data = uk_df1,
             aes(x = lon_wwtp, 
                 y = lat_wwtp),
             color = '#5D7A64',
             alpha = 0.5,
             stroke = 0.2) +
  # WWTP with df < 10
  geom_point(data = uk_df2,
             aes(x = lon_wwtp, 
                 y = lat_wwtp,
                 size = waste_dis),
             shape = 21,
             stroke = 0.7,
             fill = '#571415',
             color = '#23313A') +
  # WWTP with df 10:20
  geom_point(data = uk_df3,
             aes(x = lon_wwtp, 
                 y = lat_wwtp,
                 size = waste_dis),
             shape = 21,
             stroke = 0.8,
             fill = '#FF5201',
             color = '#23313A') +
  
  labs(title = plot_title,
      subtitle = plot_subtitle,
       size = "waste discharge",
       caption = "Source: Marcedo et al, 2022 | #TidyTuesday week 38 | @nicci_potts") +
  
  theme_void() +
  
  theme(legend.position = 'right', 
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#23313A', color = '#23313A'),
        plot.background = element_rect(fill = '#23313A', color = '#23313A'),
        plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), unit = "cm"),
        
        text = element_text(color = '#F5EDD9', family = 'mulish', size = 28),
        
        plot.title = element_markdown(color = '#F5EDD9', family = 'mulish', size = 60),
        plot.subtitle = element_markdown(color = '#F5EDD9', family = 'mulish', size = 28, lineheight = 0.5)) 


ggsave("wasterwater.png", 
       p, 
       device = "png", 
       width = 6, 
       height = 8, 
       units = "in", 
       dpi = 300)