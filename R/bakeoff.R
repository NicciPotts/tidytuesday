library(tidyverse)
library(bakeoff)
library(ggtext)
library(showtext)

#Custom font loading
font_add_google('Indie Flower', 'indieflower')
font_add_google('Permanent Marker', 'marker')
showtext_auto()



#create the series average
series_avg <-
  bakeoff::ratings %>%
  group_by(series) %>% 
  summarize(series_avg = mean(viewers_7day))


#create the dataframe to build plot
plot_df1 <-
  bakeoff::ratings %>%
  mutate(ep_id = row_number()) %>%
  group_by(series) %>% 
  select(series,ep_id, episode, viewers_7day) %>% 
  left_join(series_avg, by = 'series') %>% #joining series mean data to calculate variation
  mutate(episode_diff = viewers_7day - series_avg)

#create labels for series
series_labels <- plot_df1 %>% 
  group_by(series) %>% 
  summarize(x_position = mean(ep_id),
            ep_min = min(ep_id),
            ep_max = max(ep_id)) %>% 
  mutate(label = paste('series', series))


#build plot
p1 <-
ggplot() +
  #data columns
  geom_col(data = plot_df1,
           aes(x = ep_id, y = episode_diff, 
               fill = (factor(series))),
           color = 'white') +
  
  #series labels beginning and end point
  geom_point(data = series_labels,
             aes(x = ep_min,
                 y = -2.5,
                 color = (factor(series))),
             shape = 'I',
             size = 6) +
  geom_point(data = series_labels,
             aes(x = ep_max,
                 y = -2.5,
                 color = (factor(series))),
             shape = 'I',
             size = 5) +
  
  #series labels text
  geom_text(data = series_labels, 
            aes(label = label,
                x = x_position, 
                y = -2.5,
                color = (factor(series))),
            size = 3.5,
            family = 'indieflower') +
  
  #plot title and caption text
  labs(title = 'Great British Bake Off',
       subtitle = 'How does an episodes 7 day viewings vary from the series mean?',
       caption = "Source: {bakeoff} | #TidyTuesday week 43 | @nicci_potts") +
  
  #using {bakeoff} colors
  scale_fill_bakeoff(guide = "none") +
  scale_color_bakeoff(guide = "none") +
  
  #series 2-4 annotation
  annotate('text',
           x = 16.5,
           y = 1.8,
           angle = 20,
           hjust = 0.5,
           size = 3,
           label = 'series 2 to 4 gained more\n7 day viewings for the second\npart of their series.',
           family = 'indieflower') +
  
  geom_curve(
    aes(x = 10, 
        y = 1.5, 
        xend = 8, 
        yend = 1),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90 
  ) +
  
  geom_curve(
    aes(x = 26, 
        y = 1.9, 
        xend = 30, 
        yend = 1.7),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = -0.3
  ) +
  
  #series 5-6 annotation
  annotate('text',
           x = 47,
           y = -1.5,
           angle = 10,
           hjust = 0.5,
           size = 3,
           label = 'series 5 & 6 7 day viewings\nare all over the place.',
           family = 'indieflower') +
  
  geom_curve(
    aes(x = 41, 
        y = -1.6, 
        xend = 36, 
        yend = -1.5),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = -0.3
  ) +
  
  
  geom_curve(
    aes(x = 55, 
        y = -1.4, 
        xend = 55, 
        yend = -1.1),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = 0.3
  ) +
  
  #series 7 annotation
  annotate('text',
           x = 66,
           y = 2.7,
           angle = -10,
           hjust = 0.5,
           size = 3,
           label = 'series 7 finale was\nreally really popular.',
           family = 'indieflower') +
  
  geom_curve(
    aes(x = 68, 
        y = 2.45, 
        xend = 65, 
        yend = 2.2),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = -0.3
  ) +
  
  #series 8 - 10 annotation
  annotate('text',
           x = 80,
           y = 1.3,
           angle = -3,
           hjust = 0.5,
           size = 3,
           label = 'since moving (series 8 - 10) viewers are\nmore likely to watch first & last episodes\nwithin 7 days (compared to other episodes).',
           family = 'indieflower') +
  
  geom_curve(
    aes(x = 67, 
        y = 1.3, 
        xend = 65.5, 
        yend = 0.9),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = 0.3
  ) +
  
  geom_curve(
    aes(x = 92, 
        y = 1.2, 
        xend = 94, 
        yend = 0.9),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type = 'open',
    ),
    size = 0.7,
    angle = 90,
    curvature = -0.3
  ) +
  
  #customising theme
  theme(axis.text= element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor= element_blank(),
        
        panel.background = element_rect(fill = '#faf0e6', color = '#faf0e6'),
        plot.background = element_rect(fill = '#faf0e6', color = '#faf0e6'),
        
        text = element_text(family = 'indieflower'),
        plot.title = element_text(family = 'marker', size = 32),
        plot.subtitle = element_text(family = 'indieflower', size = 18))
 p1


# ggsave("bakeoff.png", 
#        p1, 
#        device = "png", 
#        width = 9, 
#        height = 8, 
#        units = "in", 
#        dpi = 300)