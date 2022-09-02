library(tidyverse)
library(lubridate)
library(showtext)


font_add_google('Montserrat', 'montserrat')
showtext_auto()


tuesdata <- tidytuesdayR::tt_load('2021-09-14')
tuesdata <- tidytuesdayR::tt_load(2021, week = 38)

billboard <- tuesdata$billboard
audio <- tuesdata$audio_features


df2 <- billboard %>%
  mutate(date=mdy(week_id),
         year= year(date)) %>%
  select(song_id, year) %>%
  left_join(audio, by='song_id') %>%
  distinct()%>%
  drop_na(spotify_track_popularity) %>%
  select(song_id, year, performer, spotify_track_popularity) %>% 
  group_by(song_id)%>%
  arrange(spotify_track_popularity)

df3 <- df2 %>% group_by(song_id)




ggplot(df3, aes(x= year, y= spotify_track_popularity, group= year)) +
  geom_boxplot(fatten = 2, lwd=0.5) +
  scale_x_continuous(breaks=seq(1958, 2021, 10)) +
labs(x = NULL,
     y = 'Spotify track popularity',
     title = " Is music getting better?",
     subtitle= "Spotify track popularity based on year of release",
     caption = "Source: Data.World | #TidyTuesday | @nicci_potts") +
  theme_classic()+
  
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        text = element_text(color = 'white', family = 'montserrat'),
        plot.title = element_text(hjust = 1, size = 18),
        plot.subtitle = element_text(hjust = 1, size = 10),
        axis.title = element_text(size = 14),
        axis.line = element_line(color = 'white', size = 1.7),
        axis.text = element_text(size=12, color= 'white', family= 'montserrat'),
        axis.ticks = element_line(color = 'white')) 
