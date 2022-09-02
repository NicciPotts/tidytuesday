library(tidyverse)
library(showtext)


font_add_google('Gothic A1', 'gothica1')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
tuesdata <- tidytuesdayR::tt_load(2021, week = 39)

nominees <- tuesdata$nominees


view(nominees)


df1 <- separate(data = nominees, col = production, into = c("name", "role"), sep = "\\,")

df2 <- separate(data = df1, col = name, into = c("first", "surname"), sep = "\\ " )


view(df2)


df3 <- df2 %>% group_by(year, first) %>% filter(type == 'Winner',
                                   !is.na(first)) %>% count(title) %>% tally(n) %>%
  arrange(desc(n)) %>% filter(n >8) 



ggplot(df3, aes(x= n, y= first, color= year)) +
  geom_point()


df4 <- df2 %>% group_by(distributor) %>% 
  filter(type == 'Winner') %>% count(title) %>% tally(n) %>% arrange(desc(n))



dist <- df2 %>% filter(distributor %in% c('HBO', 'NBC', 'CBS', 'ABC', 'Netflix',
                                        'FOX', 'Comedy Central', 'FX Networks', 
                                        'PBS', 'Cartoon Network')) %>%
  group_by(year, distributor, type) %>% count(title) %>% tally(n) %>%
  spread(type, n) %>%
  mutate(Winner = replace_na(Winner, 0),
         ratio = Winner/Nominee)

distyear2 <- dist %>% ungroup() %>%
  group_by(distributor) %>%
arrange(desc(ratio)) %>% slice(1:1)


distyear <- dist %>% filter(year %in% c('1980', '1990', '2020'))


p1 <- ggplot(dist, aes(x= ratio, y= distributor)) +
  geom_point(color= '#A48111', size = 7, shape= 'I', alpha= 0.5) +
  geom_point(data = distyear2, aes(x= ratio, y= distributor), color='#A48111',
             size = 8) +
  geom_text(data = distyear2, aes(x= ratio, y= distributor, label= year), 
            size = 2, color= 'black') +
  labs(y= NULL,
       x= 'wins/nominations',
       title= 'All time top 10 distributors of Emmy wins',
       caption = "Source: Emmys.com | #TidyTuesday | @nicci_potts") +
  theme_classic() +
  
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        text = element_text(color = '#A48111', family = 'gothica1'),
        plot.title = element_text(hjust = 1, size = 18),
        plot.subtitle = element_text(hjust = 1, size = 12),
        axis.title = element_text(size = 14, hjust= 1),
        axis.line = element_line(color = '#A48111', size = 0.5),
        axis.text = element_text(size=12, color= '#A48111', family= 'gothica1'),
        axis.ticks = element_line(color = '#A48111')) 


df5 <- df2 %>% group_by(year) %>% filter(type == 'Winner') %>% count(category) %>% tally(n) %>%
  left_join(df4, by= 'year')


p2 <- ggplot(df5, aes(x= n, y= factor(year))) +
  geom_bar(stat = 'identity', fill='#A48111', alpha = 0.7) +
  scale_y_discrete(breaks=seq(1957, 2021, 4)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL,
       x = 'number of categories',
       title= "Is it getting easier to win an Emmy?",
       caption = "Source: Emmys.com | #TidyTuesday | @nicci_potts") +
  theme_classic() +
  
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        text = element_text(color = '#A48111', family = 'gothica1'),
        plot.title = element_text(hjust = 1, size = 18),
        plot.subtitle = element_text(hjust = 1, size = 12),
        axis.title = element_text(size = 14, hjust=1),
        axis.line = element_line(color = '#A48111', size = 0.7),
        axis.text = element_text(size=12, color= '#A48111', family= 'gothica1'),
        axis.ticks = element_line(color = '#A48111')) 


cowplot::plot_grid(p1, p2)
