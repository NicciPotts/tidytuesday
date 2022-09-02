library(tidyverse)
library(showtext)


tuesdata <- tidytuesdayR::tt_load('2021-09-07')
tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

circuits <- tuesdata$circuits
results <- tuesdata$results
races <- tuesdata$races
drivers <- tuesdata$drivers
driverStandings <- tuesdata$driver_standings

lapTimes <- tuesdata$lap_times
pitStops <- tuesdata$pit_stops

qualifying <- tuesdata$qualifying



font_add_google('Racing Sans One', 'racing')
showtext_auto()

  

df1 <- pitStops %>%
  left_join(races, by = "raceId") %>%
  left_join(drivers, by = "driverId") %>%
  select(
    raceId,
    driverId,
    stop,
    lap,
    time.x,
    duration,
    year,
    round,
    circuitId,
    name,
    date,
    driverRef,
    code,
    surname,
    dob,
    nationality
  )

df1$duration <- as.numeric(df1$duration)


df2 <- df1 %>% group_by(name, year) %>%
  filter(name != '70th Anniversary Grand Prix') %>%
  summarise(stop = min(duration, na.rm = TRUE)) %>%
  mutate(name = str_remove(name, "Grand Prix"))
year = as.factor(year))

df2 %>% group_by(year) %>% arrange(stop) %>% slice(1:1)

df3 <-
  df2 %>% group_by(name) %>% arrange(stop) %>% slice(1:1) %>% ungroup() %>%
  count(year)


ggplot(df3, aes(x = year, y = n)) +
  geom_line(color = 'white',
            size = 2,
            linetype = 2) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  labs(
    x = NULL,
    y = 'count of locations shortest pit stop times',
    color = NULL,
    title = 'Grand Prix pit stop times',
    subtitle = 'count of locations where the year had the fastest pit stop times.',
    caption = "Ergast API | #TidyTuesday |  @nicci_potts"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#BD162C"),
    plot.background = element_rect(fill = "#BD162C"),
    text = element_text(color = 'white', family = 'racing'),
    plot.title = element_text(size = 20, hjust = 1),
    plot.subtitle = element_text(hjust = 1),
    axis.title = element_text(size = 16),
    axis.line = element_line(color = 'white', size = 1.7),
    axis.text = element_text(
      size = 12,
      color = 'white',
      family = 'racing'
    ),
    axis.ticks = element_line(color = 'white'),
    legend.position = 'bottom',
    legend.background = element_rect(fill = "transparent"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 12)
  )



