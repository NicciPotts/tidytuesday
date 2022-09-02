library(tidyverse)
library(tidycensus)
library(geofacet)
library(ggalt)
library(showtext)
library(paletteer) 
library(ggtext)


font_add_google('Nunito', 'nunito')
showtext_auto()


tuesdata <- tidytuesdayR::tt_load('2021-10-05')
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

nurses <- tuesdata$nurses

census_api_key("", install = TRUE)


#census data median incomes for 2018, 2015, 2009
income2018 <- get_acs(geography = "state", 
              variables = c(medincome = "B19013_001"), 
              year = 2018) %>%
              rename("estimate_2018" = estimate,
              "moe_2018" = moe)

income2015 <- get_acs(geography = "state", 
                      variables = c(medincome = "B19013_001"), 
                      year = 2015) %>%
              rename("estimate_2015" = estimate,
              "moe_2015" = moe)


income2009 <- get_acs(geography = "state", 
                      variables = c(medincome = "B19013_001"), 
                      year = 2009) %>%
              rename("estimate_2009" = estimate,
                     "moe_2009" = moe)

#Joining census data
census <- income2018 %>%
  left_join(income2015, by= c('NAME', 'GEOID', 'variable')) %>%
  left_join(income2009, by= c('NAME', 'GEOID', 'variable')) %>%
  rename("State" = NAME) %>% select(-variable)


#nurses filtering for 2008, 2015, 2019
nurses_df1 <- 
  nurses %>%
  filter(Year %in% c('2009', '2015', '2018'))

#joining census and nurses data together
nurses_df2 <- nurses_df1 %>% group_by(State, Year) %>%
  select(State, Year, `Annual Salary Median`) %>%
  spread(Year, `Annual Salary Median`) %>%
  left_join(census, by='State') %>%
  mutate(twenty09 = (`2009`-estimate_2009)/estimate_2009*100,
         twenty15 = (`2015`-estimate_2015)/estimate_2015*100,
         twenty18 = round((`2018`-estimate_2018)/estimate_2009*100,0)
         ) 
  
#removing island territories
nurses_df3 <- 
  nurses_df2 %>%
  filter(!State %in% c('Guam', 'Puerto Rico', 'Virgin Islands')) %>%
  select(State, twenty18)



#palette
pal2 = paletteer_d("Redmonder::dPBIPuOr")


#make map
 ggplot(nurses_df3) +
    geom_tile(aes(1, 1, fill = -twenty18)) +
    facet_geo( ~ State) +
    geom_text(aes(1, 1,
                  label = str_c(str_wrap(State,4),"\n", twenty18, "%")),
              family = "nunito",
              fontface = "bold",
              colour = "#04080A",
              size = 3) +
    scale_fill_gradientn(colors=pal2)  +
    labs(x = NULL,
         y = NULL,
         fill = NULL,
         title= "How do 2018 nurses salaries compare to their state median incomes?",
         subtitle = "nurses earn above median salaries in most states, except for Iowa, Maryland, New Hampshire, Utah, and Virginia.",
         caption = "Source: Data.World  |  #TidyTuesday |  @nicci_potts") +

    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(family = "nunito", face = "bold", colour = "white"),
          plot.subtitle  = element_text(size = 12, hjust= 1),
          strip.text = element_blank(),
          plot.title = element_text(size = 20, hjust=1),
          panel.background = element_rect(colour = "#222222", fill = "#222222"),
          plot.background = element_rect(colour = "#222222", fill = "#222222"),
          legend.position = "none",
          plot.margin = unit(c(1,1,1,1), "cm")
          ) +
    guides(fill = guide_legend(nrow = 1,
                               title.vjust = 0.5,
                               label.vjust = 0.5,
                               label.position = "bottom",
                               keyheight = 2,
                               keywidth = 2.5)) 
         
  

#data frame for geom-dumbbell plot  
  nurses_df4 <- 
    nurses_df2 %>% 
    filter(!State %in% 
             c('Guam', 'Puerto Rico', 'Virgin Islands')) %>%
    group_by(State) %>%
    select(State, twenty09, twenty18) 
  

#dumbbell plot
 ggplot(nurses_df4, aes(y=reorder(State,twenty18))) +
  geom_dumbbell(aes(x=twenty09, xend= twenty18),
                color = 'white',
                colour_x = '#7D4F73FF',
                size_x = 4,
                colour_xend = '#FEC0A3FF',
                size_xend = 4,
                inherit.aes = TRUE,
                show.legend = TRUE) +
   scale_x_continuous(
     labels = function(x) paste0(x, "%")) +
    labs(x= 'proportional salary difference',
         y= NULL,
         title = "How do nurses salaries compare to their state <br> median incomes in 
         <span style='color:#7D4F73FF;'><b>2009<b></span>
         and <span style='color:#FEC0A3FF;'><b>2018<b></span>?
         </span>",
         subtitle = "the states where nurses earn below or near median 
         salry have seen relative decreases since 2009.",
         caption = "Source: Data.World  |  #TidyTuesday |  @nicci_potts") +
    theme_classic() +
    theme(
          text = element_text(family = "nunito", face = "bold", colour = "white"),
          axis.text = element_text(color = 'white'),
          axis.title = element_text(hjust=1),
          axis.line = element_line(color = 'white'),
          axis.ticks = element_line(color = 'white'),
          plot.title = element_markdown(size= 20, hjust=1),
          plot.subtitle  = element_text(size = 12, hjust= 1),
          panel.background = element_rect(colour = "#222222", fill = "#222222"),
          plot.background = element_rect(colour = "#222222", fill = "#222222"),
          plot.margin = unit(c(1,1,1,1), "cm"))

  
  
  
  
