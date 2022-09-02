library(tidyverse)
library(tmap)
library(rgdal)
library(sf)
library(rmapshaper)
library(cowplot)
library(showtext)


font_add_google('Karma', 'karma')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-08-31')
tuesdata <- tidytuesdayR::tt_load(2021, week = 36)

bird_baths <- tuesdata$bird_baths



#Australia bioregion shapefiles
#http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B2CD8B271-A723-4B1B-8D26-9C68B1974B45%7D

#New South Wales
NSWLayer <- st_read("data/ibra61_reg_nsw_shape.shp", layer= "ibra61_reg_nsw_shape") 
NSWLayer <- st_transform(NSWLayer, "+init=epsg:4326") 
SimpleNSW <- ms_simplify(NSWLayer) 

#Victoria
VICLayer <- st_read("data/ibra61_reg_vic_shape.shp", layer= "ibra61_reg_vic_shape") 
VICLayer <- st_transform(VICLayer, "+init=epsg:4326") 
SimpleVIC <- ms_simplify(VICLayer) 

#South Australia
SALayer <- st_read("data/ibra61_reg_sa_shape.shp", layer= "ibra61_reg_sa_shape") 
SALayer <- st_transform(SALayer, "+init=epsg:4326") 
SimpleSA <- ms_simplify(SALayer) 


#Queensland
QLDLayer <- st_read("data/ibra61_reg_qld_shape.shp", layer= "ibra61_reg_qld_shape") 
QLDLayer <- st_transform(QLDLayer, "+init=epsg:4326") 
SimpleQLD <- ms_simplify(QLDLayer) 



#Joining shapefiles
Bioregions <- rbind(SimpleNSW, SimpleVIC, SimpleSA, SimpleQLD)


###Bird bath wrangling

# renaming bioregions column name to match shapefiles
bird_df_rename <- bird_baths %>% rename("REG_NAME" = bioregions)


#Number of birds/bioregion in 2015
bird_2014 <- bird_df_rename %>%
  group_by(REG_NAME) %>%
  filter(survey_year == '2015') %>%
  count(bird_count) %>% tally(n)


#Merging shapefiles and bird_bath figures
bird_map <- merge(Bioregions, bird_2014, by= 'REG_NAME')




#map
map <- tm_shape(bird_map) +
  tm_polygons(
    col = "n",
    palette = 'Purples',
    title = '',
    lwd = .25,
    border.col = 'gray20'
  ) +
  tm_text("REG_CODE",
          size= "AREA")+
  tm_style("natural") +
  tm_layout(
    frame = FALSE,
    bg.color = '#009999',
    " bird bath \n sightings - \n 2015",
    title.color = 'white',
    title.size = 3,
    legend.bg.color = TRUE,
    legend.frame = FALSE,
    fontfamily = 'karma',
    legend.position = c("left", "bottom"),
    legend.text.color = 'white',
    legend.text.size = 0.7
  ) +
  tm_credits(
    "Cleary et al. 2016| @nicci_potts| #TidyTuesday",
    position = c("right", "bottom"),
    col = 'white',
    size = 0.5,
    align = 'right'
  )

map



#plot data wranging
plot_df1 <- bird_baths %>% 
  group_by(bioregions, urban_rural) %>% 
  filter(survey_year == '2015') %>%
  count(bird_type) %>% tally(n) %>% spread(urban_rural, n) %>%
  mutate(Rural = -Rural) %>%
  gather(Rural, Urban, key='type', value='count')


#plot build
plot <- ggplot(plot_df1, aes(x=count, y= bioregions, fill= type )) +
  geom_bar(stat = 'identity') +
  labs(fill = NULL,
       y= NULL,
       x= 'number of sightings',
       title = " 2015 bird bath sightings by bioregion",
       caption = "Cleary et al. 2016| @nicci_potts| #TidyTuesday") +
  scale_fill_manual(values = alpha(c('#A60A55', '#675E3C'), alpha = 0.8)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#009999"),
        plot.background = element_rect(fill = "#009999"),
        text = element_text(color = 'white', family = 'karma'),
        plot.title = element_text(hjust = 1,
                                  size = 18),
        axis.title = element_text(size = 14),
        axis.line = element_line(color = 'white', size = 1.7),
        axis.text = element_text(size=12, color= 'white', family= 'karma'),
        axis.ticks = element_line(color = 'white'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.9, .2),
        legend.background = element_rect(fill="transparent"),
        legend.title.align = 0.5) 



