library(tidyverse)
library(igraph)
library(ggraph)
library(showtext)
library(cowplot)

showtext_auto()

font_add_google("Nunito", "Nunito")
myFont2 <- "Nunito"



lemurs <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv'
  )
tax <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv'
  )



lemur_df1 <- lemurs %>% left_join(., tax, by = "taxon") %>%
  mutate(common_name = str_to_title(common_name) %>% str_remove(" Lemur")) %>%
  select(common_name,
         taxon,
         dlc_id,
         sex,
         name,
         sire_name,
         dam_name,
         dob,
         litter_size) %>% distinct()

lemur_df2 <-
  lemur_df1 %>% select(sire_name, name, taxon) %>% filter(!is.na(sire_name))

lemur_OGG_df1 <- lemur_df2 %>% filter(taxon == 'OGG')

lemur_EMON_df1 <- lemur_df2 %>% filter(taxon == 'EMON')

lemur_HGG_df1 <- lemur_df2 %>% filter(taxon == 'HGG')

lemur_VVV_df1 <- lemur_df2 %>% filter(taxon == 'VVV')



OGG_graph <- graph_from_data_frame(lemur_OGG_df1)
EMON_graph <- graph_from_data_frame(lemur_EMON_df1)
HGG_graph <- graph_from_data_frame(lemur_HGG_df1)
VVV_graph <- graph_from_data_frame(lemur_VVV_df1)


theme_tidy <- function() {
  theme_void() +
    theme(
      text = element_text(family = myFont2),
      plot.background = element_rect(fill = "#009999", color = "#009999"),
      plot.title = element_text(color = 'white', size = 10),
      plot.caption = element_text(color = 'white'),
      legend.position = "none"
    )
}



OGG <- ggraph(OGG_graph, 'partition', circular = TRUE) +
  geom_node_arc_bar(aes(fill = depth), color = 'white', size = 0.25) +
  scale_fill_viridis(
    alpha = 1,
    direction = 1,
    discrete = FALSE,
    option = "A"
  ) +
  labs(title = "Northern Greater Galago") +
  coord_fixed() +
  theme_tidy()



EMON <- ggraph(EMON_graph, 'partition', circular = TRUE) +
  geom_node_arc_bar(aes(fill = depth), color = 'white', size = 0.25) +
  scale_fill_viridis(
    alpha = 1,
    direction = 1,
    discrete = FALSE,
    option = "A"
  ) +
  labs(title = "Mongoose") +
  coord_fixed() +
  theme_tidy()


HGG <- ggraph(HGG_graph, 'partition', circular = TRUE) +
  geom_node_arc_bar(aes(fill = depth), color = 'white', size = 0.25) +
  scale_fill_viridis(
    alpha = 1,
    direction = 1,
    discrete = FALSE,
    option = "A"
  ) +
  labs(title = "Eastern Lesser Bamboo") +
  coord_fixed() +
  theme_tidy()


VVV <- ggraph(VVV_graph, 'partition', circular = TRUE) +
  geom_node_arc_bar(aes(fill = depth), color = 'white', size = 0.25) +
  scale_fill_viridis(
    alpha = 1,
    direction = 1,
    discrete = FALSE,
    option = "A"
  ) +
  labs(title = "Black-And-White Ruffed") +
  coord_fixed() +
  theme_tidy()





plot_row <- plot_grid(OGG, EMON, HGG, VVV, ncol = 2) +
  theme(plot.background = element_rect(fill = "#009999", color = '#009999'))


title <- ggdraw() +
  draw_label(
    "Lemur family trees",
    fontface = 'bold',
    fontfamily = myFont2,
    color = 'white',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7),
    plot.background = element_rect(fill = "#009999", color = '#009999')
  )

caption <- ggdraw() +
  draw_label(
    "Duke Lemur Center Database | @nicci_potts | #TidyTuesday",
    fontfamily = myFont2,
    size = 8,
    color = 'white',
    x = 0.5,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7),
    plot.background = element_rect(fill = "#009999", color = '#009999')
  )
plot_grid(title,
          plot_row,
          caption,
          ncol = 1,
          rel_heights = c(0.1, 1))
