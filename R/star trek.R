library(tidyverse)
library(showtext)
library(jsonlite)
library(textclean)
library(wordcloud2)
library(tm)
library(tidytext)
library(ggridges)
library(wesanderson)
library(textdata)
library(plotly)
library(networkD3)

tuesdata <- tidytuesdayR::tt_load('2021-08-17')
tuesdata <- tidytuesdayR::tt_load(2021, week = 34)

computer <- tuesdata$computer

view(computer)

computer %>% group_by(type) %>% count(type) %>% tally(n)


df1 <- computer %>% 
  mutate(char2 = if_else(str_starts(char, "Computer"), "Computer", computer$char))

view(df1)








tidy_comp <- computer %>% group_by(char_type) %>%
  unnest_tokens(word, line) 

tc2 <- tidy_comp %>% anti_join(stop_words) %>%
  count(word, sort = TRUE)

nrc <- get_sentiments("nrc")
  
 nrc_df <- tc2 %>%
   inner_join(nrc) %>% group_by(char_type, sentiment) %>%
   count(word, sort = TRUE) %>% tally(n)
 

 nodes <- data.frame(
   name=c(as.character(nrc_df$char_type), 
          as.character(nrc_df$sentiment)) %>% unique()
 )
 

 nrc_df$IDsource <- match(nrc_df$char_type, nodes$name) - 1
 nrc_df$IDtarget <- match(nrc_df$sentiment, nodes$name) - 1
 
 
 node_color <-
   'd3.scaleOrdinal() .domain(["Node_0", "Node_1", "Node_2", "Node_3", "Node_4",
"Node_5", "Node_6", "Node_7", "Node_8", "Node_9"]) .range(["#000000", "#d6a444", "#2b53a7" ,
"#000000", "#c1c730", "#2b53a7", "#a71313", "#c1c730", "#2b53a7", "#a71313"])'
 
 p <- sankeyNetwork(
   Links = nrc_df,
   Nodes = nodes,
   Source = "IDsource",
   Target = "IDtarget",
   Value = "n",
   NodeID = "name",
   sinksRight = FALSE,
   colourScale = node_color,
   fontFamily = 'fantasy',
   fontSize = 14
 )
 
 p
 


