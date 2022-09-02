library(tidyverse)
library(plotly)
library(rvest)
library(ggridges)
library(wesanderson)
library(viridis)
library(showtext)



tuesdata <- tidytuesdayR::tt_load('2021-08-03')
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)

athletes <- tuesdata$athletes




core_url <- "https://db.ipc-services.org/sdms/hira/web/competition/code/PG2016/sport/AR"

"body > div.container.bg-white.border.border-header.border-top-0.pb-3 > table > tbody > tr:nth-child(1) > td:nth-child(7) > div > a"

raw_html <- "https://db.ipc-services.org/sdms/hira/web/competition/code/PG2016" %>% 
  read_html()


# Archery -----------------------------------------------------------------


clean_arrow <- function(year){
  
  arrows <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/AR") %>% 
    read_html()
  
  raw_arrow <- arrows %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>% 
    rename(event = Event) %>% 
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    )
  
  ind_arrow <- raw_arrow %>% 
    separate(athlete, into = c("athlete", "abb"), sep = " \\(") %>% 
    separate(event, into = c("gender", "event"), sep = "'s | ", fill = "left", extra = "merge") %>% 
    mutate(abb = str_remove(abb, "\\)")) %>% 
    filter(str_detect(event, "Mixed|Team", negate = TRUE)) 
  
  team_arrow <- raw_arrow %>% 
    filter(str_detect(event, "Mixed|Team")) %>% 
    separate(athlete, into = c("country", "abb"), sep = " \\(") %>% 
    separate(abb, into = c("abb", "athlete"), sep = "\\)") %>% 
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>% 
    group_by(event, medal, abb) %>% 
    mutate(grp_id = row_number()) %>% 
    ungroup()
  
  bind_rows(ind_arrow, team_arrow) %>% 
    mutate(type = "Archery", year = year) 
}

clean_arrow(2008) %>% 
  print(n = 100)

year_vec <- seq(1980, 2016, by = 4)

try_arrow <- safely(clean_arrow)

all_arrow <- year_vec %>% 
  map(try_arrow) 

arrow_years <- all_arrow %>% 
  map_dfr("result")

# athletics ---------------------------------------------------------------

sport_vec <- raw_html %>% 
  html_nodes("td:nth-child(7) > div > a") %>% 
  html_attr("href") %>% 
  str_remove("/sdms/hira/web/competition/code/PG2016/sport/")


clean_ath <- function(year){
  
  ath <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/AT") %>% 
    read_html()
  
  raw_ath <- ath %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>% 
    rename(event = Event) %>% 
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>% 
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge")
  
  raw_ind <- raw_ath %>% 
    filter(str_detect(event, "4x", negate = TRUE)) %>% 
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    separate(abb, into = c("abb", "guide"), sep = "\\)") %>% 
    mutate(guide = str_remove(guide, "Guide: "),
           guide = if_else(guide == "", NA_character_, guide)) %>% 
    filter(!is.na(athlete)) 
  
  clean_ind <- raw_ind %>% 
    add_row(
      raw_ind %>% 
        filter(str_detect(guide, "\\(")) %>% 
        select(gender:medal, athlete = guide) %>% 
        separate(athlete, c("athlete", "abb"), sep = " \\(")
    ) %>% 
    filter(!is.na(athlete)) %>% 
    mutate(guide = if_else(str_detect(guide, "\\("), NA_character_, guide)) 
  
  
  clean_grp <- raw_ath %>% 
    filter(str_detect(event, "4x")) %>% 
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>% 
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>% 
    separate_rows(athlete, sep = "(?<=[A-Z]\\.)(?=[A-Z])") %>% 
    mutate(
      guide = if_else(
        str_detect(lead(athlete), "Guide"), 
        lead(athlete),
        NA_character_
      )
    ) %>% 
    filter(str_detect(athlete, "Guide", negate = TRUE)) %>%
    group_by(event, medal, country) %>% 
    mutate(grp_id = row_number()) %>% 
    ungroup()
  
  bind_rows(clean_ind, clean_grp) %>% 
    mutate(type = "Athletics", year = year)
  
}

test_df <- clean_ath(1980) 
test_df %>% 
  filter(str_detect(event, "4x60")) %>% 
  print(n = 100)

safe_ath <- safely(clean_ath)

ath_years <- year_vec %>% 
  map(safe_ath) %>% 
  map_dfr("result")

# Cycling -----------------------------------------------------------------

clean_cycle <- function(year){
  
  url_c <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/CY") %>% 
    read_html()
  
  raw_cycle <- url_c %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>% 
    rename(event = Event) %>% 
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>% 
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge")
  
  raw_ind_c <- raw_cycle %>% 
    filter(str_detect(event, "Team|Tandem", negate = TRUE)) %>% 
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    separate(abb, into = c("abb", "pilot"), sep = "Pilot: |\\(PT\\):") %>% 
    separate(athlete, sep = "(?<=[PT\\)])(?=[A-Z])", into = c("athlete", "pilot")) %>%
    separate(athlete, sep = "(?<=[a-z]+)(?=[A-Z])", into = c("athlete", "pilot")) %>%
    mutate(pilot = if_else(pilot == "", NA_character_, pilot)) %>% 
    filter(!is.na(athlete)) 
  
  clean_ind_c <- raw_ind_c %>% 
    add_row(
      raw_ind_c %>% 
        filter(str_detect(pilot, "\\(")) %>% 
        select(gender:medal, athlete = pilot) %>% 
        separate(athlete, c("athlete", "abb"), sep = " \\(")
    ) %>% 
    filter(!is.na(athlete)) %>% 
    mutate(pilot = if_else(str_detect(pilot, "\\("), NA_character_, pilot)) %>% 
    mutate(abb = str_remove(abb, "\\)"))
  
  
  clean_grp_c <- raw_cycle %>% 
    filter(str_detect(event, "Team")) %>% 
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>% 
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>% 
    mutate(
      guide = if_else(
        str_detect(lead(athlete), "Pilot|PT\\)"), 
        lead(athlete),
        NA_character_
      )
    ) %>% 
    filter(str_detect(athlete, "Pilot|PT\\)", negate = TRUE)) %>%
    group_by(event, medal, country) %>% 
    mutate(grp_id = row_number()) %>% 
    ungroup()
  
  bind_rows(clean_ind_c, clean_grp_c) %>% 
    mutate(year = year, type = "Cycling") %>% 
    rename(guide = pilot)
  
}

safe_cycle <- safely(clean_cycle)

cycle_years <- year_vec %>% 
  map(safe_cycle) %>% 
  map_dfr("result")


# Powerlifting ------------------------------------------------------------


clean_power <- function(year){
  
  raw_power <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/PO") %>% 
    read_html()
  
  clean_pwr <- raw_power %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>% 
    rename(event = Event) %>% 
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>% 
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>% 
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    separate_rows(abb, sep = " \\)") 
  
  clean_abb_power <- clean_pwr %>%
    mutate(abb = str_remove(abb, "\\)")) %>%
    bind_rows(clean_pwr %>%
                filter(str_detect(abb, "\\(")) %>%
                separate_rows(abb, sep = "\\(") %>%
                mutate(athlete = ifelse(str_length(abb) <= 4, lag(abb), athlete)) %>%
                mutate(
                  abb = str_remove(abb, "\\).*"),
                  athlete = str_remove(athlete, ">*\\)") %>% str_trim()
                )) %>%
    filter(str_detect(abb, "\\(", negate = TRUE), !is.na(athlete)) %>% 
    mutate(abb = str_remove(abb, "\\)"))
  
  
  clean_abb_power %>% 
    mutate(year = year, type = "Powerlifting")
}

clean_power(1984)

safe_power <- safely(clean_power)

power_years <- year_vec %>% 
  map(safe_power) %>% 
  map_dfr("result")


# Swimming ----------------------------------------------------------------

clean_swim <- function(year){
  
  raw_swim <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/SW") %>%
    read_html()
  
  raw_ind_sw <- raw_swim %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "4x", negate = TRUE)) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>% 
    separate_rows(athlete, sep = "(?<=[A-Z]\\))(?=[A-Z])") %>%
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge")
  
  
  clean_ind_sw <- raw_ind_sw %>%
    mutate(abb = str_remove(abb, "\\)")) %>%
    bind_rows(raw_ind_sw %>%
                filter(str_detect(abb, "\\(")) %>%
                separate_rows(abb, sep = "\\(") %>%
                mutate(athlete = ifelse(str_length(abb) <= 4, lag(abb), athlete)) %>%
                mutate(
                  abb = str_remove(abb, "\\).*"),
                  athlete = str_remove(athlete, ">*\\)") %>% str_trim()
                )) %>%
    filter(str_detect(abb, "\\(", negate = TRUE), !is.na(athlete))
  
  
  clean_grp_sw <- raw_swim %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "4x")) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  bind_rows(clean_ind_sw, clean_grp_sw) %>% 
    mutate(year = year, type = "Swimming") 
  
}

safe_swim <- safely(clean_swim)

swim_years <- year_vec %>% 
  map(safe_swim) %>% 
  map_dfr("result")



# Table Tennis ------------------------------------------------------------

clean_tab_tennis <- function(year){
  
  raw_tab <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/TT") %>%
    read_html()
  
  clean_ind_tab <- raw_tab %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Team", negate = TRUE)) %>%
    separate(
      event,
      into = c("gender", "event"),
      sep = " |'s ",
      fill = "left",
      extra = "merge"
    ) %>%
    separate_rows(athlete, sep = "(?<=[\\)])(?=[A-Z])") %>%
    separate(
      athlete,
      into = c("athlete", "abb"),
      sep = " \\(",
      fill = "left",
      extra = "merge"
    ) %>%
    mutate(abb = str_remove(abb, "\\)"))
  
  
  clean_grp_tab <- raw_tab %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Team")) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    separate_rows(athlete, sep = "(?<=[A-Z]\\.)(?=[A-Z])") %>% 
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  bind_rows(clean_ind_tab, clean_grp_tab) %>% 
    mutate(year = year, type = "Table Tennis") 
  
}

safe_table <- safely(clean_tab_tennis)

table_years <- year_vec %>% 
  map(safe_table) %>% 
  map_dfr("result")

check_fun(table_years)

# Volleyball --------------------------------------------------------------



clean_volleyball <- function(year){
  
  raw_vb <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/VO") %>%
    read_html()
  
  clean_grp_vb <- raw_vb %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    separate_rows(athlete, sep = "(?<=[A-Z]\\.)(?=[A-Z])") %>% 
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  clean_grp_vb %>% 
    mutate(year = year, type = "Volleyball")
  
}

safe_vb <- safely(clean_volleyball)

vb_years <- year_vec %>% 
  map(safe_vb) %>% 
  map_dfr("result")

check_fun(vb_years)

# Basketball --------------------------------------------------------------

clean_basketball <- function(year){
  
  raw_bb <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/WB") %>%
    read_html()
  
  clean_grp_bb <- raw_bb %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  clean_grp_bb %>% 
    mutate(year = year, type = "Basketball")
  
}

safe_bb <- safely(clean_basketball)

bb_years <- year_vec %>% 
  map(safe_bb) %>% 
  map_dfr("result")

check_fun(bb_years)

# Fencing -----------------------------------------------------------------

clean_fencing <- function(year){
  
  raw_f <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/WF") %>%
    read_html()
  
  clean_ind_f <- raw_f %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Team", negate = TRUE)) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    mutate(abb = str_remove(abb, "\\)"))
  
  clean_grp_f <- raw_f %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Team")) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    separate_rows(athlete, sep = "(?<=[A-Z]\\.)(?=[A-Z])") %>% 
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  bind_rows(clean_ind_f, clean_grp_f) %>% 
    mutate(year = year, type = "Fencing")
  
}

safe_fence <- safely(clean_fencing)

fence_years <- year_vec %>% 
  map(safe_fence) %>% 
  map_dfr("result")

check_fun(fence_years)


# Rugby -------------------------------------------------------------------

clean_rugby <- function(year){
  
  raw_rug <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/WR") %>%
    read_html()
  
  clean_grp_rug <- raw_rug %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)"),
      event = paste(event, "Wheelchair Rugby")
    ) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    separate_rows(athlete, sep = "(?<=[A-Z]\\.)(?=[A-Z])") %>% 
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>% 
    ungroup()
  
  clean_grp_rug %>% 
    mutate(year = year, type = "Rugby")
  
}

safe_rugby <- safely(clean_rugby)

rugby_years <- year_vec %>% 
  map(safe_rugby) %>% 
  map_dfr("result")

check_fun(rugby_years)


# Tennis ------------------------------------------------------------------

clean_tennis <- function(year){
  
  raw_ten <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/TT") %>%
    read_html()
  
  clean_ind_ten <- raw_ten %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Double|Team", negate = TRUE)) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate_rows(athlete, sep = "(?<=[\\)])(?=[A-Z])") %>%
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    mutate(abb = str_remove(abb, "\\)"))
  
  
  clean_grp_ten <- raw_ten %>%
    html_table() %>%
    .[[2]] %>%
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>%
    rename(event = Event) %>%
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>%
    filter(str_detect(event, "Double|Team")) %>%
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge") %>%
    separate(athlete, into = c("country", "abb", "athlete"), sep = " \\(|\\)") %>%
    separate_rows(athlete, sep = "(?<=[a-z])(?=[A-Z])") %>%
    group_by(event, medal, country) %>%
    mutate(grp_id = row_number()) %>%
    ungroup()
  
  bind_rows(clean_ind_ten, clean_grp_ten) %>% 
    mutate(year = year, type = "Wheelchair Tennis")
  
}

safe_tennis <- safely(clean_tennis)

tennis_years <- year_vec %>% 
  map(safe_tennis) %>% 
  map_dfr("result")

check_fun(tennis_years)

# Triathlon ---------------------------------------------------------------

clean_triathlon <- function(year){
  
  tri_url <- glue::glue("https://db.ipc-services.org/sdms/hira/web/competition/code/PG{year}/sport/TR") %>% 
    read_html()
  
  raw_tri <- tri_url %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_longer(names_to = "medal", values_to = "athlete", cols = -Event) %>% 
    rename(event = Event) %>% 
    mutate(
      medal = str_remove(medal, " Medallist\\(s\\)")
    ) %>% 
    separate(event, into = c("gender", "event"), sep = " |'s ", fill = "left", extra = "merge")
  
  raw_ind_tri <- raw_tri %>% 
    filter(str_detect(event, "4x", negate = TRUE)) %>% 
    separate(athlete, into = c("athlete", "abb"), sep = " \\(", fill = "left", extra = "merge") %>% 
    separate(abb, into = c("abb", "guide"), sep = "\\)") %>% 
    mutate(guide = str_remove(guide, "Guide: "),
           guide = if_else(guide == "", NA_character_, guide)) %>% 
    filter(!is.na(athlete)) 
  
  clean_ind_tri <- raw_ind_tri %>% 
    add_row(
      raw_ind_tri %>% 
        filter(str_detect(guide, "\\(")) %>% 
        select(gender:medal, athlete = guide) %>% 
        separate(athlete, c("athlete", "abb"), sep = " \\(")
    ) %>% 
    filter(!is.na(athlete)) %>% 
    mutate(guide = if_else(str_detect(guide, "\\("), NA_character_, guide)) %>% 
    mutate(year = year, type = "Triathlon")
  
  
  clean_ind_tri
}

safe_tri <- safely(clean_triathlon)

tri_years <- year_vec %>% 
  map(safe_tri) %>% 
  map_dfr("result")

check_fun(tri_years)

all_sports <- bind_rows(
  list(arrow_years, ath_years, bb_years, cycle_years, fence_years, power_years,
       rugby_years, swim_years, table_years, tennis_years, tri_years, vb_years)
)

all_sports %>% 
  skimr::skim()

all_sports %>% 
  write_csv("2021/2021-08-03/athletes.csv")

check_fun(all_sports)

check_abb <- function(df_in) {
  df_in %>% 
    filter(str_length(abb) > 3) %>% 
    select(athlete, abb, type, year)
  
}

all_sports %>% check_abb() %>% distinct(type, year)


all_sports %>% group_by(athlete) %>% count(medal) %>% tally(n) %>% arrange(desc(n)) %>% filter(athlete !=c('NA'))

df1 <- all_sports %>% group_by(year, gender, type, medal) %>% count(medal) %>% tally(n) %>% spread(medal, n) %>%
  mutate(Total = Bronze + Silver + Gold)

df1b <- all_sports %>% group_by(year, gender, type, abb) %>% count(abb) %>% tally(n) %>% arrange(desc(n)) %>% slice_max(n)

view(df1b)
df1c <- merge(df1, df1b, by= c('year', 'type', 'gender'))

df2 <- all_sports %>% group_by(year, abb, medal) %>% count(medal) %>% tally(n) %>% spread(medal, n) %>%
  mutate(Total = Bronze + Silver + Gold) %>% filter(abb %in% c('USA', 'GBR','CHN', 'FRA','AUS', 'CAN',
                                                                                         'GER', 'POL',
                                                                                         'FRG', 'ESP'))

df3 <- all_sports %>% group_by(year, abb, type, medal) %>% count(medal) %>% tally(n)  %>% 
  filter(abb %in% c('USA', 'GBR','CHN', 'FRA','AUS', 'CAN','GER', 'POL','FRG', 'ESP')) %>% filter(gender !='Mixed')


df4 <- all_sports %>% group_by(gender, year, type) %>% count(medal) %>% tally(n)


ggplot(df3, aes(x = n, y = factor(year), fill= type)) + 
  geom_density_ridges2() +
  scale_fill_viridis(discrete = TRUE, option = "A", alpha = 0.8) +
  #scale_fill_manual(values = wes_palette("Rushmore1")) +
  #scale_fill_brewer(palette="Set2") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)



GBR <-
  all_sports %>% filter(abb == 'GBR') %>% 
  group_by(gender, year) %>% 
  count(medal) %>% tally(n) %>% filter(!is.na(gender), gender !='Mixed') %>% 
  mutate(year=factor(year)) %>% spread(gender, n)

view(GBR)


df2 = tidyr::gather(GBR, group, value, -year)


ggplot(GBR, aes(y=year))+
  geom_point(data = df2, aes(x = value, color = group), size = 3) +

  geom_segment(aes(x=Men,
                   xend=Women,
                   y=year,
                   yend=year),
               color="black", size=2)+
  geom_dumbbell(aes(x=Men, xend=Women, y=year, group=year),
                color="black", 
                size_x=6, 
                size_xend = 6,
                colour_x= '#4577A0',
                colour_xend = "#A60A55") + 
  labs(
    fill = "",
    x = "number of medals",
    title = "Great Britain paralympic medals",
    caption = "International Paralympic Committee | @nicci_potts | #TidyTuesday"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 1),
    plot.caption = element_text(hjust = 1, size = 10),
    text = element_text(family = myFont2),
    axis.title.y = element_blank(),
    legend.position='bottom', 
    legend.justification='left',
    legend.direction='horizontal'
  ) +
  scale_color_manual(name = "", values = c("#4577A0", "#A60A55"))








ggplot(GBR, aes(x = n, y = factor(year), fill = gender)) +
  geom_density_ridges2(color = 'black',
                       linetype = 1,
                       lwd = 1) +
  scale_fill_manual(values = alpha(c('#4577A0', '#A60A55'), alpha = 0.8)) +
  

  labs(
    fill = "",
    x = "number of medals",
    title = "Great Britain paralympic medals",
    caption = "International Paralympic Committee | @nicci_potts | #TidyTuesday"
  ) +
  
  theme_ridges(grid = FALSE) +
  theme(
    plot.title = element_text(hjust = 1),
    plot.caption = element_text(hjust = 1, size = 10),
    text = element_text(family = myFont2),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  )   +
  scale_y_discrete(expand = c(0, 0)) + 
  scale_x_continuous(limits = c(0, 100))+
  coord_cartesian(clip = "off") 









showtext_auto()
# https://fonts.google.com/featured/Superfamilies
font_add_google("Nunito", "Nunito")
font_add_google("Roboto", "Roboto")
font_add_google("Rambla", "Rambla")
font_add_google("Merriweather Sans", "Merriweather Sans")
myFont2 <- "Nunito"

font_add("heiti", "heiti.ttf")
font_add("constan", "constan.ttf", italic = "constani.ttf")
