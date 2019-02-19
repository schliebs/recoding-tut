#' ---
#' title: "Session 1: Introduction to R and the Tidyverse"
#' author: "David Zimmermann"
#' date: 2018-09-03
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     theme: lumen
#'     highlight: pygments
#' ---

# For each day have a separate script and separate project in which you answer the exercises.
# Always comment your code to help your future self understand what you do!

#' 
#' # dplyr
#' 
#' ## Data Manipulation
#' 
#' 1. Load the tidyverse and nycflights13 and use `glimpse()` to look at the `flights` data
library(tidyverse)
library(nycflights13)
flights %>% glimpse()

#' 2. Discuss with your neighbor what the following function do: `filter`, `arrange` + `desc`, `select`, `rename`, `mutate`, `mutate_if`, `mutate_at`, `transmute`, `summarise`, `summarise_if`, `summarise_at`, `group_by`, `slice`
#' 
#' 3. Which 7 carriers had the highest median arrival delay?
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  summarise(median_arr_delay = median(arr_delay)) %>% 
  arrange(desc(median_arr_delay)) %>% 
  slice(1:7)

#' 4. Sort all flights by their departure delay, relative to the mean departure delay of the respective origin airport.
flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(origin) %>% 
  transmute(rel_dep_delay = dep_delay - mean(dep_delay)) %>% 
  arrange(desc(rel_dep_delay))

#' 5. Look at the `starwars` data (loaded with the tidyverse)
#' 
#' - Create a count of the haircolors of the characters, which color appears most often?
starwars %>% 
  count(hair_color, sort = T)

#' - Look at the function `unnest` and `strsplit` and try to tidy the haircolors.
starwars %>% 
  mutate(hair_color = strsplit(hair_color, ", ")) %>% 
  unnest(hair_color) %>% 
  count(hair_color, sort = T)

#' - Who is the tallest character that is born on tatooine?
starwars %>% 
  filter(homeworld == "Tatooine") %>% 
  arrange(desc(height)) %>% 
  slice(1)

#' - Which character has the highest amount of movie appearances?
starwars %>% 
  unnest(films) %>% 
  group_by(name) %>% 
  tally(sort = T) 
# alternative using map
starwars %>% 
  transmute(name, n_films = map_dbl(films, length)) %>% 
  arrange(desc(n_films))

#' - Which character has the highest amount of vehicles?
starwars %>% 
  unnest(vehicles) %>% 
  group_by(name) %>% 
  tally(sort = T)

#' - Create for each movie a list of characters that appear.
starwars %>% 
  unnest(films) %>% 
  select(name, films) %>% 
  mutate(value = 1) %>% 
  spread(key = "films", value = "value", fill = 0)

#' ## Joins
#' 
#' 1. Which aircraft manufacturer had the highest number of flights?
left_join(flights,
          planes %>% select(tailnum, manufacturer), 
          by = "tailnum") %>% 
  group_by(manufacturer) %>% 
  tally(sort = T)

# use `case_when()` to replace certain values only
left_join(flights,
          planes %>% select(tailnum, manufacturer), 
          by = "tailnum") %>% 
  mutate(manufacturer = case_when(
    manufacturer == "AIRBUS INDUSTRIE" ~ "AIRBUS",
    manufacturer == "MCDONNELL DOUGLAS AIRCRAFT CO" ~ "MCDONNELL DOUGLAS",
    manufacturer == "MCDONNELL DOUGLAS CORPORATION" ~ "MCDONNELL DOUGLAS",
    TRUE ~ manufacturer
  )) %>% 
  group_by(manufacturer) %>% 
  tally(sort = T)
  
#' 2. Which model had the highest number of flights?
left_join(flights,
          planes %>% select(tailnum, model), 
          by = "tailnum") %>% 
  group_by(model) %>% 
  tally(sort = T)

#' 3. Is there a correlation between departure delay and wind speed?
flights_weather <- left_join(flights, 
                             weather %>% select(origin, time_hour, wind_dir, wind_speed), 
                             by = c("origin", "time_hour")) %>% 
  filter(!is.na(dep_delay), !is.na(wind_speed))

cor(flights_weather$dep_delay, flights_weather$wind_speed)

#' 4. Are all destination airports in the airports dataset and vice versa? Which are missing?

# are all flights in airports?
anti_join(flights, airports, by = c("origin" = "faa"))%>% count(origin)
anti_join(flights, airports, by = c("dest" = "faa")) %>% count(dest)

# are all airports in flights?
anti_join(airports, flights, by = c("faa" = "origin"))%>% count(faa)
anti_join(airports, flights, by = c("faa" = "dest"))%>% count(faa)

#' 5. Add the missing airport information to the airports dataset.
#' ...
#' 
#' # purrr
#' 
#' 1. Use the GOT characters and find the character that has the highest amount of titles.
library(repurrrsive)

vec <- map(got_chars, "titles") %>% 
  set_names(map(got_chars, "name")) %>% 
  map_dbl(function(x) length(x[x != ""])) 

vec[vec == max(vec)]

# more compact 
map(got_chars, "titles") %>% 
  set_names(map(got_chars, "name")) %>% 
  map_dbl(function(x) length(x[x != ""])) %>% 
  {.[. == max(.)]}

# alternative
map(got_chars, "titles") %>% 
  set_names(map(got_chars, "name")) %>% 
  map_dbl(~length(.x[.x != ""]))

# alternative
length_non_empty <- function(x) length(x[x != ""])
map(got_chars, "titles") %>% 
  set_names(map(got_chars, "name")) %>% 
  map_dbl(length_non_empty)

#' 2. What are the characters with the highest number of allegiances, tv appearances, and book appearances.
map(got_chars, "allegiances") %>% 
  set_names(map(got_chars, "name")) %>% 
  map_dbl(function(x) length(x[x != ""])) %>% 
  {.[. == max(.)]}

#' 3. Create a data-frame in the tidy format that contains the name-actor/actress combinations.

map_dfr(got_chars, function(x) as_data_frame(x[c("name", "playedBy")]))
map_dfr(got_chars, ~as_data_frame(.x[c("name", "playedBy")])) %>% 
  mutate(playedBy = ifelse(playedBy == "", NA, playedBy))

# alternative if all variables have the same length (doesnt work with playedBy but with born)
map_dfr(got_chars, `[`, c("name", "born"))

# map(1:3, `+`, 2)

#' 4. Reshape the earlier name-actor/actress dataset into an encoded dataset (i.e., rows are the names, columns are the actor/actress, the values are T/F or 1/0).
df <- map_dfr(got_chars, function(x) as_data_frame(x[c("name", "playedBy")]))

actor_char_map <- df %>%
  mutate(
    playedBy = ifelse(playedBy == "", NA, playedBy),
    value = 1
  ) %>% 
  spread(key = "name", value = "value", fill = 0)

actor_char_map

#' 5. Use the online dataset of all GOT characters from https://anapioficeandfire.com and https://github.com/joakimskoog/AnApiOfIceAndFire/tree/master/data (hint `jsonlite::fromJSON()`) and repeat the exercises from before.
house_url <- "https://raw.githubusercontent.com/joakimskoog/AnApiOfIceAndFire/master/data/houses.json"

library(jsonlite)
got_houses <- read_json(house_url)
map_chr(got_houses, "Name") %>% head()

got_houses[[1]] %>% names()

# region-house
map_dfr(got_houses, function(x) as_data_frame(x[c("Name", "Region")]))
map_dfr(got_houses, ~as_data_frame(.x[c("Name", "Region")]))

map_dfr(got_houses, ~as_data_frame(.x[c("Name", "Region")])) %>% 
  group_by(Region) %>% 
  tally() %>% 
  arrange(n)

# map_dfr(got_houses, ~as_data_frame(.x[c("Name", "Region")])) %>% 
#   filter(Region == "Beyond the Wall")

#' 6. Use the flights dataset and compute a bootstrapped mean of arrival delay using 1000 subsamples of 50% of the values (hint `sample_frac()`)
library(nycflights13)
flights %>% sample_n(10)
flights %>% sample_frac(0.01)

flights2 <- flights %>% filter(!is.na(arr_delay))

# make the randomness reproducible
set.seed(201809045)
mean_df <- map_dfr(1:100, function(i) {
  flights2 %>% 
    sample_frac(0.5) %>% 
    summarise(mean = mean(arr_delay))
})

mean_df

quantile(mean_df$mean)
ggplot(mean_df, aes(x = mean)) + geom_histogram(bins = 30)
