#  Package install
library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("mapproj")

# csv file
incarceration_data <- read.csv(file = 'source/incarceration_trends.csv')

# Introduction + Summary Analysis Code
pop_current_year <- incarceration_data %>% filter(year == max(year))
pop_20_years_ago <- incarceration_data %>% filter(year == (max(year)-20))

current_avg_white_pop <- pop_current_year %>% 
  summarize(avg_pop = mean((na.omit(white_pop_15to64)))) %>% 
  pull(avg_pop)
current_avg_aapi_pop <- pop_current_year %>% 
  summarize(avg_pop = mean((na.omit(aapi_pop_15to64)))) %>% 
  pull(avg_pop)

highest_white_pop_county <- pop_current_year %>% 
  filter(white_pop_15to64 == max(white_pop_15to64)) %>%
  pull(county_name)
highest_aapi_pop_county <- pop_current_year %>% 
  filter(aapi_pop_15to64 == max(aapi_pop_15to64)) %>%
  pull(county_name)

lowest_white_pop_county <- pop_current_year %>% 
  filter(white_pop_15to64 == min(white_pop_15to64)) %>%
  pull(county_name)
lowest_aapi_pop_county <- pop_current_year %>% 
  filter(aapi_pop_15to64 == min(aapi_pop_15to64)) %>%
  pull(county_name)

change_20y_white_pop <- pop_current_year %>% select(white_pop_15to64) %>%
  rename("wht_pop_15to64_current" = white_pop_15to64) %>% 
  add_column("wht_pop_15to64_20yrs_ago" = pop_20_years_ago$white_pop_15to64) %>%
  transmute(white_pop15to64_difference = 
              wht_pop_15to64_current - wht_pop_15to64_20yrs_ago) %>%
  pull(white_pop15to64_difference)

change_20y_aapi_pop <- pop_current_year %>% select(aapi_pop_15to64) %>%
  rename("aapi_pop_15to64_current" = aapi_pop_15to64) %>% 
  add_column("aapi_pop_15to64_20yrs_ago" = pop_20_years_ago$aapi_pop_15to64) %>%
  transmute(aapi_pop15to64_difference = 
              aapi_pop_15to64_current - aapi_pop_15to64_20yrs_ago) %>%
  pull(aapi_pop15to64_difference)

# Trends over time chart Code
wht_aasi_population <- incarceration_data %>%
  group_by(year) %>% filter(year >= 1998 && year <= 2018) %>%
  summarize(aapi_pop = sum(aapi_pop_15to64),
            white_pop = sum(white_pop_15to64)) %>%
  gather(key = Race, value = population, - year)

wht_aasi_ToT <- ggplot(data = wht_aasi_population) +
  geom_point(mapping = aes(x = year, y = population)) +
  geom_line(mapping = aes(x = year, y = population, color = Race)) +
  labs(title = "Whites and AAPIs Jail Population Comparison between 1998 and 2018",
    x = "Year",
    y = "Jail Population")
wht_aasi_ToT

# Variable comparison chart Code

relevant_data <- incarceration_data %>% filter(year == max(year)) %>% 
  unite(location, county_name:state, sep = ", ",remove=FALSE, na.rm=TRUE)

wht_aasi_top3_counties <- relevant_data %>% 
  select(location, 
         white_pop_15to64, 
         aapi_pop_15to64, 
         total_pop_15to64) %>%
  top_n(3) %>%
  arrange(total_pop_15to64) %>%  
  gather(key = race, 
         value = population, -location, -total_pop_15to64)

wht_aasi_top3_comparison <- ggplot(wht_aasi_top3_counties) +
  geom_col(mapping = aes(x = location, y = population, fill = race), 
           position = "dodge") +
  labs(title = "Most populated Jails comparison in 2018: White vs Asian population",
    subtitle = "Comparing the population of Whites vs Asian in the 3 most populated county jails",
    x = "County",
    y = "People arrested in 2018")
wht_aasi_top3_comparison

# Map Code

aapi_mapping <- relevant_data %>% 
  mutate(state = ifelse(state == "DC", "CO", state)) %>% 
  group_by(region = state) %>% 
  summarize(aapi_pop = sum(aapi_pop_15to64))
aapi_mapping$region <- tolower(state.name[match(aapi_mapping$region, state.abb)])

states_map <- map_data("state")
aapi_mapped <- ggplot(aapi_mapping, aes(map_id = region)) + 
  geom_map(aes(fill = aapi_pop), map = states_map) +
  scale_fill_gradientn(colours=c("purple","yellow")) + 
  expand_limits(x = states_map$long, y = states_map$lat)
aapi_mapped
