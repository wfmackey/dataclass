# Mapping -------------

library(tmap) 
library(maps) 
library(mapdata) 
library(tidyverse)
library(sp)
library(ggthemes)


aus<-map("worldHires", "Australia", 
         fill=TRUE, 
         xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))

world_map <- map_data("world") %>% 
  select(country = region,
         lat, long, group)

world_map %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed()

# cool

gapminder_1952 <- gapminder %>% 
  mutate(country = as.character(country)) %>% 
  filter(year == 1952)

world_data_1952 <-  world_data %>% 
  mutate(country = case_when(
    country == "USA" ~ "United States",
    TRUE ~ country
  )) %>% 
  left_join(gapminder_1952, by = "country")

world_data$country %>% unique()
gapminder$country %>% unique()

world_data_1952 %>% 
  ggplot(aes(x = long, 
             y = lat, 
             group = group,
             fill = lifeExp)) +
  geom_polygon() +
  coord_fixed()


world_data_1952 %>% 
  ggplot(aes(x = long, 
             y = lat, 
             group = group)) +
  geom_polygon() +
  geom_point(aes(colour = lifeExp)) +
  coord_fixed()

base_map <-
  ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

base_map +
  geom_point()

