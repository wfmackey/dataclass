install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(country = sovereignt,
         geometry)
?ne_countries
write_rds(world, "../data/worldmap_data.Rds")

gapminder$country %>% unique()
world$country %>% unique()

gapminder_map <-
gapminder %>%
  left_join(world)

world %>% 
  ggplot() +
  geom_sf() +
  ggplotly()
