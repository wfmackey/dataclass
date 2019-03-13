install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


library("rnaturalearth")
library("rnaturalearthdata")

worldl <- ne_countries(scale = "large", returnclass = "sf") %>% 
  select(country = sovereignt,
         geometry)

worlds <- ne_countries(scale = "small", returnclass = "sf") %>% 
  select(country = sovereignt,
         geometry)



?ne_countries
write_rds(worlds, "../data/worldmap_data.Rds")

gapminder$country %>% unique()
world$country %>% unique()

gapminder_map <-
gapminder %>%
  left_join(world)

world %>% 
  ggplot() +
  geom_sf() +
  ggplotly()


all <- gapminder::gapminder_unfiltered

all07 <-  all %>% filter(year == 2007)





