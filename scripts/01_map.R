

library(here)
library(rnaturalearth)
library(ggsflabel)
library(sf)
library(tidyverse)

polygons <- st_read(here("data", "processed_data", "polygons.gpkg"))

points <- polygons %>% 
  st_centroid()


### CREATE FIGURE

coast <- ne_countries(country = "Mexico", returnclass = "sf") %>% 
  st_transform(4326)

wrld <- ne_countries(continent = "North America", returnclass = "sf") %>% 
  st_transform(4326) %>% 
  st_crop(st_buffer(coast, 5e5))

eez <- st_read("/Volumes/GoogleDrive/Shared drives/emlab/data/marine-regions-eez-v11/World_EEZ_v11_20191118_gpkg/eez_v11.gpkg") %>% 
  filter(ISO_TER1 == "MEX") %>% 
  rmapshaper::ms_simplify()

map <- ggplot() +
  geom_sf(data = eez, fill = "transparent", color = "black", linetype = "dashed") +
  geom_sf(data = wrld, color = "transparent") +
  geom_sf(data = coast, color = "black", size = 0.5, fill = "steelblue", alpha = 0.75) +
  geom_sf(data = points, aes(fill = zona), shape = 21, size = 2) +
  # geom_sf_text_repel(data = points, aes(label = nombre)) +
  theme_void() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0)) +
  guides(fill = guide_legend("Region"))

ggsave(plot = map,
       file = here("results", "img", "map.jpg"),
       width = 6,
       height = 4.5)










