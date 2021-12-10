
library(here)
library(rnaturalearth)
library(sf)
library(tidyverse)

baja <- st_read(dsn = here("data", "raw_data", "shapefiles", "Baja"), layer = "ZRP_PBC") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Baja California Peninsula") %>% 
  st_transform("ESRI:54009")

mag <- st_read(dsn = here("data", "raw_data", "shapefiles", "Baja"), layer = "Reserva_Magdalena") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Baja California Peninsula") %>% 
  st_transform("ESRI:54009")

qroo <- st_read(dsn = here("data", "raw_data", "shapefiles", "Quintana Roo"), layer = "ZRP_Quintana_Roo") %>% 
  select(nombre = Name) %>% 
  mutate(zone = "Meso-American Reef")%>% 
  st_transform("ESRI:54009") %>% 
  st_zm()

ph <- st_read(dsn = here("data", "raw_data", "shapefiles", "Quintana Roo", "Punta Herrero"), layer = "ZRP_PH") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Meso-American Reef") %>% 
  st_transform("ESRI:54009")

nolasco <- st_read(dsn = here("data", "raw_data", "shapefiles", "Sonora", "Isla San Pedro Nolasco"), layer = "ZRP_Nolasco") %>% 
  select(nombre = ET_ID) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")

pl <- st_read(dsn = here("data", "raw_data", "shapefiles", "Sonora", "Puerto Libertad"), layer = "ZRP_Puerto_Libertad") %>% 
  select(nombre = ET_ID) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")


polygons <- rbind(baja, mag, ph, qroo, nolasco, pl) %>% 
  mutate(area = st_area(.)) %>% 
  select(nombre, zona = zone, area, geometry)

st_write(polygons, here("data", "processed_data", "polygons.gpkg"), append = F)

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
  geom_sf(data = points, aes(fill = zona), shape = 21, size = 3) +
  theme_void() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0)) +
  guides(fill = guide_legend("Region"))

ggsave(plot = map,
       file = here("20_res", "img", "map.pdf"),
       width = 6,
       height = 4.5)










