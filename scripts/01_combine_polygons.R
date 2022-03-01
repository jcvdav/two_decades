
library(here)
library(rnaturalearth)
library(ggsflabel)
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

martir <- st_read(dsn = here("data", "raw_data", "shapefiles", "Poligonos ANPs", "SHAPE_ANPS"), layer = "182ANP_Geo_ITRF08Septiembre_2021") %>% 
  filter(str_detect(NOMBRE, "Isla San Pedro")) %>% 
  select(nombre = NOMBRE) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")


polygons <- rbind(baja, ph, qroo, nolasco, pl, martir) %>% 
  mutate(area = st_area(.)) %>% 
  select(nombre, zona = zone, area, geometry)

polygons %>% 
  group_by(zona) %>% 
  summarize(area = units::drop_units(sum(area)) / 1e6,
            n_poly = n()) %>% 
  st_drop_geometry()

st_write(polygons, here("data", "processed_data", "polygons.gpkg"), append = F)

