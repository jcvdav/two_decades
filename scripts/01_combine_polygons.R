
# load pacakges
library(here)
library(sf)
library(tidyverse)

# Load baja shapefiles
baja <- st_read(dsn = here("data", "raw_data", "shapefiles", "Baja"), layer = "ZRP_PBC") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Baja California Peninsula") %>% 
  st_transform("ESRI:54009")

# Load magdalena shapefile
mag <- st_read(dsn = here("data", "raw_data", "shapefiles", "Baja"), layer = "Reserva_Magdalena") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Baja California Peninsula") %>% 
  st_transform("ESRI:54009")


# Load QROO shapefile
qroo <- st_read(dsn = here("data", "raw_data", "shapefiles", "Quintana Roo"), layer = "ZRP_Quintana_Roo") %>% 
  select(nombre = Name) %>% 
  mutate(zone = "Meso-American Reef")%>% 
  st_transform("ESRI:54009") %>% 
  st_zm()

# Load punta herrero
ph <- st_read(dsn = here("data", "raw_data", "shapefiles", "Quintana Roo", "Punta Herrero"), layer = "ZRP_PH") %>% 
  select(nombre = Nombre) %>% 
  mutate(zone = "Meso-American Reef") %>% 
  st_transform("ESRI:54009")

# Load nolazco
nolasco <- st_read(dsn = here("data", "raw_data", "shapefiles", "Sonora", "Isla San Pedro Nolasco"), layer = "ZRP_Nolasco") %>% 
  select(nombre = ET_ID) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")

# Load Puerto libertad
pl <- st_read(dsn = here("data", "raw_data", "shapefiles", "Sonora", "Puerto Libertad"), layer = "ZRP_Puerto_Libertad") %>% 
  select(nombre = ET_ID) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")

# Load ISPM
martir <- st_read(dsn = here("data", "raw_data", "shapefiles", "Poligonos ANPs", "SHAPE_ANPS"), layer = "182ANP_Geo_ITRF08Septiembre_2021") %>% 
  filter(str_detect(NOMBRE, "Isla San Pedro")) %>% 
  select(nombre = NOMBRE) %>% 
  mutate(zone = "Gulf of California") %>% 
  st_transform("ESRI:54009")


# COmbine, without magdalena
polygons <- rbind(baja, ph, qroo, nolasco, pl, martir) %>% 
  mutate(area = st_area(.)) %>% 
  select(nombre, zona = zone, area, geometry)

# Calculate area
t <- polygons %>% 
  filter(!nombre %in% c("Blanquizal", "Cayo Lobos", "Akumal")) %>% 
  group_by(zona) %>% 
  summarize(area = round(units::drop_units(sum(area) / 1e6), 2),
            n_poly = n()) %>% 
  st_drop_geometry()

t$area

sum(t$area)

# A tibble: 3 × 3
#zona                            area n_poly
#* <chr>                          <dbl>  <int>
#1 Baja California Peninsula  15425064.      6
#2 Gulf of California        305057839.      5
#3 Meso-American Reef        193629099.     15

# Export
st_write(polygons, here("data", "processed_data", "polygons.gpkg"), append = F)

