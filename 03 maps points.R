
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, hrbrthemes, ggthemes, extrafont, ggrepel, showtext, geodata, lubridate, zoo, SPEI, terra, rgeos, rnaturalearthdata, glue, stringr, sf, tidyverse, gtools, fs)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Font --------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
crds <- read_csv('../tbl/pnt/coordinates.csv')
wrld <- rnaturalearthdata::map_units50 %>% st_as_sf() 
wrld <- wrld[st_is_valid(wrld),]
afrc <- filter(wrld, continent == 'Africa')
shpf <- st_as_sf(x = crds, coords = c('Long', 'Lat'), crs = st_crs(4326))
lbls <- afrc %>% st_centroid %>% st_coordinates %>% as_tibble %>% mutate(country = afrc$admin)
lbls <- lbls %>% filter(country %in% c('Ivory Coast', 'Burkina Faso', 'Ghana', 'Togo', 'Benin', 'Nigeria', 'Cameroon', 'Algeria'))

# To make the map ---------------------------------------------------------
gmap <- ggplot() + 
  geom_sf(data = afrc, fill = NA, col = 'grey60') + 
  geom_point(data = crds, aes(x = Long, y = Lat), col = 'brown', size = 5) +
  geom_text_repel(data = crds, aes(x = Long, y = Lat, label = ID, family = 'Roboto'), size = 8) +
  labs(x = 'Longitude', y = 'Latitud') +
  geom_text_repel(data = lbls, aes(x = X, y = Y, label = country), family = 'Roboto', size = 10) + 
  coord_sf(xlim = extent(shpf)[1:2], ylim = c(extent(shpf)[3], 15)) + 
  theme_pander() +
  theme(axis.text.x = element_text(size = 30, family = 'Roboto'), 
        axis.text.y = element_text(size = 30, family = 'Roboto'), 
        axis.title.x = element_text(size = 40, face = 'bold', family = 'Roboto'), 
        axis.title.y = element_text(size = 40, face = 'bold', family = 'Roboto')) 

ggsave(plot = gmap, 
       filename = '../png/maps/points_ids.png', units = 'in', width = 7, height = 5, dpi = 300)
  

