## =============================================================== ##
# This script takes in the raw INEGI municipal
# geostatistic frame and finds the municipal centroids.
# It also loads the data for the coast and both borders.
# This can be used to calculate distances from each
# municipality to the coast and borders.
# The data and a graph are saved in the output directory.
## =============================================================== ##

# Load necessary libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(units)
library(haven)


base_path <- "/Users/wernerd/Desktop/Daniel Werner/GeoData/"
municipal <- st_read(file.path(base_path, "Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_municipales.shp"))
#line path of entire N border
n_border <- st_read(file.path(base_path,
                              "US Border/tl_2023_us_internationalboundary.shp"))
#line path of entire S border
s_border <- st_read(file.path(base_path,
                              "Gua Border/Southern_Border_MX_GUA.shp"))
#line path of entire coastal region
coast <- st_read(file.path(base_path, "Coast/lc2018gw.shp"))

#Land ports of entry to the US
border <- st_read(file.path(base_path,"Entry Points/border_x.shp"))

mex_border_states <- c("AZ", "CA", "NM", "TX")
border <- border %>% filter(State %in% mex_border_states)

pacific_states <- c("Baja California", "Baja California Sur", "Sonora",
                    "Sinaloa", "Guerrero", "Nayarit", "Jalisco", "Tamaulipas", 
                    "Colima", "Michoacán", "Oaxaca", "Chiapas" )
pacific_coast <- coast %>% filter(EDO %in% pacific_states)

#Make Unique Municipality Code
municipal$CVE_ENT <- sub("^0", "", municipal$CVE_ENT)
municipal <- municipal %>% mutate(municipality= paste0(`CVE_ENT`, `CVE_MUN`))

#Extract the centroid for each municipality (center of the polygon)
municipal <- municipal %>%
  mutate(centroid = st_centroid(geometry))

#Transofrm Border Data to Same Coordinate System as Mexico
#And, just as a double check - transform the Mexican one too

n_border <- st_transform(n_border, crs = 6372)
border <- st_transform(border, crs = 6372)
s_border <- st_transform(s_border, crs = 6372)
municipal <- st_transform(municipal, crs = 6372)
coast <- st_transform(coast, crs = 6372)
pacific_coast <- st_transform(coast, crs = 6372)
municipal$centroid <- st_transform(municipal$centroid, crs = 6372)

#Keep only Mexican Border -- Get Rid of Canada and Alaska
n_border <- n_border %>% filter(IBTYPE == "M")

# Visually confirm that everything is in the right place
p <- ggplot() +
  geom_sf(data = municipal, fill = "#fdfdfd", color = "black") +
  geom_sf(data = n_border, color = "red", size = 3) +
  geom_sf(data = municipal$centroid,
          color = "purple", size = 0.1) +
  geom_sf(data = border$geometry,
          color = "blue", size = 3, shape = 23) +
  #geom_sf(data = s_border, color = "red", size = 6) +
  geom_sf(data = coast, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "",
       subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Calculate Relevant Distances
municipal <- municipal %>% group_by(municipality) %>%
  mutate(d_to_n_border = min(st_distance(centroid, n_border$geometry)),
         d_to_s_border = min(st_distance(centroid, s_border$geometry)),
         d_to_coast = min(st_distance(centroid, coast$geometry)),
         d_to_pacific = min(st_distance(centroid, pacific_coast$geometry)),
         d_to_entry_points = min(st_distance(centroid, border$geometry))) %>%
  ungroup()

#Make units in kilometers instead of meters
municipal$d_to_n_border <- set_units(municipal$d_to_n_border, "km")
municipal$d_to_s_border <- set_units(municipal$d_to_s_border, "km")
municipal$d_to_coast <- set_units(municipal$d_to_coast, "km")
municipal$d_to_pacific <- set_units(municipal$d_to_pacific, "km")
municipal$d_to_entry_points <- set_units(municipal$d_to_entry_points, "km")

#Keep only relevant columns
municipal <- municipal %>%
  st_set_geometry(NULL) %>%
  select(municipality, d_to_n_border, d_to_s_border, d_to_coast, d_to_pacific, d_to_entry_points)

#Save the data
write_dta(municipal, "/Users/wernerd/Desktop/Daniel Werner/final_geo.dta")

ggsave(filename = "/Users/wernerd/Desktop/Daniel Werner/Figures/Map_w_Borders.png",
       plot = p, width = 8, height = 6, units = "in", dpi = 300, bg = "white")


