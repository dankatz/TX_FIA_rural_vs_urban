# Comparing urban vs rural tree composition in Texas
# 

### set up workspace and load any required functions =============================
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(sf)
library(ggthemes)
library(readr)
rm(list = ls())


### data source 1: traditional FIA data, downloaded from the FIA datamart ##############################################
# https://apps.fs.usda.gov/fia/datamart/

tx_boundary <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_State_Boundary/Texas_State_Boundary.shp")
tx_counties <- read_sf("C:/Users/dsk856/Box/texas/statewide_abundance/Texas_County_Boundaries_Detailed/Texas_County_Boundaries_Detailed.shp")

#load FIA data
setwd("C:/Users/dsk856/Box/texas/statewide_abundance/TX FIA plots")
plots <- read.csv("TX_PLOT.csv") #head(plots)
plots2 <- dplyr::select(plots, STATECD, COUNTYCD, PLOT, LAT, LON, ELEV) %>% distinct()
species_codes_table_raw <- read.csv("C:/Users/dsk856/Box/texas/statewide_abundance/urban FIA/REF_SPECIES.csv")
species_codes_table <- dplyr::select(species_codes_table_raw, SPCD, COMMON_NAME, GENUS, SPECIES)
t <- read_csv("TX_TREE.csv")

#combine tree data with plots 
tp <- left_join(t, plots2)
plots_uniq <- tp %>% dplyr::select(LAT, LON, ELEV) %>% distinct() 

#select just the most recent set of plot surveys
recent_plots <- tp %>% #mutate(plot_coord = paste(LAT, LON)) %>% 
  dplyr::select(LAT, LON, INVYR) %>%
  distinct() %>% group_by(LAT, LON) %>% top_n(n = 1) %>%
  mutate(most_recent_cycle = 1)

tp <- left_join(tp, recent_plots)

#map of FIA plots with tree data in TX
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_point(aes(x = LON, y = LAT, col = ELEV),
             data = plots_uniq, alpha = .7, size = 0.5)  + 
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


## filter plots that are within 100 km of Austin -----------------------------------------------------
names(tp)
austin_coords <- data.frame(place = "Austin", Lon = -97.7, Lat = 30.3) #add in other cities here
austin_coords_sf <- st_as_sf(austin_coords, coords = c("Lon", "Lat"), crs = 4326)
tp_sf <- st_as_sf(tp, coords = c("LON", "LAT"), crs = 4326)
st_crs(austin_coords) <- st_crs(tp_sf)
tp_sf_austin <- tp_sf %>% #sample_n(10) %>% #calculate distance from all rows to Austin
  mutate(distance_to_austin = st_distance(., austin_coords_sf)) #takes a few seconds to run

tp_sf_austin2 <- tp_sf_austin %>% #filter the plots that are within 100 km of Austin
                 mutate(distance_to_austin_km = as.numeric(distance_to_austin)/1000) %>%
                 filter(distance_to_austin_km < 100) #only select plots within 100 km
head(tp_sf_austin)

#map of FIA plots near Austin
plot(tp_sf_austin2$ELEV)
ggplot(tx_boundary) +   geom_sf(data = tx_boundary, colour = "black", fill = NA) +
  geom_sf(data = tp_sf_austin2, alpha = .7, size = 0.5)  + 
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum=NA) #removes sf induced gridlines


## summarize tree composition in plots that are near Austin -----------------------------------------
names(tp_sf_austin2)
ba_by_sp <- tp_sf_austin2 %>% 
  dplyr::select(DIA, SPCD) %>%
  mutate(basal_area = 0.005454154 * DIA^2) 

ba_by_sp$geometry <- NULL
total_ba_across_plots <- ba_by_sp %>% ungroup() %>% summarise(ba_grand_total = sum(basal_area, na.rm = TRUE))  
ba_by_sp <- ba_by_sp %>%
  group_by(SPCD) %>%
  summarize(basal_area_sp = sum(basal_area, na.rm = TRUE),
            n = n(),
            rel_ba = basal_area_sp/total_ba_across_plots$ba_grand_total) %>%
  arrange(-rel_ba)
ba_by_sp <- left_join(ba_by_sp, species_codes_table)
ba_by_sp

write_csv(ba_by_sp, "C:/Users/dsk856/Box/texas/statewide_abundance/tree_abundance_near_austin200506.csv")

summarize(tp_sf_austin2)

### data source 2: Urban FIA data #################################################################
# https://apps.fs.usda.gov/fia/datamart/images/urbandatamart.html
# contact person for Urban FIA data (E.g., completion dates for other cities): Lucie Lepine
urban_fia <- read_csv("C:/Users/dsk856/Box/texas/austin_pilot_sampling/Austin_UrbanFIA.csv")
head(urban_fia)
ufia_ba_by_sp <- urban_fia %>% 
  dplyr::select(DIA, SPCD,GENUS, SPECIES, COMMON_NAME) %>%
  mutate(basal_area = 0.005454154 * DIA^2) 
ufia_total_ba_across_plots <- ufia_ba_by_sp %>% ungroup() %>% summarise(ba_grand_total = sum(basal_area, na.rm = TRUE))  
ufia_ba_by_sp <- ufia_ba_by_sp %>%
  group_by(GENUS, SPECIES, COMMON_NAME) %>%
  summarize(basal_area_sp = sum(basal_area, na.rm = TRUE),
            n = n(),
            rel_ba = basal_area_sp/ufia_total_ba_across_plots$ba_grand_total) %>%
  arrange(-rel_ba)
ufia_ba_by_sp
write_csv(ufia_ba_by_sp, "C:/Users/dsk856/Box/texas/statewide_abundance/tree_abundance_in_austin200506.csv")


# data source 3: NAB stations
#load pollen collection sites
tx_nab_coords <- read.csv("C:/Users/dsk856/Box/texas/NAB/TX_NAB_station_coords.csv")
