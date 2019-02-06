library(tidyverse)
library(tigris)
library(tmap)
library(ggplot2)
library(leaflet)
library(stringr)
library(sf)
library(tidycensus)

od <- read.csv(file="ok_od_main_JT00_2015.csv")

od$w_geocode<-as.character(od$w_geocode)
od$w_geocode[which(length(od$w_geocode) == 14)] <- paste0("0", od$w_geocode[which(length(od$w_geocode) == 14)])

od$h_geocode<-as.character(od$h_geocode)
od$h_geocode[which(length(od$h_geocode) == 14)] <- paste0("0", od$h_geocode[which(length(od$h_geocode) == 14)])

work_tract <- substr(od$w_geocode,1,11) # Eliminates the blocks for work_tract
home_tract <- substr(od$h_geocode,1,11) # Eliminates the blocks for home_tract

# Add work_tract and home_tract to od as doubles
od[, "work_tract"] <- as.numeric(work_tract) 
od[, "home_tract"] <- as.numeric(home_tract) 

# Removing extra uneeded columns
od <- od %>%
  select(-(w_geocode)) %>% 
  select(-(h_geocode)) %>%
  select(-(createdate))

# Group by the work_trat and home_tract
# Then generating sums of the groups
# Filtering to only the tracts within the Oklahoma County

# This contains the tracts that have either the work or residence within the OK County
od_all_one <- od %>% 
  group_by(work_tract, home_tract) %>%
  summarise_all(funs(sum)) %>% 
  filter(between(work_tract, 40109000000, 40110000000) | between(home_tract, 40109000000, 40110000000))

# Both work and residence are within OK county
od_all_both <- od %>% 
  group_by(work_tract, home_tract) %>%
  summarise_all(funs(sum)) %>% 
  filter(between(work_tract, 40109000000, 40110000000) & between(home_tract, 40109000000, 40110000000))

##########################################################

OD <- od_all_both %>% filter(work_tract == home_tract) %>% select(work_tract, home_tract, S000)

library(tidyverse)
library(tidycensus)
library(leaflet)
library(stringr)
library(sf)
library(RColorBrewer)


tracts <- tracts("40", "109")

ok_all <- geo_join(tracts, OD, by_sp="GEOID", by_df="work_tract")


pal <- colorNumeric("Reds", domain = ok_all$S000)

ok_all <- ok_all %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(work_tract, "^([^,]*)"),
              color = "#444444",
              weight = 1,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~ pal(S000)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ S000,
            title = "Total Number of Jobs",
            opacity = 1)

plot(ok_all)




