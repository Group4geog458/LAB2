library(tidyverse)
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
od_all <- od %>% 
  group_by(work_tract, home_tract) %>%
  summarise_all(funs(sum)) %>% 
  filter(between(work_tract, 40109000000, 40110000000) | between(home_tract, 40109000000, 40110000000))

# Generating a sum of all the job columns
od_all$total_sum <-  rowSums(od_all[3])
od_all$goods_sum <- rowSums(od_all[10])
od_all$trade_sum <- rowSums(od_all[11])
od_all$other_sum <- rowSums(od_all[12])

# Creating a new dataframe of just work_tract, home_tract and the sums
od_sum <- select(od_all, 
                 work_tract, 
                 home_tract, 
                 total_sum, 
                 goods_sum,
                 trade_sum,
                 other_sum)

print(od_sum)