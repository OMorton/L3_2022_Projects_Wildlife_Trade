setwd("G:/My Drive/TUoS/Teaching/L3")
.libPaths("C:/Packages") ## Set up for working from home.

library(sf)
library(tidyverse)
sf::sf_use_s2(F)

trade <- read.csv("G:/My Drive/TUoS/Data_Sources/Scheffers2019/Data/trade.csv", stringsAsFactors = F)
continents <- st_read("Wildlife Trade/Data/Spatial/World_Continents/World_Continents.shp")
countries <- st_read("Wildlife Trade/Data/Spatial/World_Countries_(Generalized)/World_Countries__Generalized_.shp") %>% 
  st_make_valid()
realms <- st_read("Wildlife Trade/Data/Spatial/Generalised_Biogeographic_Realms_2004/brpol_fin_dd.shp") %>% 
  group_by(REALM) %>% 
  summarise()

#### JW ####

## Get country lists
trade_amph <- trade %>% 
  filter(taxa == "Amphibian")

write.csv(trade_amph, "Wildlife Trade/Data/amphibian_trade.csv", row.names = F)


amph <- st_read("G:/My Drive/TUoS/Data_Sources/Scheffers2019/Data/RAW/Spatial/AMPHIBIANS/AMPHIBIANS_FINAL.gpkg")

amph_realm_intersections <- st_intersects(amph, realms, sparse = T)

amph_realm_intersections.df <- lapply(amph_realm_intersections, FUN = function(x) {data.frame(realm = realms$REALM[x])
}) %>% 
  bind_rows(.id = "id") %>% 
  mutate(id = as.numeric(id),
         species = amph$binomial[id]) %>% 
  select(species, realm) %>% 
  distinct()

amph_country_intersections <- st_intersects(amph, countries, sparse = T)

amph_country_intersections.df <- lapply(amph_country_intersections, FUN = function(x) {data.frame(country = countries$COUNTRY[x],
                                                                                                  ISO = countries$ISO[x])}) %>% 
  bind_rows(.id = "id") %>% 
  mutate(id = as.numeric(id),
         species = amph$binomial[id]) %>% 
  select(species, country, ISO)

write.csv(amph_realm_intersections.df, "Wildlife Trade/Data/amphibian_realm_intersections.csv", row.names = F)
write.csv(amph_country_intersections.df, "Wildlife Trade/Data/amphibian_country_intersections.csv", row.names = F)

## Pull out from the Scheffers data
amph_country_intersections.df <- data.table::fread("Wildlife Trade/Data/amphibian_country_intersections.csv")
Short_list <- amph_country_intersections.df %>% filter(country %in% c("Colombia", "Brazil", "Madagascar", "Malaysia", "Indonesia", "Philippines"))

Colombia <- left_join(filter(Short_list, country == "Colombia"), trade)
Brazil <- left_join(filter(Short_list, country == "Brazil"), trade)
Madagascar <- left_join(filter(Short_list, country == "Madagascar"), trade)
Malaysia <- left_join(filter(Short_list, country == "Malaysia"), trade)
Indonesia <- left_join(filter(Short_list, country == "Indonesia"), trade)
Philippines <- left_join(filter(Short_list, country == "Philippines"), trade)

write.csv(Colombia, "Wildlife Trade/2022/Output/JW/Colombia_Amph.csv")
write.csv(Brazil, "Wildlife Trade/2022/Output/JW/Brazil_Amph.csv")
write.csv(Madagascar, "Wildlife Trade/2022/Output/JW/Madagascar_Amph.csv")
write.csv(Malaysia, "Wildlife Trade/2022/Output/JW/Malaysia_Amph.csv")
write.csv(Indonesia, "Wildlife Trade/2022/Output/JW/Indonesia_Amph.csv")
write.csv(Philippines, "Wildlife Trade/2022/Output/JW/Philippines_Amph.csv")
