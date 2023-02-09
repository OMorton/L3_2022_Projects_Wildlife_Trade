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

## MTM

# use Alice country level data with species and country resolution

## RMW

unique(trade$UT_type)

Purpose_data <- trade %>% 
  mutate(Food = ifelse(grepl("Food", UT_type), 1, 0),
         Pet_or_Display = ifelse(grepl("Pets/display animals", UT_type), 1, 0),
         Medicine = ifelse(grepl("Medicine", UT_type), 1, 0),
         Sport_Hunting = ifelse(grepl("Sport hunting/specimen collecting", UT_type), 1, 0),
         Horticulture = ifelse(grepl("Horticulture", UT_type), 1, 0),
         Unknown = ifelse(grepl("Unknown", UT_type), 1, 0),
         Research = ifelse(grepl("Research", UT_type), 1, 0),
         Manufacturing_chemicals = ifelse(grepl("Manufacturing chemicals|Other chemicals", UT_type), 1, 0),
         Jewelry_Accessories_Apparel = ifelse(grepl("Wearing apparel|accessories|jewellery", UT_type), 1, 0),
         Handicrafts = ifelse(grepl("Handicrafts", UT_type), 1, 0),
         Other = ifelse(grepl("Other household goods|Other", UT_type), 1, 0),
         Poisons = ifelse(grepl("Poisons", UT_type), 1, 0),
         Fuels = ifelse(grepl("Fuels", UT_type), 1, 0),
         Fibre = ifelse(grepl("Fibre", UT_type), 1, 0),
         Construction = ifelse(grepl("Construction or structural materials", UT_type), 1, 0))

write.csv(Purpose_data, "Wildlife Trade/2022/Output/RMW/Purpose_data.csv")
g