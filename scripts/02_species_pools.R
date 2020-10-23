library(tidyverse)
library(data.table)
library(raster)
library(ggthemes)
library(patchwork)


# define species pools ----------------------------------------------------

# This script derives the species pools from the PNV output of all four models in the FORMASAM setting
# Input needed:
# 1. EnvGrid_Rosalia.asc
# 2. dem_rosalia.asc
# 3. EnvGrid_Dischma.asc
# 4. DEM_Dischma_SpinupRuns.asc
# 5. pnv_rosalia.csv and pnv_dischma.csv (created by script 01_analyse_results_of_PNV_runs.R)

# ROSALIA -----------------------------------------------------------------

PNV_ROSALIA  <- setDT(read.csv("pnv_processed/pnv_rosalia.csv", stringsAsFactors = FALSE))

# get resource units of rosalia landscape

ruid_rosa <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc")

# get dem of rosalia landscape

dem_rosa <- raster("../../materials/ROSALIA/gis/dem_rosalia.asc")

# calculate elevation of every ressource unit and define mangement unit accordingly

coord_ruid_rosa <- rasterToPoints(ruid_rosa) %>%
  as.data.frame(.)

xy <- coord_ruid_rosa %>%
  dplyr::select(-layer)

init_rosalia <- coord_ruid_rosa %>%
  rename(ruid = layer) %>%
  mutate(elevation = round(raster::extract(dem_rosa, xy), digits = 0),
         m.unit = case_when(elevation < 600 ~ 1,
                            elevation >= 600 ~ 2)) 

agb_rosalia <- PNV_ROSALIA %>%
  right_join(init_rosalia, by = c("x", "y")) %>%
  group_by(m.unit, model, climate, year) %>%
  summarize(agb_per_year = sum(AGB)) 

species_in_all_models_r <- PNV_ROSALIA %>%
  group_by(model, species) %>%
  summarise(n = n()) %>%
  spread(key = model, value = n) %>%
  na.omit(.) %>%
  .$species

species_rosalia <- PNV_ROSALIA %>%
  filter(species %in% species_in_all_models_r) %>%
  left_join(init_rosalia, by = c("x", "y", "ruid")) %>%
  group_by(m.unit, species, year, model, climate) %>%
  summarize(agb_species = sum(AGB)) %>%
  left_join(agb_rosalia, by = c("m.unit", "year", "model", "climate")) %>%
  mutate(share = round((agb_species / agb_per_year),2)) %>%
  ungroup(.) %>%
  mutate(landscape = "rosalia") 


# DISCHMA -----------------------------------------------------------------

PNV_DISCHMA  <- setDT(read.csv("pnv_processed/pnv_dischma.csv", stringsAsFactors = FALSE))

# load Env Grid

ruid_dischma <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc")

dem_disch <- raster("../../materials/DISCHMA/gis/dem_disch_ruid.asc")

# create table for species pools

coord_ruid_dischma <- rasterToPoints(ruid_dischma) %>%
  as.data.frame(.)

xy <- coord_ruid_dischma %>%
  dplyr::select(-layer)

init_dischma <- coord_ruid_dischma %>%
  rename(ruid = layer) %>%
  mutate(elevation = round(raster::extract(dem_disch, xy), digits = 0),
         m.unit = case_when(elevation < 1900 ~ 1,
                                   elevation %in% c(1900:2250) ~ 2,
                                   elevation > 2250 ~ 3)) 

agb_dischma <- PNV_DISCHMA %>%
  right_join(init_dischma, by = c("x", "y")) %>%
  group_by(year, m.unit, model, climate) %>%
  summarize(agb_per_year = sum(AGB))


species_in_all_models_d <- PNV_DISCHMA %>%
  group_by(model, species) %>%
  summarise(n = n()) %>%
  spread(key = model, value = n) %>%
  na.omit(.) %>%
  .$species


species_dischma <- PNV_DISCHMA %>%
  filter(species %in% species_in_all_models_d) %>%
  left_join(init_dischma, by = c("x", "y", "ruid")) %>%
  group_by(m.unit, species, year, model, climate) %>%
  summarize(agb_species = sum(AGB)) %>%
  left_join(agb_dischma, by = c("m.unit", "year", "model", "climate")) %>%
  mutate(share = round((agb_species / agb_per_year),2)) %>%
  ungroup(.) %>% 
  mutate(landscape = "dischma")




# derive species pools for both landscapes --------------------------------


species_sum <- species_rosalia %>%
  bind_rows(species_dischma) 

# seperate historic and future climate conditions

historic <- filter(species_sum, climate == "historic") %>%
  dplyr::select(species, year, model, share, m.unit, landscape) %>%
  spread(key = model, value = share) 

future <- filter(species_sum, climate %in% c("RCP45","RCP85")) %>%
  dplyr::select(species, year, model, share, m.unit, landscape, climate) %>%
  spread(key = model, value = share)


hist <- filter(historic, landscape == "dischma", m.unit == 1) %>%
  gather(model, share, - species, -year, -m.unit, -landscape)  %>%
  na.omit() %>%
  group_by(species, model) %>%
  summarize(maximum = max(share)) %>%
  ggplot(., aes(y = maximum, x = reorder(species, -maximum), col = model)) +
  geom_point()

# derive the three species pools low, high, high_future

historic_no <- filter(historic, (landscape == "dischma" & species == "piab") | (landscape == "rosalia" & species == "fasy")) %>%
  mutate(diversity ="no") 

historic_low <- filter(historic, (ILAND >= 0.35 & LANDCLIM >= 0.35)) %>%
  mutate(diversity = "low")

historic_high <-  filter(historic, (ILAND >= 0.03 & LANDCLIM >= 0.03)) %>%
  mutate(diversity = "high")

future_high <- filter(future, (ILAND >= 0.03 & LANDCLIM >= 0.03)) %>%
  bind_rows(historic_high) %>%
  mutate(diversity = "high_future")


# summarize everything in on table

species_summary <- bind_rows(historic_no, historic_low, historic_high, future_high) %>%
  group_by(species, diversity, m.unit, landscape) %>%
  distinct(species) %>%
  #bind_rows(lade_low) %>%
  arrange(landscape, diversity, m.unit)


# save table

write.table(species_summary, "initalization/species_summary.txt", row.names = FALSE, quote = FALSE)

# create species pool figure for supplement -------------------------------


species_names <- data.frame(species = c("abal",
                                        "acps",
                                        "alvi",
                                        "alin",
                                        "bepe",
                                        "cabe",
                                        "fasy",
                                        "lade",
                                        "piab",
                                        "pice",
                                        "pisy",
                                        "potr",
                                        "poni",
                                        "qupe",
                                        "tico"),
                            name = c("Abies alba",
                                     "Acer pseudoplatanus",
                                     "Alnus virids",
                                     "Alnus incana",
                                     "Betulus pendula",
                                     "Caprinus betulus",
                                     "Fagus sylvatica",
                                     "Larix decidua",
                                     "Picea abies",
                                     "Pinus cembra",
                                     "Pinus sylvestris",
                                     "Populus tremula",
                                     "Poulus nigra",
                                     "Quercus petrea",
                                     "Tilia cordata"))

species_summary <- read.table("initalization/species_summary.txt", header = TRUE) 

species_plotdata_dischma <- species_summary %>%
  filter(landscape == "dischma") %>%
  mutate(there = 1,
         m.unit_trans = case_when(m.unit == 1 ~ "< 1900 m",
                                  m.unit == 2 ~ "1900 - 2250 m",
                                  m.unit == 3 ~ "> 2250 m")) %>%
  spread(species, there) %>%
  gather(species, value, abal:qupe) %>%
  left_join(species_names)

species_plotdata_dischma[is.na(species_plotdata_dischma)] <- 0

order_dischma <- c("< 1900 m", "1900 - 2250 m", "> 2250 m")

species_plotdata_rosalia <- species_summary %>%
  filter(landscape == "rosalia") %>%
  mutate(there = 1,
         m.unit_trans = case_when(m.unit == 1 ~ "< 600 m",
                                  m.unit == 2 ~ "> 600 m")) %>%
  spread(species, there) %>%
  gather(species, value, abal:tico) %>%
  left_join(species_names)

species_plotdata_rosalia[is.na(species_plotdata_rosalia)] <- 0

order_rosalia <- c("< 600 m", "> 600 m")



species_pools_dischma <- ggplot(species_plotdata_dischma, aes(x = as.factor(name), 
                                              y = factor(m.unit_trans, levels = order_dischma))) + 
  geom_tile(aes(fill = factor(value, labels = c("no", "yes"))), color = "black") +
  scale_fill_manual(values = c("white", "darkgreen")) + 
  facet_grid(factor(diversity, levels = c("high_future", "high", "low", "no"), 
                    labels =  c("high+" , "high", "low", "no")) 
             ~ landscape) +
  labs(x = "", y = "", fill = "included") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_blank())


species_pools_rosalia <- ggplot(species_plotdata_rosalia, aes(x = as.factor(name), 
                                                              y = factor(m.unit_trans, levels = order_rosalia))) + 
  geom_tile(aes(fill = factor(value, labels = c("no", "yes"))), color = "black") +
  scale_fill_manual(values = c("white", "darkgreen")) + 
  facet_grid(factor(diversity, levels = c("high_future", "high", "low", "no"), 
                    labels =  c("high+", "high", "low", "no")) 
             ~ landscape) +
  labs(x = "", y = "", fill = "included") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank())



species_pools <-species_pools_dischma + species_pools_rosalia + plot_layout(ncol = 2)

ggsave( "../../results/figures/supplement/species_pools.png", species_pools, height = 7, width = 12)



# look up table for TREE DATA run  --------------------------------------------------------------


tree_data <-  bind_rows(historic_no, historic_low, historic_high, future_high) %>%
  group_by(species, landscape) %>%
  distinct(species) %>%
  arrange(landscape, species) 

write.table(tree_data, "tree_data/tree_data.txt", row.names = FALSE, quote = FALSE)

