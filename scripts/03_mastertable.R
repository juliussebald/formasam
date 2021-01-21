
library(raster) # version 3.3-13
library(sf) # version 0.9-5
library(tidyverse) # version 1.3.0
library(purrr) # version 0.3.4

set.seed(42)

# create initialization data ----------------------------------------------

# This script creates the initialization data that is necessary to run the  initalization.R script.

# Input needed:
# 1. EnvGrid_Rosalia.asc 
# 2. dem_rosalia.asc
# 3. EnvGrid_Dischma.asc
# 4. DEM_Dischma_SpinupRuns.asc
# 5. species_summary.txt (created by script 02_derive_species_pools.R)

# master table -----------------------------------------------------------------
 
# the mastertable defines initalization age and species shares for every stand and every simulation run

# get resource units landscapes

ruid_rosa <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc")
ruid_disch <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc")

# get dems of landscapes

dem_rosa <- raster("../../materials/ROSALIA/gis/dem_rosalia.asc")
dem_disch <- raster("../../materials/DISCHMA/gis/DEM_Dischma_SpinupRuns.asc")

# get coordinates of ressource units

coord_ruid_rosa <- rasterToPoints(ruid_rosa) %>%
  as.data.frame(.)

coord_ruid_disch <- rasterToPoints(ruid_disch) %>%
  as.data.frame(.)


xy_rosa <- dplyr::select(coord_ruid_rosa, -layer)
xy_disch <- dplyr::select(coord_ruid_disch, -layer)

# define maximum age

age_rosa <- 1:100
age_disch <- 1:150

# create inital states


# rosalia

init_rosa <- coord_ruid_rosa %>%
  rename(ruid = layer) %>%
  mutate(elevation = round(raster::extract(dem_rosa, xy_rosa), digits = 0),
         m.unit = case_when(elevation < 600 ~ 1,
                            elevation >= 600 ~ 2),
         landscape = "rosalia") %>%
  dplyr::select(ruid, m.unit, landscape) %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("alpha", "beta", "prod")) %>%
  bind_rows(.id = "scenario") %>%  
  replicate(n = 4, simplify = FALSE) %>%
  set_names(c("no", "low", "high", "high_future")) %>%
  bind_rows(.id = "diversity") %>%
  filter(!(scenario == "prod" & diversity != "no")) %>%
  filter(!(scenario != "prod" & diversity == "no")) %>%
  mutate(init.age = sample(age_rosa, nrow(.), replace = TRUE))

# take care of the fact that the diversity scenario "high_future" has no separate initial state

init_rosa[init_rosa$diversity == "high_future",]$init.age <- init_rosa[init_rosa$diversity == "high",]$init.age

# check if it worked

df <- data.frame(high_future = filter(init_rosa, diversity == "high_future")$init.age,
                 high = filter(init_rosa, diversity == "high")$init.age)

head(df)

# dischma

init_disch <- coord_ruid_disch %>%
  rename(ruid = layer) %>%
  mutate(elevation = round(raster::extract(dem_disch, xy_disch), digits = 0),
         m.unit = case_when(elevation < 1900 ~ 1,
                            elevation %in% c(1900:2250) ~ 2,
                            elevation > 2250 ~ 3),
         landscape = "dischma") %>%
  dplyr::select(ruid, m.unit, landscape) %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("alpha", "beta", "prod")) %>%
  bind_rows(.id = "scenario") %>%  
  replicate(n = 4, simplify = FALSE) %>%
  set_names(c("no", "low", "high", "high_future")) %>%
  bind_rows(.id = "diversity") %>%
  filter(!(scenario == "prod" & diversity != "no")) %>%
  filter(!(scenario != "prod" & diversity == "no")) %>%
  mutate(init.age = sample(age_disch, nrow(.), replace = TRUE))

# take care of the fact that the diversity scenario "high_future" has no seperate initial state

init_disch[init_disch$diversity == "high_future",]$init.age <- init_disch[init_disch$diversity == "high",]$init.age

# check if it worked

df <- data.frame(high_future = filter(init_disch, diversity == "high_future")$init.age,
high = filter(init_disch, diversity == "high")$init.age)

# create mastertable

master <- bind_rows(init_rosa, init_disch) %>%
  replicate(n = 20, simplify = FALSE) %>%
  set_names(1:20) %>%
  bind_rows(.id = "replication") %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("historic", "future", "no")) %>%
  bind_rows(.id = "disturbances") %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("historic", "RCP45", "RCP85")) %>%
  bind_rows(.id = "climate") %>%
  dplyr::select(landscape, ruid, m.unit, scenario, climate, diversity, disturbances, replication, init.age) %>%
  arrange(., ruid, scenario, diversity)
  
  

# define species shares for every landscape, scenario, m.unit, diversity combination


species <- read.table("initalization/species_summary.txt", header = TRUE,
                      stringsAsFactors = FALSE) %>%
                                  group_by(landscape, 
                                           diversity, 
                                           m.unit) %>%
                                  mutate(alpha = 1 / n(),
                                         beta = 0,
                                         prod = case_when((landscape == "rosalia" & 
                                                             species == "fasy" & 
                                                             diversity == "no") ~ 1,
                                                          (landscape == "dischma" & 
                                                            species == "piab" & 
                                                            diversity == "no") ~ 1,
                                                          TRUE ~ 0)) %>%
  ungroup(.) %>%
  gather(key = scenario, value = share, -(species:diversity)) %>%
  filter(!(scenario != "prod" & diversity == "no")) %>%
  filter(!(scenario == "prod" & diversity != "no"))


# sample beta species distribution


beta <- master %>%
  dplyr::select(-climate, -disturbances, -replication) %>%
  distinct(.) %>%
  filter(scenario == "beta") %>%
    left_join(species, 
               by = c("diversity", "m.unit", "scenario", "landscape")) %>%
  group_by(ruid, diversity, landscape) %>%
  mutate(share = sample(c(1, rep(0, (n() - 1))),  n())) %>%
  spread(key = species, value = share) %>%
  ungroup(.) %>%
  replicate(n = 20, simplify = FALSE) %>%
  set_names(1:20) %>%
  bind_rows(.id = "replication") %>%
  mutate(replication = as.integer(replication)) %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("historic", "RCP45", "RCP85")) %>%
  bind_rows(.id = "climate") %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("historic", "future", "no")) %>%
  bind_rows(.id = "disturbances")

# combine beta with the other scenarios


mastertable <- master %>%
  left_join(species, 
            by = c("diversity", "m.unit", "scenario", "landscape")) %>%
  mutate(replication = as.integer(replication)) %>%
  spread(key = species, value = share) %>%
  arrange(., ruid, scenario) %>%
  mutate_at(.vars = vars(abal:tico), function(x) round(x,4)) %>%
  filter(! scenario == "beta") %>%
  bind_rows(beta) %>%
  arrange(ruid)
  

# set all remanining NA`s to zero

mastertable[is.na(mastertable)] <- 0.000

# check if all rows sum up to 1

test <- mastertable %>%
  mutate(rs = rowSums(dplyr::select(., abal:tico))) 



unique(test$rs)


# save mastertable


write_csv(mastertable, "initalization/mastertable.csv")

mastertable <- read.csv("initalization/mastertable.csv")



