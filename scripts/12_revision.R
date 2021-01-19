library(raster)
library(sf)
library(tidyverse)
library(purrr)
library(RSQLite)
library(ggthemes)
library(gtools)
library(data.table)
library(patchwork)

# This script creates the input data for the revision of the manuscript: Mixing tree species at different spatial scales: The buffering
# effects of alpha, beta and gamma diversity against disturbances under climate change



# Table of combinations ----------------------------------------------------

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


xy_rosa <- dplyr::select(coord_ruid_rosa, -EnvGrid_Rosalia)
xy_disch <- dplyr::select(coord_ruid_disch, -EnvGrid_Dischma)

# basic setup rosalia

init_rosa <- coord_ruid_rosa %>%
  rename(ruid = EnvGrid_Rosalia) %>%
  mutate(elevation = round(raster::extract(dem_rosa, xy_rosa), digits = 0),
         m.unit = case_when(elevation < 600 ~ 1,
                            elevation >= 600 ~ 2),
         landscape = "rosalia") %>%
  dplyr::select(ruid, m.unit, landscape) %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("alpha", "beta", "prod")) %>%
  bind_rows(.id = "scenario") %>%  
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("no", "low", "high_future")) %>%
  bind_rows(.id = "diversity") %>%
  filter(!(scenario == "prod" & diversity != "no")) %>%
  filter(!(scenario != "prod" & diversity == "no")) #%>%

# basic setup dischma

init_disch <- coord_ruid_disch %>%
  rename(ruid = EnvGrid_Dischma) %>%
  mutate(elevation = round(raster::extract(dem_disch, xy_disch), digits = 0),
         m.unit = case_when(elevation < 1900 ~ 1,
                            elevation %in% c(1900:2250) ~ 2,
                            elevation > 2250 ~ 3),
         landscape = "dischma") %>%
  dplyr::select(ruid, m.unit, landscape) %>%
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("alpha", "beta", "prod")) %>%
  bind_rows(.id = "scenario") %>%  
  replicate(n = 3, simplify = FALSE) %>%
  set_names(c("no", "low", "high_future")) %>%
  bind_rows(.id = "diversity") %>%
  filter(!(scenario == "prod" & diversity != "no")) %>%
  filter(!(scenario != "prod" & diversity == "no")) #%>%

# create table of combinations

master_base <- bind_rows(init_rosa, init_disch) %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("historic", "RCP85")) %>%
  bind_rows(.id = "climate") %>%
  replicate(n = 10, simplify = FALSE) %>%
  set_names(1:10) %>%
  bind_rows(.id = "replication")


master_rot <- master_base %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("historic_600", "future_100")) %>%
  bind_rows(.id = "disturbances") %>%
  mutate(age_structure = "normal",
         dist_impact = "species_height")

master_imp <- master_base %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("random", "height_only")) %>%
  bind_rows(.id = "dist_impact") %>%
  mutate(disturbances = "future",
         age_structure = "normal")

master_age <- master_base %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("young", "old")) %>%
  bind_rows(.id = "age_structure") %>% 
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("future", "no")) %>%
  bind_rows(.id = "disturbances") %>%
  mutate(dist_impact = "species_height")



master_revision <- bind_rows(master_age,
                             master_rot,
                             master_imp) %>%
  mutate(replication = as.integer(replication)) %>%
  dplyr::select(landscape,
                ruid, 
                m.unit, 
                climate, 
                scenario, 
                diversity, 
                disturbances, 
                replication, 
                dist_impact, 
                age_structure) %>%
  arrange(., ruid, scenario, diversity)

toc <- master_revision %>%
  dplyr::select(landscape, 
                climate, 
                scenario, 
                diversity, 
                disturbances, 
                replication, 
                dist_impact, 
                age_structure) %>%
  distinct(.) %>%
  arrange(landscape, climate, scenario, diversity, disturbances) %>%
  mutate(experiment = case_when(disturbances %in% c("future_100", "historic_600") ~ 1,
                                age_structure != "normal" ~ 2,
                                dist_impact != "species_height"~ 3),
         run_id = 2520 + as.integer(1:nrow(.)))

write_csv(toc, "revision/table_of_combinations_revision.csv")
#write_csv(toc, "../../materials/data_package_4.0/table_of_combinations_revision.csv")


# join species shares and inital age for experiment 1 and 3

mastertable_org <- read_csv("initalization/mastertable.csv") %>%
  dplyr::select(-disturbances) %>%
  distinct(.)


revision <- master_revision %>%
  left_join(mastertable_org, by = c("landscape", 
                                    "ruid", 
                                    "m.unit",  
                                    "scenario",
                                    "climate",
                                    "diversity",
                                    "replication"))


# Experiment 1 - vary disturbance rotation period -------------------------

# For our first experiment we will simulate two additional disturbance scenarios.
# In the first place we simulated no disturbances, 200 yrs disturbance rotation period
# and 400 yrs disturbance rotation period.
# Therefore, new disturbance files are necessary which will be created in the following.

# disturbance files -------------------------------------------------------

# ROSALIA -----------------------------------------------------------------

# load environment grid

ruid_rosa <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc")

# get resource unit index for both landscapes

ruindex_rosa <- raster("../../materials/ROSALIA/gis/ruindex_rosalia.asc") %>%
  crop(., extent(ruid_rosa))

ruindex_rosa_df <- data.frame(ruid = ruid_rosa[], 
                              ruindex = ruindex_rosa[]) %>% 
  filter(!is.na(ruid))

# define disturbance rotation, mean patch size and landscape size

rot_historic_600 <- 600

rot_future_100 <- 100

modifier <- 2

patch_size_rosa <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017, ISPRS

landscape_size_rosa <- length(unique(ruid_rosa))

# calculate number of disturbance patches per simulation period

sim_period <- 200

landscape_disturbed_rosa_historic <- as.integer(sim_period / rot_historic_600 * landscape_size_rosa) 

landscape_disturbed_rosa_future <- as.integer(sim_period / rot_future_100 * landscape_size_rosa) 

n_patches_rosa_historic <- as.integer(landscape_disturbed_rosa_historic / patch_size_rosa)

n_patches_rosa_future <- as.integer(landscape_disturbed_rosa_future / patch_size_rosa)

# create patches by randomly drawing from the neighbouring ruids

# direct neighbours (8)

offset <- data.frame(dx = c(-1, -1, -1, 0, 0, 1, 1, 1),
                     dy = c(-1, 0, 1, -1, 1, -1, 0, 1))

# neighbours of the direct neighbours (16)

offset2 <- data.frame(dx = c(-2, -2, -2, -2, -2, 2, 2, 2, 2, 2, -1, -1, 0, 0, 1, 1),
                      dy = c(-2, -1, 0, 1, 2, -2, -1, 0, 1,  2, -2, 2, -2, 2, -2, 2))


# neighbours of the indirect neighbours (24)

offset3 <- data.frame(dx = c(-3, -3, -3, -3, -3, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3),
                      dy = c(-3, -2, -1,  0,  1,  2,  3,  3, -3,  3, -3, 3, -3, 3, -3, 3,-3, -3, -2, -1, 0, 1, 2, 3))


# loop through all disturbance patches


disturbances_rosa_historic <- data.frame()
disturbances_rosa_future <- data.frame()


# disturbance_maps_rosa_historic <- stack()
# disturbance_maps_rosa_future <- stack()


#rep <- 1
#i <- 1

# historic disturbance sequence

for (rep in 1:10) {
  
  # make map
  
  #ruids_disturbed_rosa <- ruid_rosa
  
  # create patches
  
  patches_rosa <- rexp(5000, 1/patch_size_rosa) %>%
    round(., 0)
  
  patches_rosa <- patches_rosa[patches_rosa > 0]  
  
  # draw center of disturbance patches randomly
  
  centers_rosa <- sampleRandom(ruid_rosa, n_patches_rosa_historic, rowcol = TRUE) %>%
    as.data.frame(.) %>%
    rename(ruid = EnvGrid_Rosalia)
  
  centers_rosa$patch_size <- patches_rosa[1:n_patches_rosa_historic]
  
  centers_rosa$real_size <- NA
  
  
  for (i in 1:length(centers_rosa$ruid)) {
    
    nbs <- offset[sample(x = 1:8, size = min(centers_rosa$patch_size[i] - 1, 8)),]
    
    if (centers_rosa$patch_size[i] > 9) {
      nbs <- rbind(nbs, offset2[sample(x = 1:16, size = min(centers_rosa$patch_size[i] - 9, 16)), ])
    }
    
    if (centers_rosa$patch_size[i] > 25) {
      nbs <- rbind(nbs, offset3[sample(x = 1:24, size = min(centers_rosa$patch_size [i] - 25, 24)),])
    }
    
    nbs$row <- centers_rosa$row[i] + nbs$dx
    
    nbs$col <- centers_rosa$col[i] + nbs$dy
    
    ruids <- c(centers_rosa$ruid[i],  ruid_rosa[cellFromRowCol(ruid_rosa, nbs$row, nbs$col)]) %>%
      as.data.frame() %>%
      rename(ruid = ".") %>%
      mutate(year = sample(1:sim_period, 1)) %>%
      filter(!is.na(ruid)) 
    
    ruids$replication <- rep
    
    centers_rosa$real_size[i] <- length(ruids$ruid)
    
    disturbances_rosa_historic <- bind_rows(disturbances_rosa_historic, ruids)
    
    #ruids_disturbed_rosa[ruids_disturbed_rosa[] %in% ruids$ruid] <- 20000
    
    
    if (sum(centers_rosa$real_size, na.rm = TRUE) > landscape_disturbed_rosa_historic) {
      print("Störungen fertig!")
      break;
    }
    
  }
  
  #disturbance_maps_rosa_historic <- stack(disturbance_maps_rosa_historic, ruids_disturbed_rosa)
  
}


disturbances_rosa_historic$dist_scenario <- "historic_600"

rm(i, rep)

# future disturbance sequence


for (rep in 1:10) {
  
  # make map
  
  #ruids_disturbed_rosa <- ruid_rosa
  
  # create patches
  
  patches_rosa <- rexp(5000, 1/patch_size_rosa) %>%
    round(., 0) 
  
  patches_rosa <- patches_rosa[patches_rosa > 0]  
  
  # draw center of disturbance patches randomly
  
  centers_rosa <- sampleRandom(ruid_rosa, n_patches_rosa_future, rowcol = TRUE) %>%
    as.data.frame(.) %>%
    rename(ruid = EnvGrid_Rosalia)
  
  centers_rosa$patch_size <- patches_rosa[1:n_patches_rosa_future]
  
  centers_rosa$real_size <- NA
  
  
  for (i in 1:length(centers_rosa$ruid)) {
    
    nbs <- offset[sample(x = 1:8, size = min(centers_rosa$patch_size[i] - 1, 8)), ]
    
    if (centers_rosa$patch_size[i] > 9) {
      nbs <- rbind(nbs, offset2[sample(x = 1:16, size = min(centers_rosa$patch_size[i]-9, 16)), ])
    }
    
    if (centers_rosa$patch_size[i] > 25) {
      nbs <- rbind(nbs, offset3[sample(x = 1:24, size = min(centers_rosa$patch_size [i] - 25, 24)), ])
    }
    
    nbs$row <- centers_rosa$row[i] + nbs$dx
    
    nbs$col <- centers_rosa$col[i] + nbs$dy
    
    ruids <- c(centers_rosa$ruid[i],  ruid_rosa[cellFromRowCol(ruid_rosa, nbs$row, nbs$col)]) %>%
      as.data.frame() %>%
      rename(ruid = ".") %>%
      mutate(year = sample(1:sim_period, 1)) %>%
      filter(!is.na(ruid)) 
    
    ruids$replication <- rep
    
    centers_rosa$real_size[i] <- length(ruids$ruid)
    
    disturbances_rosa_future <- bind_rows(disturbances_rosa_future, ruids)
    
    #   ruids_disturbed_rosa[ruids_disturbed_rosa[] %in% ruids$ruid] <- 20000
    
    
    if (sum(centers_rosa$real_size, na.rm = TRUE) > landscape_disturbed_rosa_future) {
      print("Störungen fertig!")
      break;
    }
    
  }
  
  #  disturbance_maps_rosa_future <- stack(disturbance_maps_rosa_future, ruids_disturbed_rosa)
  
}

disturbances_rosa_future$dist_scenario <- "future_100"

rm(i, rep)

# check if loop worked correctly

ggplot(disturbances_rosa_historic, aes(x = as.factor(replication))) +
  geom_bar()

ggplot(disturbances_rosa_future, aes(x = as.factor(replication))) +
  geom_bar()
# 
# plot(disturbance_maps_rosa_historic[[1]])
# 
# plot(disturbance_maps_rosa_future[[1]])


# bind historic and future sequence together

disturbances_rosalia <- disturbances_rosa_historic %>%
  bind_rows(disturbances_rosa_future) %>%
  mutate(landscape = "rosalia") %>%
  left_join(ruindex_rosa_df, by = "ruid")

#write_csv(disturbances_rosalia, "revision/disturbance_files/disturbances_rosalia.csv")

# writeRaster(disturbance_maps_rosa_historic, 
#             "disturbances/disturbances_maps/rosalia/historic/disturbance_maps_rosalia_historic", 
#             bylayer = TRUE,
#             format = "GTiff")
# 
# writeRaster(disturbance_maps_rosa_future, 
#             "disturbances/disturbances_maps/rosalia/future/disturbance_maps_rosalia_future",
#             bylayer = TRUE,
#             format = "GTiff")



# DISCHMA -----------------------------------------------------------------


# load environment grid

ruid_disch <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc")
plot(ruid_disch)

# get resource unit index for both landscapes

ruindex_dischma <- raster("../../materials/DISCHMA/gis/ruindex_dischma.asc") %>%
  crop(., extent(ruid_disch))

ruindex_disch_df <- data.frame(ruid = ruid_disch[], 
                               ruindex = ruindex_dischma[]) %>% 
  filter(!is.na(ruid)) 

# define disturbance rotation, mean patch size and landscape size

rot_historic_600 <- 600

rot_future_100 <- 100

modifier <- 2

patch_size_disch <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017,ISPRS

landscape_size_disch <- length(unique(ruid_disch))

# calculate number of disturbance patches per simulation period

sim_period <- 200

landscape_disturbed_disch_historic <- as.integer(sim_period / rot_historic_600 * landscape_size_disch) 

landscape_disturbed_disch_future <- as.integer(sim_period / rot_future_100 * landscape_size_disch) 

n_patches_disch_historic <- as.integer(landscape_disturbed_disch_historic / patch_size_disch)

n_patches_disch_future <- as.integer(landscape_disturbed_disch_future / patch_size_disch)

# create patches by randomly drawing from the neighbouring ruids

# direct neighbours (8)

offset <- data.frame(dx = c(-1, -1, -1, 0, 0, 1, 1, 1),
                     dy = c(-1, 0, 1, -1, 1, -1, 0, 1))

# neighbours of the direct neighbours (16)

offset2 <- data.frame(dx = c(-2, -2, -2, -2, -2, 2, 2, 2, 2, 2, -1, -1, 0, 0, 1, 1),
                      dy = c(-2, -1, 0, 1, 2, -2, -1, 0, 1,  2, -2, 2, -2, 2, -2, 2))


# neighbours of the indirect neighbours (24)

offset3 <- data.frame(dx = c(-3, -3, -3, -3, -3, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3),
                      dy = c(-3, -2, -1,  0,  1,  2,  3,  3, -3,  3, -3, 3, -3, 3, -3, 3,-3, -3, -2, -1, 0, 1, 2, 3))


# loop through all disturbance patches


disturbances_disch_historic <- data.frame()
disturbances_disch_future <- data.frame()


# disturbance_maps_disch_historic <- stack()
# disturbance_maps_disch_future <- stack()


#rep <- 1
#i <- 48

# historic disturbance sequence

for (rep in 1:10) {
  
  # make map
  
  #ruids_disturbed_disch <- ruid_disch
  
  # create patches
  
  patches_disch <- rexp(5000, 1/patch_size_disch) %>%
    round(., 0) 
  
  patches_disch <- patches_disch[patches_disch > 0]  
  
  # draw center of disturbance patches randomly
  
  centers_disch <- sampleRandom(ruid_disch, n_patches_disch_historic, rowcol = TRUE) %>%
    as.data.frame(.) %>%
    rename(ruid = EnvGrid_Dischma)
  
  centers_disch$patch_size <- patches_disch[1:n_patches_disch_historic]
  
  centers_disch$real_size <- NA
  
  
  for (i in 1:length(centers_disch$ruid)) {
    
    nbs <- offset[sample(x = 1:8, size = min(centers_disch$patch_size[i] - 1, 8)),]
    
    if (centers_disch$patch_size[i] > 9) {
      nbs <- rbind(nbs, offset2[sample(x = 1:16, size = min(centers_disch$patch_size[i]-9, 16)), ])
    }
    
    if (centers_disch$patch_size[i] > 25) {
      nbs <- rbind(nbs, offset3[sample(x = 1:24, size = min(centers_disch$patch_size [i] - 25, 24)),])
    }
    
    nbs$row <- centers_disch$row[i] + nbs$dx
    
    nbs$col <- centers_disch$col[i] + nbs$dy
    
    ruids <- c(centers_disch$ruid[i],  ruid_disch[cellFromRowCol(ruid_disch, nbs$row, nbs$col)]) %>%
      as.data.frame() %>%
      rename(ruid = ".") %>%
      mutate(year = sample(1:sim_period, 1)) %>%
      filter(!is.na(ruid)) 
    
    ruids$replication <- rep
    
    centers_disch$real_size[i] <- length(ruids$ruid)
    
    disturbances_disch_historic <- bind_rows(disturbances_disch_historic, ruids)
    
    #ruids_disturbed_disch[ruids_disturbed_disch[] %in% ruids$ruid] <- 20000
    
    
    if (sum(centers_disch$real_size, na.rm = TRUE) > landscape_disturbed_disch_historic) {
      print("Störungen fertig!")
      break;
    }
    
  }
  
  #disturbance_maps_disch_historic <- stack(disturbance_maps_disch_historic, ruids_disturbed_disch)
  
}


disturbances_disch_historic$dist_scenario <- "historic_600"

rm(i, rep)

# future disturbance sequence


for (rep in 1:10) {
  
  # make map
  
  #ruids_disturbed_disch <- ruid_disch
  
  # create patches
  
  patches_disch <- rexp(5000, 1/patch_size_disch) %>%
    round(., 2) 
  
  patches_disch <- patches_disch[patches_disch > 0]  
  
  # draw center of disturbance patches randomly
  
  centers_disch <- sampleRandom(ruid_disch, n_patches_disch_future, rowcol = TRUE) %>%
    as.data.frame(.) %>%
    rename(ruid = EnvGrid_Dischma)
  
  centers_disch$patch_size <- patches_disch[1:n_patches_disch_future]
  
  centers_disch$real_size <- NA
  
  
  for (i in 1:length(centers_disch$ruid)) {
    
    nbs <- offset[sample(x = 1:8, size = min(centers_disch$patch_size[i] - 1, 8)), ]
    
    if (centers_disch$patch_size[i] > 9) {
      nbs <- rbind(nbs, offset2[sample(x = 1:16, size = min(centers_disch$patch_size[i]-9, 16)), ])
    }
    
    if (centers_disch$patch_size[i] > 25) {
      nbs <- rbind(nbs, offset3[sample(x = 1:24, size = min(centers_disch$patch_size [i] - 25, 24)), ])
    }
    
    nbs$row <- centers_disch$row[i] + nbs$dx
    
    nbs$col <- centers_disch$col[i] + nbs$dy
    
    ruids <- c(centers_disch$ruid[i],  ruid_disch[cellFromRowCol(ruid_disch, nbs$row, nbs$col)]) %>%
      as.data.frame() %>%
      rename(ruid = ".") %>%
      mutate(year = sample(1:sim_period, 1)) %>%
      filter(!is.na(ruid)) 
    
    ruids$replication <- rep
    
    centers_disch$real_size[i] <- length(ruids$ruid)
    
    disturbances_disch_future <- bind_rows(disturbances_disch_future, ruids)
    
    #ruids_disturbed_disch[ruids_disturbed_disch[] %in% ruids$ruid] <- 20000
    
    
    if (sum(centers_disch$real_size, na.rm = TRUE) > landscape_disturbed_disch_future) {
      print("Störungen fertig!")
      break;
    }
    
  }
  
  #disturbance_maps_disch_future <- stack(disturbance_maps_disch_future, ruids_disturbed_disch)
  
}

disturbances_disch_future$dist_scenario <- "future_100"

rm(i, rep)

# check if loop worked correctly

# ggplot(disturbances_disch_historic, aes(x = as.factor(replication))) +
#   geom_bar()
# 
# ggplot(disturbances_disch_future, aes(x = as.factor(replication))) +
#   geom_bar()
# 
# plot(disturbance_maps_disch_historic[[1]])
# 
# plot(disturbance_maps_disch_future[[1]])


# bind historic and future sequence together

disturbances_dischma <- disturbances_disch_historic %>%
  bind_rows(disturbances_disch_future) %>%
  mutate(landscape = "dischma") %>%
  left_join(ruindex_disch_df, by = "ruid")

write_csv(disturbances_dischma, "revision/disturbances_dischma.csv")


# write disturbance files for iLand

disturbances_rosalia <- read_csv("revision/disturbance_files/disturbances_rosalia.csv")

disturbances_dischma <- read_csv("revision/disturbance_files/disturbances_dischma.csv")


disturbance_files <- bind_rows(disturbances_rosalia, disturbances_dischma) %>%
  split(list(.$landscape, .$replication, .$dist_scenario), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., ruid,                              
                                           ruindex,
                                           year)), 
       ~ write_csv(.y, paste0("../../materials/",
                              unique(.x$landscape),
                              "/scripts/disturbance_files/revision/disturbances_revision_", 
                              unique(.x$landscape), "_",
                              unique(.x$dist_scenario), "_",
                              unique(.x$replication),
                              ".csv")))

# write disturbance files for LandClim  

disturbance_files <- bind_rows(disturbances_rosalia, disturbances_dischma) %>%
  split(list(.$landscape, .$replication, .$dist_scenario), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., ruid, 
                                           year)), 
       ~ write_csv(.y, paste0("revision/disturbance_files/disturbances_revision_",           
                              unique(.x$landscape), "_",
                              unique(.x$dist_scenario), "_",
                              unique(.x$replication),
                              ".csv"))) 








# Experiment 2 - vary age structure-----------------------------------------------------

# Experiment 2 varies the inital age class strucutre of the two landscapes
# in order to quantify the sensitivity of our results to differences in the 
# age class structure

# Additional to the normal forest age class structure (mean age = 50 in rosalia
# and 75 in Dischma) simulated in the first draft we simulate a scenario with more 
# young stands (mean age = 30 rosalia and 50 dischma) and more old stands (mean age
# = 70 in rosalia and 100 in Dischma)

# Therefore new initalization and management files are necessary



# initalization files -----------------------------------------------------

# define inital age

inital <- filter(revision, age_structure != "normal") %>% 
  dplyr::select(landscape, m.unit, ruid, diversity, scenario, age_structure) %>%
  distinct(.) 

# Instead of sampling from a uniform distirbution (as in the first place) we will sample
# the intial age of every stand from a combination of a Weibull function and a offset

# Weibull-Function

curve(dweibull(x, shape = 4, scale = 1), from = 0, to = 1)

# ... and a "offset" - flattening down the function (towards uniform-random)

# generate 100 bins

x <- dweibull((1:100)/100, shape = 4, scale = 1) + 0.35 

x <- x/ sum(x)# sum = 1


# sample from function, factor 1.5 scales to 150 years rotation period

young_dischma <- inital %>%
  filter(landscape == "dischma", age_structure == "young") %>%
  mutate(init.age = 151 - sample(x = 1:100, size = nrow(.), prob = x, replace = T) * 1.5) 

old_dischma <- inital %>%
  filter(landscape == "dischma", age_structure == "old") %>%
  mutate(init.age = sample(x = 1:100, size = nrow(.), prob = x, replace = T)* 1.5)

young_rosalia <- inital %>%
  filter(landscape == "rosalia", age_structure == "young") %>%
  mutate(init.age = 100 - sample(x = 1:100, size = nrow(.), prob = x, replace = T)) 

old_rosalia <- inital %>%
  filter(landscape == "rosalia", age_structure == "old") %>%
  mutate(init.age = sample(x = 1:100, size = nrow(.), prob = x, replace = T))


master_init_age <- bind_rows(young_dischma, 
                             old_dischma,
                             young_rosalia,
                             old_rosalia) %>%
  mutate(init.age = as.integer(init.age)) %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("historic", "RCP85")) %>%
  bind_rows(.id = "climate") %>%
  replicate(n = 10, simplify = FALSE) %>%
  set_names(1:10) %>%
  bind_rows(.id = "replication") %>%
  mutate(replication = as.integer(replication)) %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("future", "no")) %>%
  bind_rows(.id = "disturbances") %>%
  mutate(dist_impact = "species_height") %>%
  left_join(dplyr::select(mastertable_org, -init.age), by = c("landscape", 
                                    "ruid", 
                                    "m.unit", 
                                    "climate",
                                    "scenario",
                                    "diversity",
                                    "replication"))


master_init_age %>%
  dplyr::select(-m.unit,-ruid, -init.age, -(abal:tico)) %>%
  distinct(.) %>%
  nrow(.) %>%
  print()


ggplot(master_init_age, aes(x = init.age)) +
  geom_bar() +
  facet_grid(landscape ~ age_structure, scales= "free")


# check if all rows sum up to 1

test <- master_init_age %>%
  mutate(rs = rowSums(dplyr::select(., abal:tico))) 



unique(test$rs)


# final mastertable of revision

mastertable_revision <- revision %>%
  filter(age_structure == "normal") %>%
  bind_rows(master_init_age)
  

write_csv(mastertable_revision, "revision/mastertable_revision.csv")


# initalization files for iLand and LandClim 


species_lookup <- read.table("tree_data/tree_data.txt", header = TRUE, stringsAsFactors = FALSE)


dbhcls.lookup <- read.table(text="dbh.class	dbhfrom	dbhto	dbhmean
if_dbh_5_1_0_sum	3	5	2.5
 if_dbh_5_and_dbh_10_1_0_sum	5	10	7.5
 if_dbh_10_and_dbh_15_1_0_sum	10	15	12.5
 if_dbh_15_and_dbh_20_1_0_sum	15	20	17.5
 if_dbh_20_and_dbh_25_1_0_sum	20	25	22.5
 if_dbh_25_and_dbh_30_1_0_sum	25	30	27.5
 if_dbh_30_and_dbh_35_1_0_sum	30	35	32.5
 if_dbh_35_and_dbh_40_1_0_sum	35	40	37.5
 if_dbh_40_and_dbh_45_1_0_sum	40	45	42.5
 if_dbh_45_and_dbh_50_1_0_sum	45	50	47.5
 if_dbh_50_and_dbh_55_1_0_sum	50	55	52.5
 if_dbh_55_and_dbh_60_1_0_sum	55	60	57.5
 if_dbh_60_and_dbh_65_1_0_sum	60	65	62.5
 if_dbh_65_and_dbh_70_1_0_sum	65	70	67.5
 if_dbh_70_and_dbh_75_1_0_sum	70	75	72.5
if_dbh_75_1_0_sum	75	100	77.5
", header = TRUE, stringsAsFactors = FALSE)

#spec <- "abal"


# ROSALIA -----------------------------------------------------------------



# rosalia 

species_rosalia <- filter(species_lookup, landscape == "rosalia")[,1]


all.inits.rosa <- list()

for (spec in species_rosalia) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/ROSALIA/output/tree_data/TREE_DATA_ROSALIA_", spec, ".sqlite", 
                                                 collapse = "")) 
  dyn.stand <- dbReadTable(db.conn, "dynamicstand")
  dyn.stand$species <- spec
  dbDisconnect(db.conn)
  
  treedat <- dyn.stand %>%
    gather(key = dbh.class, value = stemnumber, if_dbh_5_1_0_sum:if_dbh_75_1_0_sum) %>%
    left_join(dbhcls.lookup) %>% 
    filter(stemnumber > 0) %>%
    mutate(hd = height_mean/dbh_mean,
           height_class = hd * dbhmean,
           age = year + 5,
           landscape = "rosalia") %>%
    dplyr::select(landscape, age, species, rid, stemnumber, dbhfrom, dbhto, hd, height_class)
  
  
  
  
  all.inits.rosa[[spec]] <- treedat
  
  
}


# dischma

species_dischma <- filter(species_lookup, landscape == "dischma")[,1]


all.inits.disch <- list()


for (spec in species_dischma) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/DISCHMA/output/tree_data/TREE_DATA_DISCHMA_", spec, ".sqlite", 
                                                 collapse = "")) 
  dyn.stand <- dbReadTable(db.conn, "dynamicstand")
  dyn.stand$species <- spec
  dbDisconnect(db.conn)
  
  treedat <- dyn.stand %>%
    gather(key = dbh.class, value = stemnumber, if_dbh_5_1_0_sum:if_dbh_75_1_0_sum) %>%
    left_join(dbhcls.lookup) %>% 
    filter(stemnumber > 0) %>%
    mutate(hd = height_mean/dbh_mean,
           height_class = hd * dbhmean,
           age = year + 5,
           landscape = "dischma") %>%
    dplyr::select(landscape, age, species, rid, stemnumber, dbhfrom, dbhto, hd, height_class)
  
  
  
  
  all.inits.disch[[spec]] <- treedat
  
  
}


all.inits.rosa_df <- bind_rows(all.inits.rosa)

all.inits.disch_df <- bind_rows(all.inits.disch)

all.inits.df <- bind_rows(all.inits.rosa_df, all.inits.disch_df) %>%
  rename(ruid = rid) %>%
  arrange(., landscape, ruid, species, age)


initialization <- master_init_age %>%
  dplyr::select(landscape, 
                m.unit, 
                ruid, 
                diversity, 
                scenario, 
                age_structure, 
                init.age, 
                abal:tico) %>%
  distinct(.) %>%
  gather(species, share, abal:tico) %>%
  filter(share > 0) %>%
  rename(age = init.age) %>%
  left_join(all.inits.df, by = c("landscape", "species", "ruid", "age")) %>%
  mutate(stem_number_reduced = round((stemnumber * share),0),
         hd = hd * 100) %>%
  filter((stem_number_reduced != 0 | is.na(stem_number_reduced))) %>%
  arrange(., landscape, ruid, scenario)

all.inits.df_max <- all.inits.df %>%
  group_by(landscape, species, ruid, age) %>%
  filter(., sum(stemnumber) > 30) %>%
  ungroup(.) %>%
  group_by(landscape, species, ruid) %>%
  filter(., age == max(age)) %>%
  ungroup(.) %>%
  arrange(., landscape, ruid, species, age)


missing_old_trees <- initialization %>%
  filter(is.na(stemnumber)) %>%
  filter(age > 50) %>%
  dplyr::select(-(stemnumber:stem_number_reduced), -age) %>%
  left_join(all.inits.df_max, by = c("landscape", "ruid", "species")) %>%
  mutate(stem_number_reduced = round((stemnumber * share),0),
         hd = hd * 100) %>%
  filter((stem_number_reduced != 0 | is.na(stem_number_reduced)))


# full initialization table

initialization_full <- initialization %>%
  filter(! (is.na(stemnumber) & age > 50)) %>%
  bind_rows(missing_old_trees) %>%
  arrange(., landscape, ruid, scenario) %>%
  dplyr::select(landscape, 
                ruid,
                m.unit, 
                scenario,
                diversity,
                age_structure,
                age,
                species,
                share,
                stemnumber,
                dbhfrom,
                dbhto,
                hd,
                height_class,
                stem_number_reduced)

write_csv(initialization_full, "revision/initialization_full_revision.csv")


old <- read_csv("initalization/initialization_full.csv")
# check how age class distribution looks without old missing trees

ggplot(initialization_full, aes(x = age)) +
  geom_bar() +
  facet_grid(landscape ~ age_structure, scales = "free")


# write initalization file for tim

initalization_files <- initialization_full %>%
  mutate_at(.vars = vars(hd, height_class), function(x) round(x, 1)) %>%
  split(list(.$landscape, .$scenario, .$diversity, .$age_structure), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(.,ruid, 
                                           age, 
                                           species, 
                                           count = stem_number_reduced, 
                                           dbh_from = dbhfrom, 
                                           dbh_to = dbhto, 
                                           hd, 
                                           height = height_class)), 
       ~ write_csv(.y, paste0("revision/initialization_files/initialization_revision_", 
                              unique(.x$landscape), "_", 
                              unique(.x$age_structure), "_",
                              unique(.x$scenario), "_",
                              unique(.x$diversity),
                              ".csv")))


# create init files for iLand

init_files_iland <- initialization_full %>%
  na.omit(.) %>%  #### remove
  mutate_at(.vars = vars(hd, height_class), function(x) round(x, 1)) %>%
  split(list(.$landscape, .$scenario, .$diversity, .$age_structure), drop = TRUE) %>%
  map(., ~ arrange(., ruid)) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., stand_id = ruid, 
                                           age, 
                                           species, 
                                           count = stem_number_reduced, 
                                           dbh_from = dbhfrom, 
                                           dbh_to = dbhto, 
                                           hd, 
                                           height = height_class)), 
       ~ write_csv(.y, paste0("../../materials/", 
                              unique(toupper(.x$landscape)), 
                              "/init/init_files/revision/", 
                              unique(.x$landscape), "_", 
                              unique(.x$age_structure), "_",
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))

# create sapling init files for iLand  

sapling_files_iland <- initialization_full %>%
  filter(is.na(stemnumber) & ! is.na(age)) %>%
  mutate(stemnumber = 130000,
         stem_number_reduced = stemnumber * share,
         height_from = round(age * 0.1, 2),
         height_to = round(age * 0.16, 2)) %>%
  mutate(height_from = case_when(height_from > 4 ~ 3.5,
                                 TRUE ~ height_from),
         height_to = case_when(height_to > 4 ~ 4,
                               TRUE ~ height_to)) %>%
  split(list(.$landscape, .$scenario, .$diversity, .$age_structure), drop = TRUE) %>%
  map(., ~ arrange(., ruid)) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., stand_id = ruid,
                                           species,  
                                           count = stem_number_reduced, 
                                           age, 
                                           height_from, 
                                           height_to)), 
       ~ write_csv(.y, paste0("../../materials/", 
                              unique(toupper(.x$landscape)), 
                              "/init/sapling_lists/final_simulations/revision/", 
                              unique(.x$landscape), "_", 
                              unique(.x$age_structure), "_",
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))

# management sequences --------------------------------------------------------------

# the management sequences define were and when thinnings, clear cuts and plantings happen

# define rotation periods

rotation_rosa <- 100
rotation_disch <- 150

# get inital states

mastertable_revision <- read_csv("revision/mastertable_revision.csv") %>%
  filter(age_structure != "normal")

manage <- mastertable_revision %>%
  dplyr::select(landscape, ruid, scenario, diversity, init.age, age_structure) %>%
  distinct(.)

# calculate year of mangement intervention

manage_rosa <- filter(manage, landscape == "rosalia") %>%
  mutate(DF1 = as.integer(0.35 * rotation_rosa) - init.age,
         DF2 = as.integer(0.55 * rotation_rosa) - init.age,
         clearcut.I = rotation_rosa - init.age,
         clearcut.II = 2 * rotation_rosa - init.age,
         DF1.I = clearcut.I + as.integer(0.35 * rotation_rosa),
         DF2.I = clearcut.I + as.integer(0.55 * rotation_rosa),
         DF1.II = clearcut.II + as.integer(0.35 * rotation_rosa),
         DF2.II = clearcut.II + as.integer(0.55 * rotation_rosa))

manage_disch <- filter(manage, landscape == "dischma") %>%
  mutate(DF1 = as.integer(0.35 * rotation_disch) - init.age,
         DF2 = as.integer(0.55 * rotation_disch) - init.age,
         clearcut.I = rotation_disch - init.age,
         clearcut.II = 2 * rotation_disch - init.age,
         DF1.I = clearcut.I + as.integer(0.35 * rotation_disch),
         DF2.I = clearcut.I + as.integer(0.55 * rotation_disch),
         DF1.II = clearcut.II + as.integer(0.35 * rotation_disch),
         DF2.II = clearcut.II + as.integer(0.55 * rotation_disch))

# combine both landscapes


management <- bind_rows(manage_rosa, manage_disch) %>%
  gather(., key = activity, value = sim_year, -(landscape:age_structure)) %>%
  filter(sim_year %in% 0:200) %>%
  arrange(., landscape, ruid, scenario, diversity, init.age) 

# tidy names in managment actions

management$activity <- gsub(".I", "", management$activity)
management$activity <- gsub("I", "", management$activity)


# write management files for LandClim

management_files <- management %>%
  left_join(dplyr::select(mastertable_revision, 
                          landscape, 
                          ruid, 
                          scenario, 
                          diversity,
                          age_structure,
                          init.age,
                          abal:tico) %>%
              distinct(.), 
            by = c("landscape", "ruid", "scenario", "diversity", "init.age", "age_structure")) %>%
  mutate(activity = case_when(activity %in% c("DF1", "DF2") ~ "thinning",
                              TRUE ~ activity)) %>%
  split(list(.$landscape, .$scenario, .$diversity, .$age_structure), drop = TRUE) %>%
  map(., ~ write_csv(., paste0("revision/management_files/management_", 
                               unique(.$landscape), "_",
                               unique(.$age_structure), "_",
                               unique(.$scenario), "_", 
                               unique(.$diversity),
                               ".csv")))

# write management files for iLand

# get resource unit id for both landscapes

ruid_rosa <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc")

ruid_disch <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc")


# get resource unit index for both landscapes

ruindex_rosa <- raster("../../materials/ROSALIA/gis/ruindex_rosalia.asc") %>%
  crop(., extent(ruid_rosa))

ruindex_dischma <- raster("../../materials/DISCHMA/gis/ruindex_dischma.asc") %>%
  crop(., extent(ruid_disch))


ruindex_rosa_df <- data.frame(ruid = ruid_rosa[], 
                              ruindex = ruindex_rosa[]) %>% 
  filter(!is.na(ruid))


ruindex_df <- data.frame(ruid = ruid_disch[], 
                         ruindex = ruindex_dischma[]) %>% 
  filter(!is.na(ruid)) %>%
  bind_rows(., ruindex_rosa_df)

management_files_iland <- management %>%
  mutate(activity = case_when(activity %in% c("DF1", "DF2") ~ "thinning",
                              TRUE ~ activity)) %>%
  left_join(ruindex_df, by = "ruid") %>%
  left_join(dplyr::select(mastertable_revision, 
                          landscape, 
                          ruid, 
                          scenario, 
                          diversity, 
                          age_structure,
                          init.age, 
                          abal:tico) %>%
              distinct(.), 
            by = c("landscape", "ruid", "scenario", "diversity", "init.age", "age_structure")) %>%
  split(list(.$landscape, .$scenario, .$diversity, .$age_structure), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(.,ruid, 
                                           ruindex, 
                                           activity, 
                                           year = sim_year,
                                           abal:tico)), 
       ~ write_csv(.y, paste0("../../materials/", 
                              unique(toupper(.x$landscape)), 
                              "/scripts/management_files/revision/management_", 
                              unique(.x$landscape), "_", 
                              unique(.x$age_structure), "_",
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))






# Experiment 3 - vary disturbance impact ----------------------------------

# Here only the functions were changed directly in the two models

# Batch file --------------------------------------------------------------

### This R-script creates a batch file for running the final simulations of the 
### FORMASAM study


# load table of all combinations

toc <- read_csv("revision/table_of_combinations_revision.csv")



# Experiment 1

# ROSALIA

toc_rosa_1 <- toc %>%
  filter(landscape == "rosalia" & experiment == 1) 

setup_rosa_1 <-  "$ILAND ROSALIA/xmls/FORMASAM_ROSALIA_"

setup_rosa_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_rosa <- "system.database.out=FORMASAM_ROSALIA_" 

log_rosa <- " system.logging.logFile=../log/FORMASAM_ROSALIA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"

management <- "model.management.file=../scripts/FORMASAM_ROSALIA_REV_EXP_1.js"

batch_file_rosa_1 <- toc_rosa_1 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_rosa_1,
                        climate,
                        setup_rosa_2,
                        output_rosa, run_id, ".sqlite", 
                        log_rosa, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        management, " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))



batch_file_rosa_1$batch[1]

# Experiment 2

# ROSALIA

toc_rosa_2 <- toc %>%
  filter(landscape == "rosalia" & experiment == 2) 

setup_rosa_1 <-  "$ILAND ROSALIA/xmls/FORMASAM_ROSALIA_"

setup_rosa_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_rosa <- "system.database.out=FORMASAM_ROSALIA_" 

log_rosa <- " system.logging.logFile=../log/FORMASAM_ROSALIA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/revision/"

init <- "model.initialization.file=../init/init_files/revision/"

management <- "model.management.file=../scripts/FORMASAM_ROSALIA_REV_EXP_2.js"

batch_file_rosa_2 <- toc_rosa_2 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_rosa_1,
                        climate,
                        setup_rosa_2,
                        output_rosa, run_id, ".sqlite", 
                        log_rosa, run_id, ".txt",
                        sapling, paste(landscape, age_structure, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, age_structure, scenario, diversity, sep = "_"), ".csv ",
                        management, " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))


batch_file_rosa_2$batch[1]



# Experiment 3

# ROSALIA

toc_rosa_3 <- toc %>%
  filter(landscape == "rosalia" & experiment == 3) 

setup_rosa_1 <-  "$ILAND ROSALIA/xmls/FORMASAM_ROSALIA_"

setup_rosa_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_rosa <- "system.database.out=FORMASAM_ROSALIA_" 

log_rosa <- " system.logging.logFile=../log/FORMASAM_ROSALIA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"

management <- "model.management.file=../scripts/FORMASAM_ROSALIA_REV_EXP_3"

batch_file_rosa_3 <- toc_rosa_3 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_rosa_1,
                        climate,
                        setup_rosa_2,
                        output_rosa, run_id, ".sqlite", 
                        log_rosa, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        paste0(management, "_", dist_impact, ".js"), " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))


batch_file_rosa_3$batch[1]

# DISCHMA


# Experiment 1

toc_disch_1 <- toc %>%
  filter(landscape == "dischma" & experiment == 1) 

setup_disch_1 <-  "$ILAND DISCHMA/xmls/FORMASAM_DISCHMA_"

setup_disch_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_disch <- "system.database.out=FORMASAM_DISCHMA_" 

log_disch <- " system.logging.logFile=../log/FORMASAM_DISCHMA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"

management <- "model.management.file=../scripts/FORMASAM_DISCHMA_REV_EXP_1.js"


batch_file_disch_1 <- toc_disch_1 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_disch_1,
                        climate,
                        setup_disch_2,
                        output_disch, run_id, ".sqlite", 
                        log_disch, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        management, " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))


batch_file_disch_1$batch[1]



# Experiment 2


toc_disch_2 <- toc %>%
  filter(landscape == "dischma" & experiment == 2) 

setup_disch_1 <-  "$ILAND DISCHMA/xmls/FORMASAM_DISCHMA_"

setup_disch_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_disch <- "system.database.out=FORMASAM_DISCHMA_" 

log_disch <- " system.logging.logFile=../log/FORMASAM_DISCHMA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/revision/"

init <- "model.initialization.file=../init/init_files/revision/"

management <- "model.management.file=../scripts/FORMASAM_DISCHMA_REV_EXP_2.js"


batch_file_disch_2 <- toc_disch_2 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_disch_1,
                        climate,
                        setup_disch_2,
                        output_disch, run_id, ".sqlite", 
                        log_disch, run_id, ".txt",
                        sapling, paste(landscape, age_structure, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, age_structure, scenario, diversity, sep = "_"), ".csv ",
                        management, " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))


batch_file_disch_2$batch[1]


# Experiment 3


toc_disch_3 <- toc %>%
  filter(landscape == "dischma" & experiment == 3) 

setup_disch_1 <-  "$ILAND DISCHMA/xmls/FORMASAM_DISCHMA_"

setup_disch_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_disch <- "system.database.out=FORMASAM_DISCHMA_" 

log_disch <- " system.logging.logFile=../log/FORMASAM_DISCHMA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"

management <- "model.management.file=../scripts/FORMASAM_DISCHMA_REV_EXP_3"


batch_file_disch_3 <- toc_disch_3 %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_disch_1,
                        climate,
                        setup_disch_2,
                        output_disch, run_id, ".sqlite", 
                        log_disch, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        paste0(management, "_", dist_impact, ".js"), " ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.age_structure=", age_structure, " ",
                        "user.run_id=", run_id, " "))


batch_file_disch_3$batch[1]




# combine batch files



batch_file_rosa <- bind_rows(batch_file_rosa_1,
                             batch_file_rosa_2,
                             batch_file_rosa_3) %>%
  dplyr::select(batch) %>%
  write.table(., "../../methods/r/batch_file/batch_rosa_revision.txt",
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE)



batch_file_disch <- bind_rows(batch_file_disch_1,
                             batch_file_disch_2,
                             batch_file_disch_3) %>%
  dplyr::select(batch) %>%
  write.table(., "../../methods/r/batch_file/batch_disch_revision.txt",
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE)


# data processing revision ------------------------------------------------

### This script analyzes the results of the first FORMASAM test run

library(RSQLite)
library(tidyverse)
library(data.table)
library(patchwork)
library(ggthemes)
library(gtools)
library(tictoc)


# ILAND -------------------------------------------------------------------

# load toc

toc <- read.csv("revision/table_of_combinations_revision.csv")


# Bring iLand output in FORMASAM format -----------------------------------

# rosalia

toc_rosalia <- filter(toc, landscape == "rosalia") 

output_rosa <- list()

#runid <- 3452

for (runid in toc_rosalia$run_id) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/ROSALIA/output/final_simulations/revision/FORMASAM_ROSALIA_", runid , ".sqlite", 
                                                 collapse = "")) 
  dyn.stand <- dbReadTable(db.conn, "dynamicstand")
  dyn.stand$run_id <- runid
  dbDisconnect(db.conn)
  
  
  output_rosa[[runid]] <- dyn.stand
  
  print(runid)
}


output_rosalia_iland <- output_rosa %>%
  bind_rows() %>%
  mutate(model = "ILAND",
         AGB = rowSums(dplyr::select(.,
                                     stemmass_sum,
                                     foliagemass_sum,
                                     branchmass_sum))) %>%
  rename(big_trees = if_dbh_30_1_0_sum,
         ruid = rid) %>%
  left_join(toc_rosalia) %>%
  dplyr::select(landscape,
                model,
                ruid,
                year,
                species,
                climate,
                scenario,
                diversity,
                disturbances,
                dist_impact,
                age_structure,
                experiment,
                run_id,
                replication,
                AGB,
                big_trees)


write_csv(output_rosalia_iland, "final_simulations/iland_output/rosalia/output_rosalia_iland_revision.csv")

rm(runid)



# dischma

toc_dischma <- filter(toc, landscape == "dischma") 

output_disch <- list()

#runid <- 1

for (runid in toc_dischma$run_id) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/DISCHMA/output/final_simulations/revision/FORMASAM_DISCHMA_", runid , ".sqlite", 
                                                 collapse = "")) 
  dyn.stand <- dbReadTable(db.conn, "dynamicstand")
  dyn.stand$run_id <- runid
  dbDisconnect(db.conn)
  
  output_disch[[runid]] <- dyn.stand
  
  print(runid)
  
}

rm(runid)

rstudioapi::documentSave()

output_dischma_iland <- output_disch %>%
  bind_rows() %>%
  mutate(model = "ILAND",
         AGB = rowSums(dplyr::select(.,
                                     stemmass_sum,
                                     foliagemass_sum,
                                     branchmass_sum))) %>%
  rename(big_trees = if_dbh_30_1_0_sum,
         ruid = rid) %>%
  left_join(toc_dischma) %>%
  dplyr::select(landscape,
                model,
                ruid,
                year,
                species,
                climate,
                scenario,
                diversity,
                disturbances,
                dist_impact,
                age_structure,
                experiment,
                run_id,
                replication,
                AGB,
                big_trees)


rstudioapi::documentSave()

write_csv(output_dischma_iland, "final_simulations/iland_output/dischma/output_dischma_iland_revision.csv")



# LANDCLIM ----------------------------------------------------------------

toc <- read_csv("revision/table_of_combinations_revision.csv")

# create one big output table per landscape

# rosalia

out_rosa <- list.files("final_simulations/landclim_output/rosalia/raw_output_revision/", pattern = "*.csv") %>%
  mixedsort(.)

output_rosa_lanclim <- lapply(paste0("final_simulations/landclim_output/rosalia/raw_output_revision/", out_rosa), read_csv) %>%
  set_names(3321:4120) %>% # change once missing files arrive
  bind_rows(.id = "run_id") %>%
  setDT(.)

output_rosa_lanclim_j <- output_rosa_lanclim %>%
  mutate(run_id = as.integer(run_id)) %>%
  separate(scenario, c("scenario", "diversity"), extra = "merge") %>%
  left_join(toc)

head(output_rosa_lanclim_j)

write_csv(output_rosa_lanclim_j, "final_simulations/landclim_output/rosalia/output_rosalia_landclim_revision.csv")

# dischma

out_disch <- list.files("final_simulations/landclim_output/dischma/raw_output_revision/", pattern = "*.csv") %>%
  mixedsort(.)

output_disch_lanclim <- lapply(paste0("final_simulations/landclim_output/dischma/raw_output_revision/", out_disch), read_csv) %>%
  set_names(2521:3320) %>%
  bind_rows(.id = "run_id") %>%
  setDT(.)

output_disch_lanclim_j <- output_disch_lanclim %>%
  separate(scenario, c("scenario", "diversity"), extra = "merge") %>%
  mutate(run_id = as.integer(run_id)) %>%
  left_join(toc)

write_csv(output_disch_lanclim_j, "final_simulations/landclim_output/dischma/output_dischma_landclim_revision.csv")

# AGGREGATE OUTPUT --------------------------------------------------------

# landclim

# create landscape output landclim ----------------------------------------------------------

output_dischma_landclim <- read_csv("final_simulations/landclim_output/dischma/output_dischma_landclim_revision.csv")

output_rosalia_landclim <- read_csv("final_simulations/landclim_output/rosalia/output_rosalia_landclim_revision.csv")

output_landclim <- bind_rows(output_dischma_landclim, output_rosalia_landclim)

landscape_landclim <- output_landclim %>%
  group_by(model, 
           landscape, 
           climate, 
           year, 
           species,
           disturbances,
           age_structure,
           dist_impact,
           experiment,
           replication,
           diversity, 
           scenario) %>%
  summarize(AGB = sum(AGB, na.rm = TRUE)) %>%
  ungroup(.) %>%
  as.data.frame(.)


summary(landscape_landclim)


write_csv(landscape_landclim, "final_simulations/landclim_output/output_aggregated/landscape_landclim_revision.csv")

# something is not working in the case_when function of the "big_trees_ha" column  so I do the calculation by hand

landscape_landclim_ha <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim_revision.csv")

summary(landscape_landclim_ha)

landscape_landclim_ha_rosa <- filter(landscape_landclim_ha, landscape == "rosalia") %>%
  mutate(AGB_t_ha = AGB / 1222000,
         big_trees_ha = big_trees / 1222)

landscape_landclim_ha_disch <- filter(landscape_landclim_ha, landscape == "dischma") %>%
  mutate(AGB_t_ha = AGB / 923000,
         big_trees_ha = big_trees / 923)


landscape_landclim_ha <- bind_rows(landscape_landclim_ha_disch, landscape_landclim_ha_rosa)

summary(landscape_landclim_ha)

write_csv(landscape_landclim_ha, "final_simulations/landclim_output/output_aggregated/landscape_landclim_ha_revision.csv")

test <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim_ha_revision.csv")

summary(test)

# ...now I found out that  that the bug comes from the read_csv() function...


# iland

# create landscape output iland ----------------------------------------------------------

output_dischma_iland <- read_csv("final_simulations/iland_output/dischma/output_dischma_iland_revision.csv")

output_rosalia_iland <- read_csv("final_simulations/iland_output/rosalia/output_rosalia_iland_revision.csv")

output_iland <- bind_rows(output_dischma_iland, output_rosalia_iland)


landscape_output <- output_iland %>%
  filter(year != 0) %>%
  group_by(model, 
           landscape, 
           climate, 
           year, 
           species,
           disturbances,
           replication,
           diversity, 
           scenario,
           dist_impact,
           experiment,
           age_structure) %>%
  summarize(AGB = sum(AGB, na.rm = TRUE),
            big_trees = sum(big_trees, na.rm = TRUE)) %>%
  mutate(AGB_t_ha = case_when(landscape == "rosalia" ~ AGB/1222000,
                              landscape == "dischma" ~ AGB/923000),
         big_trees_ha = case_when(landscape == "rosalia" ~ big_trees/1222,
                                  landscape == "dischma" ~ big_trees/923)) %>%
  ungroup(.)

summary(landscape_output)

write_csv(landscape_output, "final_simulations/iland_output/output_aggregated/landscape_iland_revision.csv")





