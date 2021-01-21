
library(raster) # version 3.3-13
library(sf) # version 0.9-5
library(tidyverse) # version 1.3.0
library(purrr) # version 0.3.4

set.seed(42)

# create initialization data ----------------------------------------------

# This script creates the management sequences based on the master table
# for iLand and for LandClim
# Input needed:
# 1. mastertable.csv 
# 2. ruindex_dischma.asc
# 3. ruindex_rosalia.asc
# 4. EnvGrid_Rosalia.asc
# 5. EnvGrid_Dischma.asc


# management sequences --------------------------------------------------------------

# the management sequences define were and when thinnings, clear cuts and plantings happen

# define rotation periods

rotation_rosa <- 100
rotation_disch <- 150

# get inital states

mastertable <- read_csv("initalization/mastertable.csv")

manage <- mastertable %>%
  dplyr::select(landscape, ruid, scenario, diversity, init.age) %>%
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
  gather(., key = activity, value = sim_year, -(landscape:init.age)) %>%
  filter(sim_year %in% 0:200) %>%
  arrange(., landscape, ruid, scenario, diversity, init.age) 

# tidy names in managment actions

management$activity <- gsub(".I", "", management$activity)
management$activity <- gsub("I", "", management$activity)


# write management files for LandClim

management_files <- management %>%
  left_join(dplyr::select(mastertable, 
                          landscape, 
                          ruid, 
                          scenario, 
                          diversity, 
                          init.age, 
                          abal:tico) %>%
              distinct(.), 
            by = c("landscape", "ruid", "scenario", "diversity", "init.age")) %>%
  mutate(activity = case_when(activity %in% c("DF1", "DF2") ~ "thinning",
                              TRUE ~ activity)) %>%
  split(list(.$landscape, .$scenario, .$diversity), drop = TRUE) %>%
  map(., ~ write_csv(., paste0("management/management_", 
                              unique(.$landscape), "_", 
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
  left_join(dplyr::select(mastertable, 
                          landscape, 
                          ruid, 
                          scenario, 
                          diversity, 
                          init.age, 
                          abal:tico) %>%
              distinct(.), 
            by = c("landscape", "ruid", "scenario", "diversity", "init.age")) %>%
  split(list(.$landscape, .$scenario, .$diversity), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(.,ruid, 
                                           ruindex, 
                                           activity, 
                                           year = sim_year,
                                           abal:tico)), 
       ~ write_csv(.y, paste0("../../materials/", 
                              unique(.x$landscape), 
                              "/scripts/management_files/management_", 
                              unique(.x$landscape), "_", 
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))
