# This script creates all files needed for the data creating run for the FORMASAM propject.
# The idea is to derive tree dimensions for every age and every ressource unit in the two landscapes from iLand simulation data
# Further down this script creates the initalization files for iLand and LandClim

library(tidyverse) # version 1.3.0

# init files for single species run with iland (structural data) ----------

# create sapling lists


species_pools <- read.table("tree_data/tree_data.txt", header = TRUE)

# Rosalia


species_rosalia <- as.character(unique(filter(species_pools, landscape == "rosalia")$species))

ruid <- read.table("../../materials/ROSALIA/gis/EnvFile_Rosalia.txt", header = TRUE)[,1]

sapling_list_rosalia <- data.frame(stand_id = ruid) %>%
  mutate(count = 130000,
         height_from = 0.2,
         height_to = 0.5,
         age = 5)




for (i in species_rosalia) {
  
  sapling_list <- sapling_list_rosalia %>%
    mutate(species = i) %>%
    dplyr::select(stand_id, species, count, age, height_from, height_to)
  
  write.table(sapling_list, 
              paste0("../../materials/ROSALIA/init/sapling_lists/sapling_list_", i, ".txt", collapse = ""),
              row.names = FALSE,
              quote = FALSE
              )
    
  
}


# Dischma


species_dischma <- as.character(unique(filter(species_pools, landscape == "dischma")$species))

ruid <- read.table("../../materials/DISCHMA/gis/EnvFile_Dischma.txt", header = TRUE)[,1]

sapling_list_dischma <- data.frame(stand_id = ruid) %>%
  mutate(count = 130000,
         height_from = 0.2,
         height_to = 0.5,
         age = 5)




for (i in species_dischma) {
  
  sapling_list <- sapling_list_dischma %>%
    mutate(species = i) %>%
    dplyr::select(stand_id, species, count, age, height_from, height_to)
  
  write.table(sapling_list, 
              paste0("../../materials/DISCHMA/init/sapling_lists/sapling_list_", i, ".txt", collapse = ""), 
              row.names = FALSE,
              quote = FALSE
  )
  
  
}



# initalization files for iLand and LandClim ------------------------------


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

initialization <- read.csv("initalization/mastertable.csv", 
                           stringsAsFactors = FALSE) %>%
  dplyr::select(-climate, -disturbances, -replication) %>%
  distinct(.) %>%
  filter(!diversity == "high_future") %>%
  gather(species, share, abal:tico) %>%
  filter(share > 0) %>%
  rename(age = init.age) %>%
  left_join(all.inits.df, by = c("landscape", "species", "ruid", "age")) %>%
  mutate(stem_number_reduced = round((stemnumber * share),0),
         hd = hd * 100) %>%
  filter((stem_number_reduced != 0 | is.na(stem_number_reduced))) %>%
  arrange(., landscape, ruid, scenario)

# take care of the fact that the high future scenario has the same initial state 
# as the high scenario

initialization_high_future <- initialization %>%
  filter(diversity == "high") %>%
  mutate(diversity = "high_future")

initialization <- bind_rows(initialization, initialization_high_future)


# fill missing old trees


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
  dplyr::select(landscape:share, -age) %>%
  left_join(all.inits.df_max, by = c("landscape", "ruid", "species"))


# full initalization table

initialization_full <- initialization %>%
  filter(! (is.na(stemnumber) & age > 50)) %>%
  bind_rows(missing_old_trees) %>%
  arrange(., landscape, ruid, scenario)

#write_csv(initialization_full, "initalization/initialization_full.csv")

# check how age class distribution looks without old missing trees

plotdata <- initialization_full %>%
  dplyr::select(landscape:age, -m.unit) %>%
  distinct(.) 

filter(plotdata, is.na(age)) %>%  
  head(.)

ggplot(plotdata, aes(x = age)) +
  geom_bar() +
  facet_wrap(~ landscape)

# write initalization file for tim

initalization_files <- initialization_full %>%
  mutate_at(.vars = vars(hd, height_class), function(x) round(x, 1)) %>%
  split(list(.$landscape, .$scenario, .$diversity), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(.,ruid, 
                                           age, 
                                           species, 
                                           count = stem_number_reduced, 
                                           dbh_from = dbhfrom, 
                                           dbh_to = dbhto, 
                                           hd, 
                                           height = height_class)), 
       ~ write_csv(.y, paste0("initalization/initialization_files/", 
                              unique(.x$landscape), "_", 
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))


# create init files for iLand

init_files_iland <- initialization_full %>%
  na.omit(.) %>%  #### remove
  mutate_at(.vars = vars(hd, height_class), function(x) round(x, 1)) %>%
  split(list(.$landscape, .$scenario, .$diversity), drop = TRUE) %>%
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
                              unique(.x$landscape), 
                              "/init/init_files/", 
                              unique(.x$landscape), "_", 
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
  split(list(.$landscape, .$scenario, .$diversity), drop = TRUE) %>%
  map(., ~ arrange(., ruid)) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., stand_id = ruid,
                                           species,  
                                           count = stem_number_reduced, 
                                           age, 
                                           height_from, 
                                           height_to)), 
       ~ write_csv(.y, paste0("../../materials/", 
                              unique(.x$landscape), 
                              "/init/sapling_lists/final_simulations/", 
                              unique(.x$landscape), "_", 
                              unique(.x$scenario), "_", 
                              unique(.x$diversity),
                              ".csv")))
  
