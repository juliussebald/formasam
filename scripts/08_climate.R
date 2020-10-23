### This script produces the CO2 concentration file as well as the the climate sampling sequence
### for the final simulations of the FORMASAM study
### Additionally this script derives numbers on climate in different scenarios for the mansucript

library(tidyverse)
library(RSQLite)
library(raster)

set.seed(42)

# CO2 concentration

# load in gunnars files and filter year 1:200

rcp45 <- read.csv("../../materials/old/CO2_data/rcp45.csv",
                    sep = ";",
                    header = TRUE) %>%
  filter(year %in% c(2000:2199)) %>%
  mutate(CO2 = case_when(year > 2099 ~ 537.8714,
                         TRUE ~ CO2)) %>%
  mutate(year = 1:200) %>%
  rename(model.climate.co2concentration = CO2) %>%
  write_csv("../../materials/DISCHMA/scripts/time_event_files/CO2_rcp45.csv") %>%
  write_csv("../../materials/ROSALIA/scripts/time_event_files/CO2_rcp45.csv") %>%
  write_csv("climate/CO2_rcp45.csv")
  



rcp85 <- read.csv("../../materials/old/CO2_data/rcp85.csv",
                  sep = ";",
                  header = TRUE) %>%
  filter(year %in% c(2000:2199)) %>%
  mutate(CO2 = case_when(year > 2099 ~ 926.6653,
                         TRUE ~ CO2)) %>%
  mutate(year = 1:200) %>%
  rename(model.climate.co2concentration = CO2) %>%
  write_csv("../../materials/DISCHMA/scripts/time_event_files/CO2_rcp85.csv") %>%
  write_csv("../../materials/ROSALIA/scripts/time_event_files/CO2_rcp85.csv") %>%
  write_csv("climate/CO2_rcp85.csv")
  



# climate sampling sequence

# historic

sampling_list_historic_iland <- paste(sample(0:29, 200, replace = TRUE), 
                                      collapse = ",") %>%
  write.table("../../methods/r/climate/climate_sampling_list_historic_iland.txt", 
              row.names = FALSE, 
              col.names = FALSE)

sampling_list_historic_LandClim <- paste(sample(1986:2010, 200, replace = TRUE), 
                                         collapse = ",") %>%
  write.table("../../methods/r/climate/climate_sampling_list_historic_LandClim.txt", 
              row.names = FALSE, 
              col.names = FALSE)

# future

sampling_list_future_iland <- paste0(paste(c(0:99), collapse = ","), 
                                     ",", 
                                     paste(sample(70:99, 100, replace = TRUE), collapse = ","))%>%
  write.table("../../methods/r/climate/climate_sampling_list_future_iland.txt", 
              row.names = FALSE, 
              col.names = FALSE)
                                      
sampling_list_future_LandClim <- paste0(paste(c(2000:2099), collapse = ","), 
                                        ",", 
                                        paste(sample(2070:2099, 100, replace = TRUE), collapse = ",")) %>%
  write.table("../../methods/r/climate/climate_sampling_list_future_LandClim.txt", 
              row.names = FALSE, 
              col.names = FALSE)



# climate numbers for the manuscript --------------------------------------

# ROSALIA -----------------------------------------------------------------


db.conn <- dbConnect(SQLite(), dbname = "../../materials/ROSALIA/database/rcp45.sqlite") 
tables_rosa <- dbListTables(db.conn)


climate_rosa <- list()

#clim <- "climate2"

for (clim in tables_rosa) {
  
  table <- dbReadTable(db.conn, clim) %>%
    filter(year %in% 1981:2010) %>%
    mutate(mean_temp = (min_temp + max_temp)/2) %>%
    group_by(year) %>%
    summarize(precip_year = sum(prec),
              temp_year = mean(mean_temp)) 
  
  historic_climate <- data.frame(file = clim, 
                                 mean_prec = mean(table$precip_year),
                                 mean_temp = mean(table$temp_year))
  
  climate_rosa[[clim]] <- historic_climate
  
}

climate_rosa_df <- climate_rosa %>%
  bind_rows(.) 

# get resource units of rosalia landscape

ruid_rosa <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc")

# get dem of rosalia landscape

dem_rosa <- raster("../../materials/ROSALIA/gis/dem_rosalia.asc")

# calculate elevation of every ressource unit and define mangement unit accordingly

coord_ruid_rosa <- rasterToPoints(ruid_rosa) %>%
  as.data.frame(.)

xy <- coord_ruid_rosa %>%
  dplyr::select(-EnvGrid_Rosalia)

env_file <- read.table("../../materials/ROSALIA/gis/EnvFile_Rosalia.txt", header = TRUE) %>%
  dplyr::select(id, model.climate.tableName)

init_rosalia <- coord_ruid_rosa %>%
  rename(id = EnvGrid_Rosalia) %>%
  mutate(elevation = round(raster::extract(dem_rosa, xy), digits = 0)) %>%
  left_join(env_file) %>%
  left_join(climate_rosa_df, by = c("model.climate.tableName" = "file"))

min(init_rosalia$mean_prec)
max(init_rosalia$mean_prec)
min(init_rosalia$mean_temp)
max(init_rosalia$mean_temp)


ggplot(init_rosalia, aes(x = elevation, y = mean_prec)) +
  geom_point()


# DISCHMA -----------------------------------------------------------------


db.conn <- dbConnect(SQLite(), dbname = "../../materials/DISCHMA/database/historic.sqlite") 
tables_disch <- dbListTables(db.conn)


climate_disch <- list()

#clim <- "c001"

for (clim in tables_disch) {
  
  table <- dbReadTable(db.conn, clim) %>%
    filter(year %in% 1981:2010) %>%
    mutate(mean_temp = (min_temp + max_temp)/2) %>%
    group_by(year) %>%
    summarize(precip_year = sum(prec),
              temp_year = mean(mean_temp)) 
  
  historic_climate <- data.frame(file = clim, 
                                 mean_prec = mean(table$precip_year),
                                 mean_temp = mean(table$temp_year))
  
  climate_disch[[clim]] <- historic_climate
  
}

climate_disch_df <- climate_disch %>%
  bind_rows(.) 


# get resource units of discma landscape

ruid_disch <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc")

# get dem of rosalia landscape

dem_disch <- raster("../../materials/DISCHMA/gis/dem_disch_ruid.asc")

# calculate elevation of every ressource unit and define mangement unit accordingly

coord_ruid_disch <- rasterToPoints(ruid_disch) %>%
  as.data.frame(.)

xy <- coord_ruid_disch %>%
  dplyr::select(-EnvGrid_Dischma)

env_file_disch <- read.table("../../materials/DISCHMA/gis/EnvFile_Dischma.txt", header = TRUE) %>%
  dplyr::select(id, model.climate.tableName)

init_dischma <- coord_ruid_disch %>%
  rename(id = EnvGrid_Dischma) %>%
  mutate(elevation = round(raster::extract(dem_disch, xy), digits = 0)) %>%
  left_join(env_file_disch) %>%
  left_join(climate_disch_df, by = c("model.climate.tableName" = "file")) 


min(init_dischma$mean_prec)
max(init_dischma$mean_prec)
min(filter(init_dischma, elevation <2301)$mean_temp)
max(init_dischma$mean_temp)
min(init_dischma$elevation)
max(init_dischma$elevation)


min(init_rosalia$mean_prec)
max(init_rosalia$mean_prec)
min(init_rosalia$mean_temp)
max(init_rosalia$mean_temp)
min(init_rosalia$elevation)
max(init_rosalia$elevation)


ggplot(init_dischma, aes(x = elevation, y = mean_prec)) +
  geom_point()



