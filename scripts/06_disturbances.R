
library(raster) # version 3.3-13
library(tidyverse) # version 1.3
library(ggthemes) # version 4.2.0



# This script creates disturbance probability functions according to Schmidt et al. 2010
# further down the script creates the disturbance sequences for the simulation runa




# disturbance probability funcitons ---------------------------------------


height <- c(1:50)

dbh <- seq(50, 50, length.out = 50)

# # try effect of different topex to distance constants
# 
# top1 <- -0.00622
# 
# top2 <- -0.00149
# 
# top3 <- 0.00440
# 
# top4 <- -0.00280
# 
# 
# gradient <- -30
# 
# # hill
# 
# constant <- top1 * (20 * gradient) + top2 * (20 * gradient) + top3 * (20 * gradient) + top4 * (20 * gradient)
# 
# 
# # slope
# 
# constant <- top1 * (20 * gradient) + top2 * (20 * -gradient)

constant <- 3 # this constant value describes the topographic exposure of a given stand to 
# wind disturbances (Topex to distance Index).
# We decided for a value of 3 which correspondeds to a west exposed slope with 30 % 
# gradient.
# piab

logit_piab  <- -12.27 + log((dbh^(-1.775)) / ((height)^(-5.128))) + constant
# logistic link function
p_piab <- exp(logit_piab) / (exp(logit_piab) + 1)

# other broad leaved

logit_obl  <- -8.78 + log((dbh^(-0.287)) / ((height)^(-1.770))) + constant
# logistic link function
p_obl <- exp(logit_obl) / (exp(logit_obl) + 1)

# fasy_qupe

logit_fasy_qupe  <- -13.04 + log((dbh^(-0.998)) / ((height)^(-3.994))) + constant
# logistic link function
p_fasy_qupe <- exp(logit_fasy_qupe) / (exp(logit_fasy_qupe) + 1)

# pisy_lade

logit_pisy_lade  <- -8.59 + log((dbh^(-1.625)) / ((height)^(-3.525))) + constant
# logistic link function
p_pisy_lade <- exp(logit_pisy_lade) / (exp(logit_pisy_lade) + 1)

# abal_psme

logit_abal_psme  <- -8.46 + log((dbh^(-0.505)) / ((height)^(-2.449))) + constant
# logistic link function
p_abal_psme <- exp(logit_abal_psme) / (exp(logit_abal_psme) + 1)

# data frame

species_labels <- factor(c("piab", "other_broadleaved", "fasy_quepe", "pisy_lade", "abal", 
                           labels = c("Piab", "Other broadleaved", "Fasy and Qupe", "Pisy and Lade", "Abal")))

p_dist <- data.frame(dbh = dbh,
                     height = height,
                     piab = p_piab,
                     other_broadleaved = p_obl,
                     fasy_qupe = p_fasy_qupe,
                     pisy_lade = p_pisy_lade,
                     abal = p_abal_psme) %>%
  gather(species, prob, piab:abal) %>%
  mutate(species = factor(species, 
                          levels = c("piab", "other_broadleaved", "fasy_qupe", "pisy_lade", "abal"), 
                          labels = c("piab", "other broadleaved", "fasy and qupe", "pisy and lade", "abal"))) %>%
  ggplot(., aes(x = height, y = prob, col = species))+
  geom_line() +
  ylim(0, 1) +
  labs(y = "probability of disturbance", col = "species group", x = "height [m]") +
  theme_few()

ggsave("../../results/figures/supplement/p_dist.png", p_dist, width = 5, height = 3.5)


# disturbance files for iLand and LandClim --------------------------------

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

rot_historic <- 400

rot_future <- 200

modifier <- 2

patch_size_rosa <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017, ISPRS

landscape_size_rosa <- length(unique(ruid_rosa))

# create figure for supplement

modifier <- 2

patch_size_rosa <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017, ISPRS

patches_rosa <- data.frame(patchsize = rexp(5000, 1/patch_size_rosa)) %>%
  round(., 0) %>%
  filter(patchsize > 0)

head(patches_rosa)

ggplot(patches_rosa, aes(x = factor(patchsize))) +
  geom_histogram(stat = "count") +
  labs(y = "frequency", x = "patchsize [ha]") +
  theme_few() +
  ggsave("../../results/figures/supplement/patch_size_distribution.png", width = 5, height = 5)


# calculate number of disturbance patches per simulation period

sim_period <- 200

landscape_disturbed_rosa_historic <- as.integer(sim_period / rot_historic * landscape_size_rosa) 

landscape_disturbed_rosa_future <- as.integer(sim_period / rot_future * landscape_size_rosa) 

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

for (rep in 1:20) {
  
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


disturbances_rosa_historic$dist_scenario <- "historic"

rm(i, rep)

# future disturbance sequence


for (rep in 1:20) {
  
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

disturbances_rosa_future$dist_scenario <- "future"

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

write_csv(disturbances_rosalia, "disturbances/disturbances_rosalia.csv")

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

rot_historic <- 400

rot_future <- 200

modifier <- 2

patch_size_disch <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017,ISPRS

landscape_size_disch <- length(unique(ruid_disch))

# calculate number of disturbance patches per simulation period

sim_period <- 200

landscape_disturbed_disch_historic <- as.integer(sim_period / rot_historic * landscape_size_disch) 

landscape_disturbed_disch_future <- as.integer(sim_period / rot_future * landscape_size_disch) 

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

for (rep in 1:20) {
  
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


disturbances_disch_historic$dist_scenario <- "historic"

rm(i, rep)

# future disturbance sequence


for (rep in 1:20) {
  
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

disturbances_disch_future$dist_scenario <- "future"

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

write_csv(disturbances_dischma, "disturbances/disturbances_dischma.csv")


# write disturbance files for iLand


disturbance_files <- bind_rows(disturbances_rosalia, disturbances_dischma) %>%
  split(list(.$landscape, .$replication, .$dist_scenario), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., ruid,                              
                                           ruindex,
                                           year)), 
       ~ write_csv(.y, paste0("../../materials/",
                              unique(.x$landscape),
                              "/scripts/disturbance_files/disturbances_", 
                              unique(.x$landscape), "_",
                              unique(.x$dist_scenario), "_",
                              unique(.x$replication),
                              ".csv")))

# write disturbance files for LandClim  

disturbance_files <- bind_rows(disturbances_rosalia, disturbances_dischma) %>%
  split(list(.$landscape, .$replication, .$dist_scenario), drop = TRUE) %>%
  map2(.x = ., .y = map(., ~ dplyr::select(., ruid, 
                                           year)), 
       ~ write_csv(.y, paste0("disturbances/disturbance_files/disturbances_",           
                              unique(.x$landscape), "_",
                              unique(.x$dist_scenario), "_",
                              unique(.x$replication),
                              ".csv"))) 
  
  
  
# writeRaster(disturbance_maps_disch_historic, 
#             "disturbances/disturbances_maps/dischma/historic/disturbance_maps_dischma_historic", 
#             bylayer = TRUE,
#             format = "GTiff")
# 
# writeRaster(disturbance_maps_disch_future, 
#             "disturbances/disturbances_maps/dischma/future/disturbance_maps_dischma_future",
#             bylayer = TRUE,
#             format = "GTiff")


# write disturbance files for iLand



