### This script analyzes the results of the final FORMASAM simulations


library(tidyverse) # version 1.3.0
library(RSQLite) # version 2.2.0
library(data.table) # version 1.13.0
library(patchwork) # version 1.0.1
library(ggthemes) # version 4.2.0
library(gtools) # version 3.8.2
library(vegetarian) # version 1.2




# ILAND -------------------------------------------------------------------

# load toc

toc <- read.csv("batch_file/table_of_combinations.csv")


# Bring iLand output in FORMASAM format -----------------------------------

# rosalia

toc_rosalia <- filter(toc, landscape == "rosalia") 

output_rosa <- list()

#runid <- 1261

for (runid in toc_rosalia$run_id) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/ROSALIA/output/final_simulations/FORMASAM_ROSALIA_", runid , ".sqlite", 
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
                replication,
                AGB,
                big_trees)



write_csv(output_rosalia_iland, "final_simulations/iland_output/rosalia/output_rosalia_iland.csv")

rm(runid)



# dischma

toc_dischma <- filter(toc, landscape == "dischma") 

output_disch <- list()

#runid <- 1

for (runid in toc_dischma$run_id) {
  
  
  db.conn <- dbConnect(SQLite(), dbname = paste0("../../materials/DISCHMA/output/final_simulations/FORMASAM_DISCHMA_", runid , ".sqlite", 
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
                replication,
                AGB,
                big_trees)


rstudioapi::documentSave()

write_csv(output_dischma_iland, "final_simulations/iland_output/dischma/output_dischma_iland.csv")


# LANDCLIM ----------------------------------------------------------------

toc <- read_csv("batch_file/table_of_combinations.csv")

# create one big output table per landscape

# rosalia

out_rosa <- list.files("final_simulations/landclim_output/rosalia/raw_output/", pattern = "*.csv") %>%
  mixedsort(.)

output_rosa_lanclim <- lapply(paste0("final_simulations/landclim_output/rosalia/raw_output/", out_rosa), read_csv) %>%
  set_names(1261:2520) %>%
  bind_rows(.id = "run_id") %>%
  setDT(.)

output_rosa_lanclim_j <- output_rosa_lanclim %>%
  mutate(run_id = as.integer(run_id)) %>%
  left_join(toc)

write_csv(output_rosa_lanclim_j, "final_simulations/landclim_output/rosalia/output_rosalia_landclim.csv")

# dischma

out_disch <- list.files("final_simulations/landclim_output/dischma/raw_output/", pattern = "*.csv") %>%
  mixedsort(.)

output_disch_lanclim <- lapply(paste0("final_simulations/landclim_output/dischma/raw_output/", out_disch), read_csv) %>%
  set_names(1:1260) %>%
  bind_rows(.id = "run_id") %>%
  setDT(.)

output_disch_lanclim_j <- output_disch_lanclim %>%
  mutate(run_id = as.integer(run_id)) %>%
  left_join(toc)

write_csv(output_disch_lanclim_j, "final_simulations/landclim_output/dischma/output_dischma_landclim.csv")

# AGGREGATE OUTPUT --------------------------------------------------------

# landclim

# create landscape output landclim ----------------------------------------------------------

output_dischma_landclim <- read_csv("final_simulations/landclim_output/dischma/output_dischma_landclim.csv")

output_rosalia_landclim <- read_csv("final_simulations/landclim_output/rosalia/output_rosalia_landclim.csv")

output_landclim <- bind_rows(output_dischma_landclim, output_rosalia_landclim)

landscape_landclim <- output_landclim %>%
  group_by(model, 
           landscape, 
           climate, 
           year, 
           species,
           disturbances,
           replication,
           diversity, 
           scenario) %>%
  summarize(AGB = sum(AGB, na.rm = TRUE),
            big_trees =  sum(Big_trees, na.rm = TRUE)) %>%
    ungroup(.)


summary(landscape_landclim)


write_csv(landscape_landclim, "final_simulations/landclim_output/output_aggregated/landscape_landclim.csv")

# something is not working in the case_when function of the "big_trees_ha" column  so I do the calculation by hand

landscape_landclim_ha <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim.csv")

summary(landscape_landclim_ha)

landscape_landclim_ha_rosa <- filter(landscape_landclim_ha, landscape == "rosalia") %>%
  mutate(AGB_t_ha = AGB / 1222000,
         big_trees_ha = big_trees / 1222)

landscape_landclim_ha_disch <- filter(landscape_landclim_ha, landscape == "dischma") %>%
  mutate(AGB_t_ha = AGB / 923000,
         big_trees_ha = big_trees / 923)


landscape_landclim_ha <- bind_rows(landscape_landclim_ha_disch, landscape_landclim_ha_rosa)

summary(landscape_landclim_ha)

write_csv(landscape_landclim_ha, "final_simulations/landclim_output/output_aggregated/landscape_landclim_ha.csv")

test <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim_ha.csv")

summary(test)

# ...now I found out that  that the bug comes from the read_csv() function...


# iland

# create landscape output iland ----------------------------------------------------------

output_dischma_iland <- read_csv("final_simulations/iland_output/dischma/output_dischma_iland.csv")

output_rosalia_iland <- read_csv("final_simulations/iland_output/rosalia/output_rosalia_iland.csv")

output_iland <- bind_rows(output_dischma_iland, output_rosalia_iland)

landscape_output <- output_iland %>%
  group_by(model, 
           landscape, 
           climate, 
           year, 
           species,
           disturbances,
           replication,
           diversity, 
           scenario) %>%
  summarize(AGB = sum(AGB),
            big_trees =  sum(big_trees)) %>%
  mutate(AGB_t_ha = case_when(landscape == "rosalia" ~ AGB/1222000,
                              landscape == "dischma" ~ AGB/923000),
         big_trees_ha = case_when(landscape == "rosalia" ~ big_trees/1222,
                                  landscape == "dischma" ~ big_trees/923)) %>%
  ungroup(.)


#write_csv(landscape_output, "final_simulations/iland_output/output_aggregated/landscape_iland.csv")


# create landscapeoutput with biomass relative to no disturbances--------------------------------------------------

toc <- read.csv("batch_file/table_of_combinations.csv", stringsAsFactors = FALSE)

landscape_iland <- read.csv("final_simulations/iland_output/output_aggregated/landscape_iland.csv",
                            stringsAsFactors = FALSE)

landscape_landclim <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim.csv",
                               stringsAsFactors = FALSE)

summary(landscape_iland)
summary(landscape_landclim)

# aggregate over all species

landscape_output <- bind_rows(landscape_iland, landscape_landclim) %>%
  filter(year!= 0) %>%
  group_by(model, 
           landscape,
           climate,
           year,
           disturbances,
           replication,
           diversity,
           scenario) %>%
  summarize(AGB_t_ha = sum(AGB_t_ha),
            big_trees_ha = sum(big_trees_ha)) %>%
  left_join(toc) %>%
  ungroup(.)


baseline <- filter(landscape_output, disturbances == "no") %>% 
  rename(AGB_base = AGB_t_ha,
         big_trees_base = big_trees_ha) %>%
  ungroup(.) %>%
  dplyr::select(-disturbances, -run_id)


scenario <- filter(landscape_output, disturbances != "no") %>%
  left_join(baseline, by = c("model",
                             "landscape",
                             "climate",
                             "year",
                             "replication",
                             "diversity",
                             "scenario"))


landscape_diff <- scenario %>%
  mutate(AGB_diff_rel = (AGB_t_ha / AGB_base) * 100 ,
         big_trees_diff_rel = (big_trees_ha / big_trees_base) * 100) %>%
  dplyr::select(run_id, model:scenario, AGB_diff_rel, big_trees_diff_rel) %>%
  ungroup(.)

ggplot(landscape_diff, aes(x = disturbances, y = AGB_diff_rel)) +
  geom_boxplot() +
  labs(y = "biomass relative to no disturbance [%]") +
  facet_wrap(~ model)


write_csv(landscape_diff, "final_simulations/landscape_rev_rel_no_spec.csv")

# create landscape output with difference in biomass to "no disturbances" run --------------------------------------------------
  
toc <- read.csv("batch_file/table_of_combinations.csv", stringsAsFactors = FALSE)

landscape_iland <- read.csv("final_simulations/iland_output/output_aggregated/landscape_iland.csv",
                              stringsAsFactors = FALSE)

landscape_landclim <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim.csv",
                               stringsAsFactors = FALSE)

summary(landscape_iland)
summary(landscape_landclim)

# aggregate over all species

landscape_output <- bind_rows(landscape_iland, landscape_landclim) %>%
  filter(year!= 0) %>%
  group_by(model, 
           landscape,
           climate,
           year,
           disturbances,
           replication,
           diversity,
           scenario) %>%
  summarize(AGB_t_ha = sum(AGB_t_ha),
            big_trees_ha = sum(big_trees_ha)) %>%
  left_join(toc) %>%
  ungroup(.)


ggplot(landscape_output, aes(x = disturbances, y = big_trees_ha)) +
  geom_boxplot() +
  facet_wrap(~model)

#write_csv(landscape_output, "final_simulations/landscape_rev_no_spec.csv")

summary(landscape_output)


baseline <- filter(landscape_output, disturbances == "no") %>% 
  rename(AGB_base = AGB_t_ha,
         big_trees_base = big_trees_ha) %>%
  ungroup(.) %>%
  dplyr::select(-disturbances, -run_id)


scenario <- filter(landscape_output, disturbances != "no") %>%
  left_join(baseline, by = c("model",
                             "landscape",
                             "climate",
                             "year",
                             "replication",
                             "diversity",
                             "scenario"))

# plot for concept figure


scenario %>%
  filter(run_id == 1 & model == "ILAND") %>%
  dplyr::select(year, AGB_t_ha, AGB_base) %>%
  rename(disturbed = AGB_t_ha, undisturbed = AGB_base) %>%
  gather(disturbance, agb, - year) %>%
  mutate(disturbance = factor(disturbance, levels = c("undisturbed", "disturbed"), labels = c("undisturbed", "disturbed"))) %>%
  ggplot() +
  geom_ribbon(data = data2, aes(x = year, ymin = disturbed, ymax = undisturbed, fill = "disturbance \n impact"), alpha = 0.7) +
  geom_line(aes(x = year, y = agb, col = disturbance), size = 1) +
    theme_few() +
  scale_fill_manual(values = c("grey")) +
  scale_color_manual(values = c("black", "darkred")) +
  labs(y = "landscape biomass \n [t/ha]", col = "", fill = "") +
  ylim(0,250) +
  theme(legend.position = c(0.75, 0.35),
        legend.background = element_blank()) +
  ggsave("../../results/figures/main_text/old/impact.png", width = 3, height = 3)


data2 <- scenario %>%
  filter(run_id == 1 & model == "ILAND") %>%
  dplyr::select(year, AGB_t_ha, AGB_base) %>%
  rename(disturbed = AGB_t_ha, undisturbed = AGB_base) 

landscape_diff <- scenario %>%
  mutate(AGB_diff = AGB_base - AGB_t_ha,
         big_trees_diff = big_trees_base - big_trees_ha) %>%
  dplyr::select(run_id, model:scenario, AGB_diff, big_trees_diff) %>%
  ungroup(.)


ggplot(landscape_diff, aes(x = disturbances, y = AGB_diff)) +
  geom_boxplot() +
  facet_wrap(~ model)


ggplot(landscape_diff, aes(x = disturbances, y = big_trees_diff)) +
  geom_boxplot() +
  facet_wrap(~ model)


write_csv(landscape_diff, "final_simulations/landscape_rev_diff_no_spec.csv")



# create aggregated landscape data to analyse species shares --------------------------------------------------


landscape_iland <- read.csv("final_simulations/iland_output/output_aggregated/landscape_iland.csv",
                            stringsAsFactors = FALSE)

landscape_landclim <- read.csv("final_simulations/landclim_output/output_aggregated/landscape_landclim.csv",
                               stringsAsFactors = FALSE)

landscape_output <- bind_rows(landscape_iland, landscape_landclim) 

summary(landscape_output)

landscape_species <- landscape_output %>%
  group_by(model, 
           landscape, 
           climate, 
           year,
           species,
           disturbances, 
           diversity, 
           scenario) %>%
  summarize(AGB_t_ha_med = median(AGB_t_ha)) %>%
  ungroup(.)

summary(landscape_species)

write_csv(landscape_species, "final_simulations/landscape_rev_species_agg.csv")



# create dataset with realized diversity with new gamma according to Jost (2006) ----------------------------------


# the folder gamma new contains just the ressource unit output for both landscapes
# and both models in one folder (to make looping through the seperate files easier...)

toc <- read_csv("batch_file/table_of_combinations.csv") %>%
  replicate(n = 2, simplify = FALSE) %>%
  set_names(c("ILAND", "LANDCLIM")) %>%
  bind_rows(.id = "model")

output <- list.files("final_simulations/gamma_new/")

veg_data_all <- data.frame()

file <-output[1]

for (file in output) {
  
  data <- read_csv(paste0("final_simulations/gamma_new/", file)) 
  
  if (data[1,]$model == "LANDCLIM") {
    
    data <- data %>%
      dplyr::select(-x,-y)
  }
  
  data <- filter(data, year == 200)  
  
  run_ids <- unique(data$run_id)
  
  
  for (id in run_ids) { 
    
    veg_data <- data %>%
      filter(run_id == id) %>%
      spread(species, AGB) %>%
      mutate_all(function (x) ifelse(is.na(x), 0, x)) %>%
      group_by(run_id, 
               model) %>%
      summarize(enos = d(as.matrix(.[, 12:ncol(.)]), lev = "gamma", q = 1))
    
    veg_data_all <- bind_rows(veg_data, veg_data_all)
    
    print(id)
    
  }
}

realized_diversity <- veg_data_all %>%
  left_join(toc)

write_csv("final_simulations/realized_diversity_new.csv")
  
# create dataset to study variation

variation_iland <- read_csv("final_simulations/iland_output/output_aggregated/variation_iland.csv")

variation_landclim <- read_csv("final_simulations/landclim_output/output_aggregated/variation_landclim.csv")


variation <- bind_rows(variation_iland, variation_landclim)


variation_spatial <- variation %>%
  group_by(model,
           landscape,
           replication,
           climate,
           disturbances,
           diversity,
           scenario) %>%
  summarize(cv_spatial_agb = sd(AGB) / mean(AGB) * 100,
            cv_spatial_bt = sd(big_trees) / mean(big_trees) * 100)

write_csv(variation_spatial, "final_simulations/variation_spatial.csv")

variation_temporal <- read.csv("final_simulations/landscape_rev_no_spec.csv") %>%
  group_by(model, 
           landscape,
           climate,
           disturbances,
           replication,
           diversity,
           scenario) %>%
  summarize(cv_temporal_agb = sd(AGB_t_ha) / mean(AGB_t_ha) * 100,
            cv_temporal_bt = sd(big_trees_ha) / mean(big_trees_ha) * 100) 

write_csv(variation_temporal, "final_simulations/variation_temporal.csv")







