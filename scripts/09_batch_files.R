### This R-script creates a batch file for running the final simulations of the 
### FORMASAM study

library(tidyverse)

# create table of all combinations

mastertable <- read.csv("initalization/mastertable.csv")

toc <- mastertable %>%
  dplyr::select(landscape, climate, scenario, diversity, disturbances, replication) %>%
  distinct(.) %>%
  arrange(landscape, climate, scenario, diversity, disturbances) %>%
  mutate(run_id = 1:nrow(.)) %>%
  write_csv(., "batch_file/table_of_combinations.csv")

toc_testrun <- mastertable %>%
  dplyr::select(landscape, climate, scenario, diversity, disturbances) %>%
  distinct(.) %>%
  arrange(landscape, climate, scenario, diversity, disturbances) %>%
  mutate(run_id = 1:nrow(.)) %>%
  write_csv(., "batch_file/testrun_table_of_combinations.csv")


# ROSALIA

toc_rosa <- toc %>%
  filter(landscape == "rosalia") 

setup_rosa_1 <-  "$ILAND ROSALIA/xmls/FORMASAM_ROSALIA_"

setup_rosa_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_rosa <- "system.database.out=FORMASAM_ROSALIA_" 

log_rosa <- " system.logging.logFile=../log/FORMASAM_ROSALIA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"



batch_file_rosa <- toc_rosa %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_rosa_1,
                        climate,
                        setup_rosa_2,
                        output_rosa, run_id, ".sqlite", 
                        log_rosa, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.run_id=", run_id, " "))

batch_file_rosa %>%
  dplyr::select(batch) %>%
  write.table(., "../../methods/r/batch_file/batch_rosa.txt",
            row.names = FALSE,
            quote = FALSE,
            col.names = FALSE)


# DISCHMA


toc_disch <- toc %>%
  filter(landscape == "dischma") 

setup_disch_1 <-  "$ILAND DISCHMA/xmls/FORMASAM_DISCHMA_"

setup_disch_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "

output_disch <- "system.database.out=FORMASAM_DISCHMA_" 

log_disch <- " system.logging.logFile=../log/FORMASAM_DISCHMA_" 

sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"

init <- "model.initialization.file=../init/init_files/"



batch_file_disch <- toc_disch %>%
  rowwise(.) %>%
  mutate(batch = paste0(setup_disch_1,
                        climate,
                        setup_disch_2,
                        output_disch, run_id, ".sqlite", 
                        log_disch, run_id, ".txt",
                        sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
                        init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
                        "user.replication=", replication, " ",
                        "user.dist_scenario=", disturbances, " ",
                        "user.scenario=", scenario, " ",
                        "user.diversity=", diversity, " ",
                        "user.run_id=", run_id, " "))

batch_file_disch %>%
  dplyr::select(batch) %>%
  write.table(., "../../methods/r/batch_file/batch_disch.txt",
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE)


# # batch file for simulations that did not finish --------------------------
# 
# toc <- read_csv("batch_file/table_of_combinations.csv")
# 
# error_dischma <- read.table("../../materials/DISCHMA/log/errorfiles.txt", header = FALSE) %>%
#   separate(V1, c("one", "two", "run"), "_" ) %>%
#   separate(run, c("run_id", "txt")) %>%
#   mutate(run_id = as.integer(run_id))
# 
# error_rosalia <- read.table("../../materials/ROSALIA/log/errorfiles.txt", header = FALSE) %>%
#   separate(V1, c("one", "two", "run"), "_" ) %>%
#   separate(run, c("run_id", "txt")) %>%
#   mutate(run_id = as.integer(run_id))
# 
# 
# toc_error_disch <- filter(toc, run_id %in% error_dischma$run_id)
# 
# toc_error_rosa <- filter(toc, run_id %in% error_rosalia$run_id)
# 
# 
# # ROSALIA
# 
# 
# setup_rosa_1 <-  "$ILAND ROSALIA/xmls/FORMASAM_ROSALIA_"
# 
# setup_rosa_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "
# 
# output_rosa <- "system.database.out=FORMASAM_ROSALIA_" 
# 
# log_rosa <- " system.logging.logFile=../log/FORMASAM_ROSALIA_" 
# 
# sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"
# 
# init <- "model.initialization.file=../init/init_files/"
# 
# 
# 
# batch_file_rosa_error <- toc_error_rosa %>%
#   rowwise(.) %>%
#   mutate(batch = paste0(setup_rosa_1,
#                         climate,
#                         setup_rosa_2,
#                         output_rosa, run_id, ".sqlite", 
#                         log_rosa, run_id, ".txt",
#                         sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
#                         init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
#                         "user.replication=", replication, " ",
#                         "user.dist_scenario=", disturbances, " ",
#                         "user.scenario=", scenario, " ",
#                         "user.diversity=", diversity, " ",
#                         "user.run_id=", run_id, " "))
# 
# batch_file_rosa_error %>%
#   dplyr::select(batch) %>%
#   write.table(., "../../methods/r/batch_file/batch_rosa_error.txt",
#               row.names = FALSE,
#               quote = FALSE,
#               col.names = FALSE)
# 
# 
# # DISCHMA
# 
# 
# setup_disch_1 <-  "$ILAND DISCHMA/xmls/FORMASAM_DISCHMA_"
# 
# setup_disch_2 <- ".xml $NYEARS system.database.in=$SPECIESPARAMS "
# 
# output_disch <- "system.database.out=FORMASAM_DISCHMA_" 
# 
# log_disch <- " system.logging.logFile=../log/FORMASAM_DISCHMA_" 
# 
# sapling <- " model.initialization.saplingFile=../init/sapling_lists/final_simulations/"
# 
# init <- "model.initialization.file=../init/init_files/"
# 
# 
# 
# batch_file_disch_error <- toc_error_disch %>%
#   rowwise(.) %>%
#   mutate(batch = paste0(setup_disch_1,
#                         climate,
#                         setup_disch_2,
#                         output_disch, run_id, ".sqlite", 
#                         log_disch, run_id, ".txt",
#                         sapling, paste(landscape, scenario, diversity, sep = "_"), ".csv ", 
#                         init, paste(landscape, scenario, diversity, sep = "_"), ".csv ",
#                         "user.replication=", replication, " ",
#                         "user.dist_scenario=", disturbances, " ",
#                         "user.scenario=", scenario, " ",
#                         "user.diversity=", diversity, " ",
#                         "user.run_id=", run_id, " "))
# 
# batch_file_disch_error %>%
#   dplyr::select(batch) %>%
#   write.table(., "../../methods/r/batch_file/batch_disch_error.txt",
#               row.names = FALSE,
#               quote = FALSE,
#               col.names = FALSE)
# 
