# This script loads in the raw simulation data of iLand and
# converts it to the FORMASAM format
# Further it combines the pnv data from iLand with pnv data from LandClim
# and combines everything in one nice dataframe
# Finally it plots the results of the pnv runs nicely


# load packages

library(raster) # version 3.3-13
library(sf) # version 0.9-5
library(tidyverse) # version 1.3.0
library(RSQLite) # version 2.2.0
library(data.table) # version 1.13.0
library(patchwork) # version 1.0.1



# ROSALIA -----------------------------------------------------------------


# bring iLand output in FORMASAM format -----------------------------------


#load coordinates of ressource units

coord <- raster("../../materials/ROSALIA/gis/EnvGrid_Rosalia.asc") %>%
  rasterToPoints(.) %>%
  as.data.frame(.) %>%
  rename(ruid = layer)

# load simulated data

# historic

db.conn <- dbConnect(SQLite(), dbname="../../materials/ROSALIA/output/PNV_ROSALIA_historic.sqlite") 
dbListTables(db.conn)
stand_iland_historic <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)

# RCP 45

db.conn <- dbConnect(SQLite(), dbname="../../materials/ROSALIA/output/PNV_ROSALIA_RCP45.sqlite") 
dbListTables(db.conn)
stand_iland_rcp45 <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)

# RCP 85

db.conn <- dbConnect(SQLite(), dbname="../../materials/ROSALIA/output/PNV_ROSALIA_RCP85.sqlite") 
dbListTables(db.conn)
stand_iland_rcp85 <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)


# bring data in FORMASAM format


# historic

PNV_ILAND_ROSALIA_historic <- stand_iland_historic %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))

# RCP45

PNV_ILAND_ROSALIA_rcp45 <- stand_iland_rcp45 %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))

# RCP85

PNV_ILAND_ROSALIA_rcp85 <- stand_iland_rcp85 %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))

write_csv(PNV_ILAND_ROSALIA_historic, "../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_historic.csv")
write_csv(PNV_ILAND_ROSALIA_rcp45, "../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_RCP45.csv")
write_csv(PNV_ILAND_ROSALIA_rcp85, "../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_RCP85.csv")


rm(list = ls())

# load pnv output  --------------------------------------------------------

# iLand

PNV_ILAND_ROSALIA_historic <- read.csv("../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_historic.csv",
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "historic") 

PNV_ILAND_ROSALIA_rcp45 <- read.csv("../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_RCP45.csv",
                                    stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP45") 

PNV_ILAND_ROSALIA_rcp85 <- read.csv("../../materials/ROSALIA/output/PNV_results/iLand/PNV_ILAND_ROSALIA_RCP85.csv", 
                                    stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP85") 

PNV_ILAND_ROSALIA <- bind_rows(PNV_ILAND_ROSALIA_historic, PNV_ILAND_ROSALIA_rcp45, PNV_ILAND_ROSALIA_rcp85) %>%
  filter(species != "rops")

# LandClim

PNV_LANDCLIM_ROSALIA_historic <- read.csv("../../materials/ROSALIA/output/PNV_results/LandClim/PNV_LANDCLIM_ROSALIA_historic.csv",
                                          stringsAsFactors = FALSE) %>%
  mutate(climate = "historic") %>%
  filter(AGB != 0)

PNV_LANDCLIM_ROSALIA_rcp45 <- read.csv("../../materials/ROSALIA/output/PNV_results/LandClim/PNV_LANDCLIM_ROSALIA_RCP45.csv",
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP45") %>%
  filter(AGB != 0)

PNV_LANDCLIM_ROSALIA_rcp85 <- read.csv("../../materials/ROSALIA/output/PNV_results/LandClim/PNV_LANDCLIM_ROSALIA_RCP85.csv",
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP85" )%>%
  filter(AGB != 0)

PNV_LANDCLIM_ROSALIA <- bind_rows(PNV_LANDCLIM_ROSALIA_historic, PNV_LANDCLIM_ROSALIA_rcp45, PNV_LANDCLIM_ROSALIA_rcp85) %>%
  mutate(species = case_when(species == "pinucemb" ~ "pice",
                             species == "betupube" ~ "bepu",
                             species == "popunigr" ~ "poni",
                             species == "salialba" ~ "saal",
                             species == "ilexaqui" ~ "ilaq",
                             TRUE ~ .$species))


# both models in one data table

PNV_ROSALIA <- setDT(bind_rows(PNV_ILAND_ROSALIA, PNV_LANDCLIM_ROSALIA))

write_csv(PNV_ROSALIA, "../r/pnv_processed/pnv_rosalia.csv")


# landscape plots ---------------------------------------------------------

PNV_ROSALIA <-setDT(read.csv("../r/pnv_processed/pnv_rosalia.csv"))

# define species colours and factor levels

cols <- c("fasy"="#33CC33", "piab"="#006600", "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900", "abal"="#003300", "acca"="#F3F781", "acpl"="#86B404", "acps"="#58FAD0", "algl"="#61210B", "alin"="#A4A4A4", "alvi"="#0B3B17", "bepe"="#2E64FE", "bepu"="#FF7F00", "cabe"="#F7BE81", "casa"="#A9F5A9", "coav"="#58D3F7", "frex"="#FF0000", "lade"="#8A4B08",  "pice"="#FFB90F", "pini"="#610B21", "pimo"="#000035", "pimu"="#000000", "taba"="#7B4F4B", "ilaq"="#8000FF", "juco"="#DF7401", "pisy"="#B18904", "poni"="#000000", "potr"="#610B5E","saca"="#F5ECCE", "saal"="#00C2A0", "soar"="#E6E0F8", "soau"="#B40404", "tico"="#9F81F7", "tipl"="#8000FF", "ulgl"="#DF7401" )

new_order_gg <- c("ulgl", "tipl", "tico", "soau", "soar", "saca", "saal", "potr", "poni", "pisy", "pini", "pice", "lade", "frex", "coav", "casa","cabe", "bepe", "bepu", "alvi", "alin", "algl", "acps", "acpl", "acca", "abal","qupu", "qupe","quro", "piab", "fasy")

cols_map <- c("01_larch_stonepine"="#FFCC00","02_larch"="#8A4B08","03_subalpine_spruce"="#006600","04_montane_spruce"="#666633","05_spruce_fir"="#0033CC","06_spruce_fir_beech"="#1A8080","07_beech"="#33CC33", "08_oak_hornbeam"="#FF9900","22_silicate_scotspine"="#B18904","23_black_pine"="#610B21", "unclassified"="#D3D3D3")

# create plots

landscape_rosalia <- PNV_ROSALIA %>%
  group_by(model, climate, year, species) %>%
  summarize(AGB = sum(AGB),
            AGB_ha = AGB/1222000) %>%
  ungroup(.) %>%
  mutate(landscape = "rosalia")


all_rosalia <- ggplot(landscape_rosalia, aes(x = year, y = AGB_ha, fill = species)) +
  geom_area() +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  labs(y = "biomass [t/ha]") +
  labs(tag = "A)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold", vjust = 1)) +
  theme(plot.background = element_rect(colour = NA)) +
  facet_grid(model ~ climate, scales = "free")


pnv_supplement <- all_rosalia + all_dischma + plot_layout(ncol = 1)

ggsave("../../results/figures/supplement/pnv_plots.png", pnv_supplement, width = 7, height = 8.5)




# DISCHMA -----------------------------------------------------------------

rm(list=ls())


# bring iLand output in FORMASAM format ---------------------------------

coord <- raster("../../materials/DISCHMA/gis/EnvGrid_Dischma.asc") %>%
  rasterToPoints(.) %>%
  as.data.frame(.) %>%
  rename(ruid = EnvGrid_Dischma)

db.conn <- dbConnect(SQLite(), dbname="../../materials/DISCHMA/output/PNV_DISCHMA_historic.sqlite") 
dbListTables(db.conn)
stand_dischma_historic <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)

db.conn <- dbConnect(SQLite(), dbname="../../materials/DISCHMA/output/PNV_DISCHMA_RCP45.sqlite") 
dbListTables(db.conn)
stand_dischma_rcp45 <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)

db.conn <- dbConnect(SQLite(), dbname="../../materials/DISCHMA/output/PNV_DISCHMA_RCP85.sqlite") 
dbListTables(db.conn)
stand_dischma_rcp85 <- dbReadTable(db.conn, "dynamicstand") %>%
  mutate(model = "ILAND",
         AGB = rowSums(.[, c("foliagemass_sum", "stemmass_sum", "branchmass_sum")]))
dbDisconnect(db.conn)



PNV_ILAND_DISCHMA_historic <- stand_dischma_historic %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))

PNV_ILAND_DISCHMA_rcp45 <- stand_dischma_rcp45 %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))

PNV_ILAND_DISCHMA_rcp85 <- stand_dischma_rcp85 %>%
  dplyr::select(year, rid, species, AGB, model) %>%
  rename(ruid = rid) %>%
  left_join(coord) %>%
  dplyr::select(x, y, ruid, year, species, AGB, model) %>%
  mutate(AGB = round(AGB, 3))


write_csv(PNV_ILAND_DISCHMA_historic, "../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_historic.csv")
write_csv(PNV_ILAND_DISCHMA_rcp45, "../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_RCP45.csv")
write_csv(PNV_ILAND_DISCHMA_rcp85, "../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_RCP85.csv")


rm(list=setdiff(ls(), "all_rosalia"))


#  load pnv output --------------------------------------------------------

# iLand

PNV_ILAND_DISCHMA_historic <- read.csv("../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_historic.csv", 
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "historic")

PNV_ILAND_DISCHMA_rcp45 <- read.csv("../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_RCP45.csv", 
                                    stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP45")

PNV_ILAND_DISCHMA_rcp85 <- read.csv("../../materials/DISCHMA/output/PNV_results/iLand/PNV_ILAND_DISCHMA_RCP85.csv", 
                                    stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP85")

PNV_ILAND_DISCHMA <- bind_rows(PNV_ILAND_DISCHMA_historic, PNV_ILAND_DISCHMA_rcp45, PNV_ILAND_DISCHMA_rcp85)

# LandClim

PNV_LANDCLIM_DISCHMA_historic <- read.csv("../../materials/DISCHMA/output/PNV_results/LandClim/PNV_LANDCLIM_DISCHMA_historic.csv", 
                                          stringsAsFactors = FALSE) %>%
  mutate(climate = "historic")

PNV_LANDCLIM_DISCHMA_rcp45 <- read.csv("../../materials/DISCHMA/output/PNV_results/LandClim/PNV_LANDCLIM_DISCHMA_RCP45.csv", 
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP45")

PNV_LANDCLIM_DISCHMA_rcp85 <- read.csv("../../materials/DISCHMA/output/PNV_results/LandClim/PNV_LANDCLIM_DISCHMA_RCP85.csv", 
                                       stringsAsFactors = FALSE) %>%
  mutate(climate = "RCP85")

PNV_LANDCLIM_DISCHMA <- bind_rows(PNV_LANDCLIM_DISCHMA_historic, PNV_LANDCLIM_DISCHMA_rcp45, PNV_LANDCLIM_DISCHMA_rcp85) %>%
  filter(AGB != 0)


# both models in one data table

PNV_DISCHMA <- setDT(bind_rows(PNV_ILAND_DISCHMA, PNV_LANDCLIM_DISCHMA)) 

write_csv(PNV_DISCHMA, "pnv_processed/pnv_dischma.csv")


# landscape plots ---------------------------------------------------------

PNV_DISCHMA <- setDT(read.csv("pnv_processed/pnv_dischma.csv"))

# define species colours and factor levels

cols <- c("fasy"="#33CC33", "piab"="#006600", "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900", "abal"="#003300", "acca"="#F3F781", "acpl"="#86B404", "acps"="#58FAD0", "algl"="#61210B", "alin"="#A4A4A4", "alvi"="#0B3B17", "bepe"="#2E64FE", "bepu"="#FF7F00", "cabe"="#F7BE81", "casa"="#A9F5A9", "coav"="#58D3F7", "frex"="#FF0000", "lade"="#8A4B08",  "pice"="#FFB90F", "pini"="#610B21", "pimo"="#000035", "pimu"="#000000", "taba"="#7B4F4B", "ilaq"="#8000FF", "juco"="#DF7401", "pisy"="#B18904", "poni"="#000000", "potr"="#610B5E","saca"="#F5ECCE", "saal"="#00C2A0", "soar"="#E6E0F8", "soau"="#B40404", "tico"="#9F81F7", "tipl"="#8000FF", "ulgl"="#DF7401" )

new_order_gg <- c("ulgl", "tipl", "tico", "soau", "soar", "saca", "saal", "potr", "poni", "pisy", "pini", "pice", "lade", "frex", "coav", "casa","cabe", "bepe", "bepu", "alvi", "alin", "algl", "acps", "acpl", "acca", "abal","qupu", "qupe","quro", "piab", "fasy")

cols_map=c("01_larch_stonepine"="#FFCC00","02_larch"="#8A4B08","03_subalpine_spruce"="#006600","04_montane_spruce"="#666633","05_spruce_fir"="#0033CC","06_spruce_fir_beech"="#1A8080","07_beech"="#33CC33", "08_oak_hornbeam"="#FF9900","22_silicate_scotspine"="#B18904","23_black_pine"="#610B21", "unclassified"="#D3D3D3")


# create plots


landscape_dischma <- PNV_DISCHMA %>%
  group_by(model, climate, year, species) %>%
  summarize(AGB = sum(AGB),
            AGB_ha = AGB/923000) %>%
  ungroup(.) %>%
  mutate(landscape = "dischma")


all_dischma <- ggplot(landscape_dischma, aes(x = year, y = AGB_ha, fill = species)) +
  geom_area() +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  labs(y = "biomass [t/ha]") +
  theme_bw() +
  labs(tag = "B)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold", vjust = 1)) +
  scale_x_continuous(expand = c(0.0, 0.0)) +
  theme(plot.background = element_rect(colour = NA),
        legend.position = "none") +
  facet_grid(model ~ climate, scales = "free") 


# pnv plot for supplement -------------------------------------------------

pnv_supplement <- all_rosalia + all_dischma + plot_layout(ncol = 1)

ggsave("../../results/figures/supplement/pnv_plots.png", pnv_supplement, width = 7, height = 8.5)


