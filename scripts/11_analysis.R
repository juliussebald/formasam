# This script builds a model to quantify the effects of different mixtures and levels
# of species diversity on the resistance against natural disturbances

library(tidyverse)
library(lme4)
library(effects)
library(ggthemes)
library(MuMIn)
library(DHARMa)
library(emmeans)
library(ggrepel)
library(merTools)
library(patchwork)
library(vegan)
library(ggridges)
library(hexbin)


# Figure 3 realized diversity ------------------------------------------------------


realized_diversity <- read_csv("../data/realized_diversity_new.csv")%>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future"),
                            labels = c("no", "low", "high", "high+"))) 

species_summary <- read.table("../data/species_summary.txt", header = TRUE) %>%
  mutate(biomass = 1) %>%
  group_by(species, diversity, landscape) %>%
  summarize(biomass_landscape = sum(biomass)) %>%
  spread(species, biomass_landscape) %>%
  mutate_at(.vars = vars(abal:tico), function (x) ifelse(is.na(x), 0, x)) %>%
  ungroup(.) %>%
  mutate(enos = exp(vegan::diversity(dplyr::select(., abal:tico),))) %>%
  dplyr::select(-(abal:tico)) %>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future"),
                            labels = c("no", "low", "high", "high+"))) 


# numbers

real_div_disch <- filter(realized_diversity, landscape == "dischma" & climate == "RCP85" & diversity == "high+")$enos %>%
  max(.)

theo_div_disch <- filter(species_summary, landscape == "dischma" & diversity == "high+")$enos

real_div_disch/theo_div_disch * 100


real_div_rosa <- filter(realized_diversity, landscape == "rosalia" & climate == "historic" & diversity == "high+")$enos %>%
  max(.)

theo_div_rosa <- filter(species_summary, landscape == "rosalia" & diversity == "high+")$enos

real_div_rosa/theo_div_rosa * 100


# plot

realized_diversity_summary <- realized_diversity %>%
  group_by(model,
           landscape,
           climate, 
           diversity) %>%
  summarize(enos_mea = mean(enos),
            enos_min = min(enos),
            enos_max = max(enos))

ggplot() +
  geom_bar(data = species_summary, aes(x = diversity, y = enos), stat = "identity", alpha = 0.15) +
  geom_errorbar(data = realized_diversity_summary,
                aes(x = diversity, ymin = enos_min, ymax = enos_max, group = climate),
                position = position_dodge(width = 0.7),
                width = 0.3) +
  geom_point(data = realized_diversity_summary,
             aes(x = diversity, y = enos_mea, fill = factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")), group = climate),
             pch = 21,
             size = 3,
             position = position_dodge(width = 0.7)) +
  # geom_boxplot(data = realized_diversity,
  #            aes(x = diversity, y = enos, fill = climate),
  #            position = position_dodge2(width = 0.7)) +
  geom_step(data = species_summary, 
            aes(x = diversity, y = enos, linetype = "theoretical maximum", group = 1), 
            position = position_nudge(x = -0.5)) +
  geom_segment(data = filter(species_summary, diversity == "high+"),
               aes(x = 3.5, y = enos, xend = 4.5, yend = enos),
               linetype = "dashed") +
  facet_grid(model ~ factor(landscape, labels = c("Dischma", "Rosalia"))) +
  theme_bw() +
  labs(y = "Effecive number of species",
       x = "Initialized gamma diversity")+
  scale_fill_brewer(name = "Climate scenario",
                    palette = "Reds") +
  scale_linetype_manual(name = "",
                        values = "dashed") +
  theme(legend.justification = "top",
        axis.text.x = element_text(hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  ggsave("../results/figures/main_text/realized_diversity.pdf", 
         width = 7,
         height = 5.5)

# Figure 4 effects of climate, diversity and spatial configuration ----------------------------------------------------


# read in data

data_rel <- read.csv("../data/landscape_rev_rel_no_spec.csv") %>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future"),
                            labels = c("no", "low", "high", "high+")))


effects_agb <- data_rel %>%
  group_by(landscape,
           replication, 
           scenario,
           diversity, 
           climate) %>%
  summarize(agb = mean(AGB_diff_rel)) %>%
  ungroup(.) %>%
  group_by(landscape,
           scenario,
           diversity,
           climate) %>%
  summarize(agb_mea = mean(agb),
            agb_min = min(agb),
            agb_max = max(agb)) %>%
  mutate_at(.vars = vars(agb_mea:agb_max), function(x) - 100 + x) %>%
  ungroup(.)

# biomass

  
p_agb <- ggplot(filter(effects_agb, diversity!= "no")) +  
  stat_smooth(aes(x = diversity, y = agb_mea, group = scenario, col = scenario),
              method = "loess",
              se = FALSE) + 
  geom_hline(data = filter(effects_agb, diversity == "no"),
             aes(yintercept = agb_mea, linetype = "no diversity")) +
  geom_point(aes(x = diversity, y = agb_mea, shape = scenario),
             position = position_dodge(width = 0.75),
             size = 2) +
  geom_errorbar(aes(x = diversity, ymin = agb_min, ymax = agb_max, group = scenario),
                width = 0.1,
                position = position_dodge(width = 0.75)) +
  labs(x = "Gamma diversity", 
       y = "Biomass impact [%]",
       tag = "A)") +
  #coord_flip() +
  #ylim(-7,0) +
  facet_grid(factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")) ~ 
               factor(landscape, labels = c("Dischma", "Rosalia"))) +
  theme_bw()  +
  scale_colour_manual(name = "",
                      labels = c("alpha", "beta"),
                      values = c("#b30000", "#253494")) +   
  scale_shape_manual(name = "",
                     labels = c("alpha", "beta"),
                     values = c(16, 17)) +
  scale_linetype_manual(name = "",
                        values = c("dashed")) +
  theme(legend.position = "top",
        legend.justification = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

  
# big trees

# read in data

data_abs <- read.csv("../data/landscape_rev_diff_no_spec.csv") %>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future"),
                            labels = c("no", "low", "high", "high+")))

effects_bt <- data_abs %>%
  group_by(landscape,
           replication, 
           scenario,
           diversity, 
           climate) %>%
  summarize(bt = mean(big_trees_diff)) %>%
  ungroup(.) %>%
  group_by(landscape,
           scenario,
           diversity,
           climate) %>%
  summarize(bt_mea = mean(bt),
            bt_min = min(bt),
            bt_max = max(bt)) %>%
  mutate_at(.vars = vars(bt_mea:bt_max), function(x) x * -1) %>%
  ungroup(.)



# plot effects

p_bt <- ggplot(filter(effects_bt, diversity != "no")) +  
  stat_smooth(aes(x = diversity, y = bt_mea, group = scenario, col = scenario),
              method = "loess",
              se = FALSE) + 
  geom_hline(data = filter(effects_bt, diversity == "no"), 
             aes(yintercept = bt_mea, linetype = "no diversity")) +
  geom_point(aes(x = diversity, y = bt_mea, shape = scenario),
             position = position_dodge(width = 0.75),
             size = 2) +
  geom_errorbar(aes(x = diversity, ymin = bt_min, ymax = bt_max, group = scenario),
                width = 0.1,
                position = position_dodge(width = 0.75)) +
  labs(x = "Gamma diversity", 
       y = bquote("Structural impact [trees "*ha^-1*"]"),
       tag = "B)") +
  facet_grid(factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")) ~ 
               factor(landscape, labels = c("Dischma", "Rosalia"))) +
  theme_bw() +  
  scale_shape_manual(name = "",
                     labels = c("alpha", "beta"),
                     values = c(16, 17)) +
  scale_colour_manual(name = "",
                      labels = c("alpha", "beta"),
                      values = c("#b30000", "#253494")) +   
  scale_linetype_manual(name = "",
                        values = c("dashed")) +
  theme(legend.position = "top",
        legend.justification = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())


# combined plots

p_effects_bt_agb <- p_agb + p_bt + plot_layout(ncol = 2)

ggsave("../results/figures/main_text/effects.pdf", p_effects_bt_agb,  width = 7, height = 5)


#  Figure 5 temporal variation depending on gamma diversity ---------------


# arrange data

# mean variation

diversity_temporal <- read.csv("../data/variation_temporal.csv") %>%
  group_by(diversity,
           landscape,
           climate) %>%
  summarize(temporal_agb_mea = mean(cv_temporal_agb),
            temporal_bt_mea = mean(cv_temporal_bt)) %>%
  gather(indicator, value, -(diversity:climate))

# minimum variation

diversity_min <- read.csv("../data/variation_temporal.csv") %>%
  group_by(climate,
           diversity,
           landscape) %>%
  summarize(temporal_agb_mea = min(cv_temporal_agb),
            temporal_bt_mea = min(cv_temporal_bt)) %>%
  gather(indicator, min, -(climate:landscape))

# maximum variation

diversity_max <- read.csv("../data/variation_temporal.csv") %>%
  group_by(climate,
           diversity,
           landscape) %>%
  summarize(temporal_agb_mea = max(cv_temporal_agb),
            temporal_bt_mea = max(cv_temporal_bt)) %>%
  gather(indicator, max, -(climate:landscape)) 

# data errorbars

diversity_errorbar <- diversity_min %>%
  left_join(diversity_max)


# plotdata

diversity_data <- diversity_temporal %>%
  left_join(diversity_errorbar) %>%
  ungroup(.) %>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future")))

# facet labels

indicator_labs <- c("biomass", 
                    "structure")

names(indicator_labs) <- c("temporal_agb_mea", "temporal_bt_mea")


# plot

ggplot(diversity_data, aes(x = factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")), 
                           y = value, 
                           fill = diversity, 
                           group = diversity)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  facet_grid(indicator ~ factor(landscape, labels = c("Dischma", "Rosalia")), 
             scales = "free", 
             labeller = labeller(indicator = indicator_labs)) +
  scale_fill_manual(name = "Gamma diversity",
                    labels = c("no", "low", "high", "high+"),
                    values = c("#d9d9d9", "#c7e9c0", "#41ab5d", "#006d2c")) +
  geom_errorbar(aes(x = factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")),
                    ymin = min,
                    ymax = max,
                    group = diversity),
                position = position_dodge2(padding = 0.7))+
  theme_bw() +
  labs(y = "Temporal variation (CV)",
       x = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  ggsave("../results/figures/main_text/variation.pdf",
         width = 6, 
         height = 4)

 
# Figure 6 model differences ------------------------------------

# read in data

data_rel <- read.csv("../data/landscape_rev_rel_no_spec.csv") %>%
  filter(disturbances != "no") %>%
  mutate(diversity = factor(diversity, levels = c("no", "low", "high", "high_future"),
                            labels = c("no", "low", "high", "high+"))) %>%
  mutate_at(.vars = vars(AGB_diff_rel), function(x) - 100 + x)


hline <- data_rel %>%
  filter(diversity == "no") %>%
  group_by(landscape,
           diversity, 
           model) %>%
  summarize(agb = mean(AGB_diff_rel)) 

effects_agb <- data_rel %>%
  group_by(landscape,
           replication, 
           scenario,
           diversity,
           model) %>%
  summarize(agb = mean(AGB_diff_rel))
  
ggplot(filter(effects_agb, diversity!= "no")) +  
  
  geom_boxplot(aes(x = diversity, y = agb, fill = scenario),
               position = position_dodge2()) +
  geom_hline(data = hline,
             aes(yintercept = agb, linetype = "no diversity")) +
  # stat_smooth(aes(x = diversity, y = agb, group = model), col = "black",
  #             method = "loess",
  #             se = FALSE) +
  labs(x = "Gamma diversity", 
       y = "Biomass impact [%]") +
  #coord_flip() +
  #ylim(-7,0) +
  facet_grid(model ~ factor(landscape, labels = c("Dischma", "Rosalia"))) +
  theme_bw()  +
  scale_fill_manual(name = "",
                    labels = c("alpha", "beta"),
                    values = c("#b30000", "#253494")) +
  scale_linetype_manual(name = "",
                        values = c("dashed")) +
  theme(panel.grid.major = element_blank(),
        legend.justification = "top",
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  ggsave("../results/figures/main_text/model_differences.pdf",
         width = 6,
         height = 4.5)

# SI 1 PNV runs -------------------------------

# load data

landscape_pnv <- read_csv("../data/landscape_pnv.csv")

# define species colours and factor levels

cols <- c("fasy"="#33CC33", "piab"="#006600", "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900", "abal"="#003300", "acca"="#F3F781", "acpl"="#86B404", "acps"="#58FAD0", "algl"="#61210B", "alin"="#A4A4A4", "alvi"="#0B3B17", "bepe"="#2E64FE", "bepu"="#FF7F00", "cabe"="#F7BE81", "casa"="#A9F5A9", "coav"="#58D3F7", "frex"="#FF0000", "lade"="#8A4B08",  "pice"="#FFB90F", "pini"="#610B21", "pimo"="#000035", "pimu"="#000000", "taba"="#7B4F4B", "ilaq"="#8000FF", "juco"="#DF7401", "pisy"="#B18904", "poni"="#000000", "potr"="#610B5E","saca"="#F5ECCE", "saal"="#00C2A0", "soar"="#E6E0F8", "soau"="#B40404", "tico"="#9F81F7", "tipl"="#8000FF", "ulgl"="#DF7401" )

new_order_gg <- c("ulgl", "tipl", "tico", "soau", "soar", "saca", "saal", "potr", "poni", "pisy", "pini", "pice", "lade", "frex", "coav", "casa","cabe", "bepe", "bepu", "alvi", "alin", "algl", "acps", "acpl", "acca", "abal","qupu", "qupe","quro", "piab", "fasy")

cols_map <- c("01_larch_stonepine"="#FFCC00","02_larch"="#8A4B08","03_subalpine_spruce"="#006600","04_montane_spruce"="#666633","05_spruce_fir"="#0033CC","06_spruce_fir_beech"="#1A8080","07_beech"="#33CC33", "08_oak_hornbeam"="#FF9900","22_silicate_scotspine"="#B18904","23_black_pine"="#610B21", "unclassified"="#D3D3D3")



rosalia <- ggplot(filter(landscape_pnv, landscape == "rosalia"), 
                  aes(x = year, y = AGB_ha, fill = species)) +
  geom_area() +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Biomass [t/ha]",
       x = "Year") +
  labs(tag = "A)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold", vjust = 1)) +
  theme(plot.background = element_rect(colour = NA)) +
  facet_grid(model ~ factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")), scales = "free")


dischma <- ggplot(filter(landscape_pnv, landscape == "dischma"),
                  aes(x = year, y = AGB_ha, fill = species)) +
  geom_area() +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Biomass [t/ha]",
       x = "Year") +
  theme_bw() +
  labs(tag = "B)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold", vjust = 1)) +
  scale_x_continuous(expand = c(0.0, 0.0)) +
  theme(plot.background = element_rect(colour = NA),
        legend.position = "none") +
  facet_grid(model ~ factor(climate, labels = c("historic", "RCP4.5", "RCP8.5")) , scales = "free") 




pnv_supplement <- rosalia + dischma + plot_layout(ncol = 1)

ggsave("../results/figures/supplement/pnv_plots.pdf", pnv_supplement, width = 7, height = 8.5)


# SI 2 Species pools -------------------------------


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

species_summary <- read.table("../data/species_summary.txt", header = TRUE) 

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
             ~ "Dischma") +
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
             ~ "Rosalia") +
  labs(x = "", y = "", fill = "included") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())



species_pools <- species_pools_dischma + species_pools_rosalia + plot_layout(ncol = 2)

ggsave( "../results/figures/supplement/species_pools.pdf", species_pools, height = 7, width = 12)



# SI 3 Patch size distribution derived from Senf et al. 2017

modifier <- 2

patch_size <- mean(c(1.17, 1.17, 1.35, 0.99, 1.08)) * modifier # derived from Senf 2017, ISPRS

patches<- data.frame(patchsize = rexp(5000, 1/patch_size)) %>%
  round(., 0) %>%
  filter(patchsize > 0)

ggplot(patches, aes(x = factor(patchsize))) +
  geom_histogram(stat = "count") +
  labs(y = "Frequency", x = "Patchsize [ha]") +
  theme_few() 


# SI 4 Disturbance probability functions ----------------------------------

height <- c(1:50)

dbh <- seq(50, 50, length.out = 50)



constant <- 3 # this constant value describes the topographic exposure of a given stand to 
# wind disturbances (Topex to distance Index).
# We decided for a value of 3 which correspondeds to a west exposed slope with 30 % 
# gradient.
# piab



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

species_and_height <- data.frame(dbh = dbh,
           height = height,
           piab = p_piab,
           other_broadleaved = p_obl,
           fasy = p_fasy_qupe,
           pisy_lade = p_pisy_lade,
           abal_psme = p_abal_psme) %>%
  gather(species, prob, piab:abal_psme) %>%
  ggplot(., aes(x = height, y = prob, col = factor(species, labels = c("Abal and Psme", "Fasy", "Other broadleaved", "Piab", "Pisy and Lade")))) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "Probability of disturbance", col = "Species group", x = "Mean tree height") +
  theme_bw()


ggsave("../results/figures/supplement/disturbance_probability_function.pdf", species_and_height, width = 5, height = 3.5)

# SI 5 temporal variation for every climate scenario and both spatial configurations ----------------------------------------------------------------------

# create data

# mean variation

scenario_temporal <- read.csv("../data/variation_temporal.csv") %>%
  filter(diversity != "no") %>%
  group_by(scenario,
           landscape,
           climate,) %>%
  summarize(temporal_agb_mea = mean(cv_temporal_agb),
            temporal_bt_mea = mean(cv_temporal_bt)) %>%
  gather(indicator, value, -(scenario:climate))


# minimum variation

scenario_min <- read.csv("../data/variation_temporal.csv") %>%  
  filter(diversity != "no") %>%
  group_by(scenario,
           climate,
           landscape) %>%
  summarize(temporal_agb_mea = min(cv_temporal_agb),
            temporal_bt_mea = min(cv_temporal_bt)) %>%
  gather(indicator, min, -(scenario:landscape))


# maximum variation

scenario_max <- read.csv("../data/variation_temporal.csv") %>%
  filter(diversity != "no") %>%
  group_by(scenario,
           climate,
           landscape) %>%
  summarize(temporal_agb_mea = max(cv_temporal_agb),
            temporal_bt_mea = max(cv_temporal_bt)) %>%
  gather(indicator, max, -(scenario:landscape))



# data for errorbars

scenario_errorbar <- scenario_min %>%
  left_join(scenario_max)

scenario_data <- scenario_temporal %>%
  left_join(scenario_errorbar) %>%
  mutate(climate = factor(climate, labels = c("historic", "RCP4.5", "RCP8.5"))) 


# facet labels

indicator_labs <- c("Biomass", 
                    "Structure")

names(indicator_labs) <- c("temporal_agb_mea", "temporal_bt_mea")


ggplot(scenario_data, aes(x = climate,
                          y = value, fill = scenario, group = scenario)) +
  geom_col(position = position_dodge2(width = 1.2)) +
  facet_grid(indicator ~ factor(landscape, labels = c("Dischma", "Rosalia")),
             scales = "free", 
             labeller = labeller(indicator = indicator_labs)) +
  scale_fill_manual(name = "",
                    labels = c("alpha", "beta", "no diversity"),
                    values = c("#b30000", "#253494", "#bdbdbd")) +
  geom_errorbar(aes(x = climate,
                    ymin = min,
                    ymax = max,
                    group = scenario),
                position = position_dodge2(padding = 1.2))+
  theme_bw() +
  labs(y = "Temporal variation (CV)",
       x = "Climate scenario") +
  theme(legend.justification = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  ggsave("../results/figures/supplement/variaton_scenario.pdf",
         width = 6, 
         height = 4)



#  SI6 and SI7 tables with model differences and absolute values ------------------------------------


data_biomass <- read.csv("../data/landscape_rev_rel_no_spec.csv") %>%
  group_by(landscape,
           model,
           scenario,
           diversity, 
           replication) %>%
  summarize(biomass = mean(AGB_diff_rel)) %>%
  group_by(landscape, 
           model, 
           scenario, 
           diversity) %>%
  summarize(biomass_mea = -100 + mean(biomass),
            biomass_min = -100 + min(biomass),
            biomass_max = -100 + max(biomass))

data_big_trees <- read.csv("../data/landscape_rev_diff_no_spec.csv") %>%
  group_by(landscape,
           model,
           scenario,
           diversity, 
           replication) %>%
  summarize(big_trees = mean(big_trees_diff)) %>%
  group_by(landscape, 
           model, 
           scenario,
           diversity) %>%
  summarize(big_trees_mea = -1 * mean(big_trees),
            big_trees_min = -1 * min(big_trees),
            big_trees_max = -1 * max(big_trees))

data_cv <- read.csv("../data/variation_temporal.csv") %>%
  group_by(landscape,
           model,
           climate,
           diversity) %>%
  summarize(cv_agb = mean(cv_temporal_agb),
            cv_bt = mean(cv_temporal_bt)) %>%
  mutate_at(.vars = vars(cv_agb:cv_bt), function(x) round(x,2))



data_table <- data_biomass %>%
  left_join(data_big_trees) %>%
  mutate_at(.vars = vars(biomass_mea:big_trees_max), function(x) round(x,2))

write_csv(data_table, "../results/tables/model_differences.csv")

write_csv(data_cv, "../results/tables/model_differences_cv.csv")

# create table with absolute values

data_abs <- read_csv("../data/landscape_rev_no_spec.csv") %>%
  group_by(landscape,
           model,
           scenario,
           diversity) %>%
  summarize(biomass = round(mean(AGB_t_ha),2),
            big_trees = round(mean(big_trees_ha),2)) 

write_csv(data_abs, "../results/tables/model_differences_abs.csv")


# SI 8 Sensitivity Analysis disturbance rotation period ----------------------------------------------------

# load data

toc <- read.csv("../data/table_of_combinations_revision.csv", stringsAsFactors = FALSE)

landscape_revision <- read_csv("../data/landscape_revision.csv")


landscape_old_1 <- read_csv("../data/landscape_rev_no_spec.csv") %>%
  mutate(age_structure = "normal",
         dist_impact = "species_height") %>%
  dplyr::select(-run_id) %>%
  filter(climate != "RCP45" & diversity != "high" & replication < 11)

baseline_1 <- filter(landscape_old_1, disturbances == "no") %>% 
  rename(AGB_base = AGB_t_ha) %>%
  ungroup(.) %>%
  dplyr::select(-disturbances)

experiment_1 <- filter(landscape_revision, experiment == 1) %>%
  ungroup(.) %>%
  dplyr::select(-experiment) %>%
  bind_rows(landscape_old_1) %>%
  filter(disturbances != "no") %>%
  left_join(baseline_1, by = c("model",
                               "landscape",
                               "climate",
                               "year",
                               "replication",
                               "diversity",
                               "scenario",
                               "age_structure",
                               "dist_impact")) %>%
  mutate(AGB_diff_rel = (AGB_t_ha / AGB_base) * 100) %>%
  dplyr::select(model:age_structure, AGB_diff_rel) %>%
  ungroup(.) 


p1 <- experiment_1 %>%
  ggplot(., aes(x = factor(disturbances, 
                           levels = c("future_100", "future", "historic", "historic_600"), 
                           labels = c("100", "200", "400", "600")), 
                y = AGB_diff_rel - 100, fill = factor(scenario, levels = c("prod", "alpha", "beta")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Spatial configuration",
                    labels = c("no diversity","alpha", "beta"),
                    values = c("grey", "#b30000", "#253494")) +
  labs(x = "Disturbance rotation period",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~ factor(landscape, labels = c("Dischma", "Rosalia")))

p2 <- experiment_1 %>%
  ggplot(., aes(x = factor(disturbances, 
                           levels = c("future_100", "future", "historic", "historic_600"), 
                           labels = c("100", "200", "400", "600")), 
                y = AGB_diff_rel - 100, fill = factor(diversity, 
                                                      levels = c("no", "low", "high_future")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Gamma diversity",
                    labels = c("no", "low", "high+"),
                    values = c("grey", "#ec7014", "#006d2c")) +
  labs(x = "Disturbance rotation period",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~ factor(landscape, labels = c("Dischma", "Rosalia")))


exp_1_biomass <- p1 + p2 + plot_layout(ncol = 1) 

ggsave("../results/figures/supplement/exp_1.pdf", exp_1_biomass,  width = 7, height = 7)

# SI 9 Sensitivity analysis age class sturcutre ---------------------------------------------------

x <- dweibull((1:100)/100, shape = 4, scale = 1) + 0.35 

x <- x/ sum(x)# sum = 1


age_class_structure_inital <- data.frame(stand = c(1:10000)) %>%
  mutate(young = 100 - sample(x = 1:100, size = nrow(.), prob = x, replace = T)) %>%
  mutate(old = sample(x = 1:100, size = nrow(.), prob = x, replace = T)) %>%
  mutate(normal = sample(x = 1:100, size = nrow(.), replace = T)) %>%
  gather(age_class_structure, age, -stand) %>%
  group_by(age_class_structure) %>%
  mutate(mean_age = mean(age)) %>%
  ungroup(.)


ggplot(age_class_structure_inital, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = mean_age), col = "red") +
  facet_wrap(~ factor(age_class_structure, 
                      labels = c("Normal", "Right skewed", "Left skewed"))) +
  labs(x = "Stand age",
       y = "Frequency") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()) +
  ggsave("../results/figures/supplement/age_class_structure_inital.pdf", width = 7, height = 2.5)

# SI 10 Sensitivity analysis age class sturcutre ---------------------------------------------------

toc <- read.csv("../data/table_of_combinations_revision.csv", stringsAsFactors = FALSE)

landscape_revision <- read_csv("../data/landscape_revision.csv")


landscape_old_2 <- read.csv("../data/landscape_rev_no_spec.csv") %>%
  mutate(age_structure = "normal",
         dist_impact = "species_height") %>%
  dplyr::select(-run_id) %>%
  filter(climate != "RCP45" & diversity != "high" & replication < 11 & disturbances != "historic")

baseline_2 <- filter(landscape_revision, experiment == 2) %>%
  ungroup(.) %>%
  dplyr::select(-experiment) %>%
  as.data.frame() %>%
  filter(disturbances == "no") %>%
  bind_rows(filter(landscape_old_2, disturbances == "no")) %>%
  rename(AGB_base = AGB_t_ha) %>%
  ungroup(.) %>%
  dplyr::select(-disturbances)

experiment_2 <- filter(landscape_revision, experiment == 2) %>%
  ungroup(.) %>%
  dplyr::select(-experiment) %>%
  as.data.frame() %>%
  bind_rows(landscape_old_2) %>%
  filter(disturbances != "no") %>%
  left_join(baseline_2, by = c("model",
                               "landscape",
                               "climate",
                               "year",
                               "replication",
                               "diversity",
                               "scenario",
                               "age_structure",
                               "dist_impact")) %>%
  mutate(AGB_diff_rel = (AGB_t_ha / AGB_base) * 100) %>%
  dplyr::select(model:age_structure, AGB_diff_rel) %>%
  ungroup(.) 


p5 <- experiment_2 %>%
  ggplot(., aes(x = factor(age_structure, 
                           levels = c("young", "normal", "old"), 
                           labels = c("left skewed", "normal", "right skewed")), 
                y = AGB_diff_rel-100, fill = factor(scenario, levels = c("prod", "alpha", "beta")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Spatial configuration",
                    labels = c("no diversity","alpha", "beta"),
                    values = c("grey", "#b30000", "#253494")) +
  labs(x = "Age class structure",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~factor(landscape, labels = c("Dischma", "Rosalia")))

p6 <- experiment_2 %>%
  ggplot(., aes(x = factor(age_structure, 
                           levels = c("young", "normal", "old"), 
                           labels = c("left skewed", "normal", "right skewed")), 
                y = AGB_diff_rel-100, fill = factor(diversity, levels = c("no", "low", "high_future")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Gamma diversity",
                    labels = c("no", "low", "high+"),
                    values = c("grey", "#ec7014", "#006d2c")) +
  labs(x = "Age class structure",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~ factor(landscape, labels = c("Dischma", "Rosalia")))


exp_2_biomass <- p5 + p6 + plot_layout(ncol = 1)

ggsave("../results/figures/supplement/exp_2.pdf", exp_2_biomass,  width = 7, height = 7)

# SI 11 Sensitivity analysis disturbance impact ---------------------------------------------------


height <- c(1:50)

dbh <- seq(50, 50, length.out = 50)



constant <- 3 # this constant value describes the topographic exposure of a given stand to 
# wind disturbances (Topex to distance Index).
# We decided for a value of 3 which correspondeds to a west exposed slope with 30 % 
# gradient.
# piab

# species and height

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

species_and_height <- data.frame(dbh = dbh,
                                 height = height,
                                 piab = p_piab,
                                 other_broadleaved = p_obl,
                                 fasy = p_fasy_qupe,
                                 pisy_lade = p_pisy_lade,
                                 abal_psme = p_abal_psme) %>%
  gather(species, prob, piab:abal_psme) %>%
  ggplot(., aes(x = height, y = prob, col = species)) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "Probability of disturbance", col = "", x = "") +
  facet_wrap(~"Original") +
  theme_bw() +
  theme(legend.position = "none") 


# height only

logit_pisy_lade  <- -8.59 + log((dbh^(-1.625)) / ((height)^(-3.525))) + constant

# logistic link function

p_pisy_lade <- exp(logit_pisy_lade) / (exp(logit_pisy_lade) + 1)




# data frame

species_labels <- factor("pisy_lade", 
                         labels = "Only height depended")

height_only <- data.frame(dbh = dbh,
                          height = height,
                          prob = p_pisy_lade) %>%
  ggplot(., aes(x = height, y = prob)) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "", x = "Mean stand height [m]") +
  theme_bw() +
  facet_wrap(~"Height only")

# random

random <- data.frame(dbh = dbh,
                     height = height,
                     prob = 0.5) %>%
  ggplot(., aes(x = height, y = prob)) +
  geom_line() +
  ylim(0, 1) +
  labs(y = "", x = "") +
  theme_bw() +
  facet_wrap(~"Random")


tree_disturbance_functions <-  species_and_height + height_only + random + plot_layout(ncol = 3)

ggsave("../results/figures/supplement/tree_disturbance_functions.pdf", tree_disturbance_functions, width = 7.5 , height = 3.5)


# SI 12 Sensitivity analysis disturbance impact ---------------------------------------------------


toc <- read.csv("../data/table_of_combinations_revision.csv", stringsAsFactors = FALSE)

landscape_revision <- read_csv("../data/landscape_revision.csv")


landscape_old_3 <- read.csv("../data/landscape_rev_no_spec.csv") %>%
  mutate(age_structure = "normal",
         dist_impact = "species_height") %>%
  dplyr::select(-run_id) %>%
  filter(climate != "RCP45" & 
           diversity != "high" & 
           replication < 11 & 
           disturbances != "historic")


baseline_3 <- filter(landscape_old_3, disturbances == "no") %>%
  rename(AGB_base = AGB_t_ha,
         big_trees_base = big_trees_ha) %>%
  ungroup(.) %>%
  dplyr::select(-disturbances, -dist_impact)


experiment_3 <- filter(landscape_revision, experiment == 3) %>%
  ungroup(.) %>%
  dplyr::select(-experiment) %>%
  as.data.frame() %>% 
  bind_rows(landscape_old_3) %>%
  filter(disturbances != "no") %>%
  left_join(baseline_3, by = c("model",
                               "landscape",
                               "climate",
                               "year",
                               "replication",
                               "diversity",
                               "scenario",
                               "age_structure")) %>%
  mutate(AGB_diff_rel = (AGB_t_ha / AGB_base) * 100) %>%
  dplyr::select(model:dist_impact, AGB_diff_rel) %>%
  ungroup(.) 


p9 <- experiment_3 %>%
  ggplot(., aes(x = factor(dist_impact, 
                           levels = c("species_height", "height_only", "random"), 
                           labels = c("original", "height only", "random")), 
                y = AGB_diff_rel-100, fill = factor(scenario, levels = c("prod", "alpha", "beta")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Spatial configuration",
                    labels = c("no diversity","alpha", "beta"),
                    values = c("grey", "#b30000", "#253494")) +
  labs(x = "Disturbance impact model",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~ factor(landscape, labels = c("Dischma", "Rosalia")))

p10 <- experiment_3 %>%
  ggplot(., aes(x = factor(dist_impact, 
                           levels = c("species_height", "height_only", "random"), 
                           labels = c("orignal", "height only", "random")),  
                y = AGB_diff_rel - 100, fill = factor(diversity, levels = c("no", "low", "high_future")))) +
  geom_boxplot() +
  scale_fill_manual(name = "Gamma diversity",
                    labels = c("no", "low", "high+"),
                    values = c("grey", "#ec7014", "#006d2c")) +
  labs(x = "Disturbance impact model",
       y = "Biomass impact [%]",
       fill = "Spatial configuration") +
  theme_bw() +
  facet_grid(~ factor(landscape, labels = c("Dischma", "Rosalia")))


exp_3_biomass <- p9 + p10 + plot_layout(ncol = 1)


ggsave("../results/figures/supplement/exp_3.pdf", exp_3_biomass,  width = 7, height = 7)








