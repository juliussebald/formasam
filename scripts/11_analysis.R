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
           aes(x = diversity, y = enos_mea, fill = climate, group = climate),
           pch = 21,
           size = 3,
           position = position_dodge(width = 0.7)) +
  geom_step(data = species_summary, 
            aes(x = diversity, y = enos, linetype = "theoretical maximum", group = 1), 
            position = position_nudge(x = -0.5)) +
  geom_segment(data = filter(species_summary, diversity == "high+"),
               aes(x = 3.5, y = enos, xend = 4.5, yend = enos),
               linetype = "dashed") +
  facet_grid(model ~ landscape) +
  theme_bw() +
  labs(y = "effecive number of species",
       x = "initialized gamma diversity")+
  scale_fill_brewer(name = "climate scenario",
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
  labs(x = "gamma diversity", 
       y = "biomass impact [%]",
       tag = "A)") +
  #coord_flip() +
  #ylim(-7,0) +
  facet_grid(climate ~ landscape) +
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
  labs(x = "gamma diversity", 
       y = bquote("structural impact [trees "*ha^-1*"]"),
       tag = "B)") +
  facet_grid(climate ~ landscape) +
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

ggsave("../results/figures/main_text/effects_bt_agb.pdf", p_effects_bt_agb,  width = 7, height = 5)

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

ggplot(diversity_data, aes(x = climate, y = value, fill = diversity, group = diversity)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  facet_grid(indicator ~ landscape, scales = "free", 
             labeller = labeller(indicator = indicator_labs)) +
  scale_fill_manual(name = "gamma diversity",
                    labels = c("no", "low", "high", "high+"),
                    values = c("#d9d9d9", "#c7e9c0", "#41ab5d", "#006d2c")) +
  geom_errorbar(aes(x = climate,
                    ymin = min,
                    ymax = max,
                    group = diversity),
                position = position_dodge2(padding = 0.7))+
  theme_bw() +
  labs(y = "temporal variation (CV)",
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
  labs(x = "gamma diversity", 
       y = "biomass impact [%]") +
  #coord_flip() +
  #ylim(-7,0) +
  facet_grid(model ~ landscape, scales = "free") +
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


# SI 5 temporal variation for every climate scneario and both spatial configurations ----------------------------------------------------------------------

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
  left_join(scenario_errorbar) 


# facet labels

indicator_labs <- c("biomass", 
                    "structure")

names(indicator_labs) <- c("temporal_agb_mea", "temporal_bt_mea")


ggplot(scenario_data, aes(x = climate, y = value, fill = scenario, group = scenario)) +
  geom_col(position = position_dodge2(width = 1.2)) +
  facet_grid(indicator ~ landscape, scales = "free", 
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
  labs(y = "temporal variation (CV)",
       x = "climate scenario") +
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


