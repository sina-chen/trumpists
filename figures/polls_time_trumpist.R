#-------------------------------------------------------------------------------
# Did “Trumpists” perform worse in US Senate election polls?: poll development
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Data --------------------------------------------------------------------

polls <- readRDS("~/data/senate_polls1998_2020_score.RDS")


# Preparation -------------------------------------------------------------

# Prepare data: 
# remove observations with missing information on sample size
# subset polls for 2018 and 2020
# generate state-year groups
# compute endorsement and incumbency dummies  
polls <- polls  %>%
  mutate(n = na_if(n, '-'))  %>%
  subset(is.na(n) == F & election_year >= 2018)  %>% 
  mutate(state_year = paste0(state, election_year),
         state_year_int = as.integer(as.factor(state_year)),
         end_trump = if_else(state == 'WV' & election_year == 2020 |
                               state == 'LA' & election_year == 2020|
                               state == 'TX' & election_year == 2020|
                               state == 'MT' & election_year == 2020|
                               state == 'IA' & election_year == 2020|
                               state == 'CO' & election_year == 2020|
                               state == 'SC' & election_year == 2020|
                               state == 'TN' & election_year == 2020|
                               state == 'MS' & election_year == 2020|
                               state == 'OK' & election_year == 2020|
                               state == 'MN' & election_year == 2020|
                               state == 'WY' & election_year == 2020|
                               state == 'KS' & election_year == 2020|
                               state == 'KY' & election_year == 2020|
                               state == 'NH' & election_year == 2020|
                               state == 'GA' & election_year == 2020|
                               state == 'ID' & election_year == 2020|
                               state == 'SD' & election_year == 2020|
                               state == 'NE' & election_year == 2020|
                               state == 'AK' & election_year == 2020|
                               state == 'NC' & election_year == 2020|
                               state == 'AL' & election_year == 2020|
                               state == 'AZ' & election_year == 2018|
                               state == 'IN' & election_year == 2018|
                               state == 'FL' & election_year == 2018|
                               state == 'MI' & election_year == 2018|
                               state == 'MN' & election_year == 2018|
                               state == 'MS' & election_year == 2018|
                               state == 'MO' & election_year == 2018|
                               state == 'MT' & election_year == 2018|
                               state == 'NE' & election_year == 2018|
                               state == 'NV' & election_year == 2018|
                               state == 'NJ' & election_year == 2018|
                               state == 'ND' & election_year == 2018|
                               state == 'OH' & election_year == 2018|
                               state == 'PA' & election_year == 2018|
                               state == 'TN' & election_year == 2018|
                               state == 'TX' & election_year == 2018|
                               state == 'UT' & election_year == 2018|
                               state == 'WV' & election_year == 2018|
                               state == 'WI' & election_year == 2018|
                               state == 'WY' & election_year == 2018, 1, 0),
         inc_rep = if_else(rep_candidate == senator, 1, 0),
         battle = if_else(state == 'AZ' & election_year == 2018|
                            state == 'CA' & election_year == 2018|
                            state == 'FL' & election_year == 2018|
                            state == 'IN' & election_year == 2018|
                            state == 'MN' & election_year == 2018|
                            state == 'MO' & election_year == 2018|
                            state == 'MS' & election_year == 2018|
                            state == 'MT' & election_year == 2018|
                            state == 'ND' & election_year == 2018|
                            state == 'NJ' & election_year == 2018|
                            state == 'NM' & election_year == 2018|
                            state == 'NV' & election_year == 2018|
                            state == 'TN' & election_year == 2018|
                            state == 'TX' & election_year == 2018|
                            state == 'WV' & election_year == 2018|
                            state == 'AL' & election_year == 2020|
                            state == 'CO' & election_year == 2020|
                            state == 'GA' & election_year == 2020|
                            state == 'IA' & election_year == 2020|
                            state == 'KS' & election_year == 2020|
                            state == 'KN' & election_year == 2020|
                            state == 'ME' & election_year == 2020|
                            state == 'MI' & election_year == 2020|
                            state == 'MN' & election_year == 2020|
                            state == 'MT' & election_year == 2020|
                            state == 'NH' & election_year == 2020|
                            state == 'NC' & election_year == 2020|
                            state == 'SC' & election_year == 2020|
                            state == 'TX' & election_year == 2020, 1, 0),
         end_inc = case_when(end_trump == 1 & inc_rep == 1 ~ 'Endorsed & incumbent',
                             end_trump == 1 & inc_rep == 0 ~ 'Endorsed & challenger',
                             end_trump == 0 & inc_rep == 1 ~ 'Not endorsed & incumbent',
                             end_trump == 0 & inc_rep == 0 ~ 'Not endorsed & challenger'),
         end_bat = case_when(end_trump == 1 & battle == 1 ~ 'Endorsed & battleground',
                             end_trump == 1 & battle == 0 ~ 'Endorsed & safe state',
                             end_trump == 0 & battle == 1 ~ 'Not endorsed & battleground',
                             end_trump == 0 & battle == 0 ~ 'Not endorsed & safe state'))


polls <- polls %>% 
  mutate(end_trump_factor = if_else(end_trump == 1, "Endorsed", "Not endorsed"),
         inc_rep_factor = if_else(inc_rep == 1, "Incumbent", "Challenger"),
         bat_factor = if_else(battle == 1, "Battleground", "Safe state"),
         end_inc =  factor(end_inc, levels = c("Endorsed & challenger", 
                                               "Not endorsed & incumbent", 
                                               "Endorsed & incumbent", 
                                               "Not endorsed & challenger")),
         end_bat =  factor(end_bat, levels = c("Endorsed & safe state",
                                               "Not endorsed & battleground", 
                                               "Endorsed & battleground", 
                                               "Not endorsed & safe state")))

# Plot --------------------------------------------------------------------

plot_t_end <- ggplot(subset(polls, t <= 365), aes(x = t, y = rep_poll2 - rep_result2, 
                                                  color = end_trump_factor)) +
  geom_point() +
  facet_wrap(~election_year, scales = "free_x") +
  theme_bw() +
  scale_x_reverse() +
  labs(y = "", x = "") +
  scale_color_manual(name = "", values = c("#E69F00", "#999999")) +
  geom_smooth(method = "lm", se = F) +
  theme(text = element_text(size = 12), axis.text = element_text(size = 12))

plot_t_inc <- ggplot(subset(polls, t <= 365), aes(x = t, y = rep_poll2 - rep_result2, 
                                                  color = inc_rep_factor)) +
  geom_point() +
  facet_wrap(~election_year, scales = "free_x") +
  theme_bw() +
  scale_x_reverse() +
  labs(y = "", x = "") +
  scale_color_manual(name = "", values = c("#56B4E9","#999999"))  +
  geom_smooth(method = "lm", se = F)  + 
  theme(strip.background = element_blank(), strip.text = element_blank(),
        text = element_text(size = 12), axis.text = element_text(size = 12))



plot_t_battle <- ggplot(subset(polls, t <= 365), aes(x = t, y = rep_poll2 - rep_result2, 
                                    color = bat_factor)) +
  geom_point() +
  facet_wrap(~election_year, scales = "free_x") +
  theme_bw() +
  scale_x_reverse() +
  labs(y = "Poll support - vote share", x = "") +
  scale_color_manual(name = "", values = c("#56B4E9", "#999999")) +
  geom_smooth(method = "lm", se = F)  + 
  theme(strip.background = element_blank(), strip.text = element_blank(),
        text = element_text(size = 12), axis.text = element_text(size = 12))

plot_t_end_inc <- ggplot(subset(polls, t <= 365), aes(x = t, y = rep_poll2 - rep_result2,
                                                      color = as.factor(end_inc))) +
  geom_point() +
  facet_wrap(~election_year, scales = "free_x") +
  theme_bw() +
  scale_x_reverse() +
  labs(y = "", x = "") +
  scale_color_manual(name = '', values = c("#E69F00","#56B4E9", "#009E73", 
                                           "#999999")) +
  geom_smooth(method = "lm", se = F)  + 
  theme(strip.background = element_blank(), strip.text = element_blank(),
        text = element_text(size = 12), axis.text = element_text(size = 12))

plot_t_end_battle <- ggplot(subset(polls, t <= 365), aes(x = t, y = rep_poll2 - rep_result2, 
                                                         color = as.factor(end_bat))) +
  geom_point() +
  facet_wrap(~election_year, scales = "free_x") +
  theme_bw() +
  scale_x_reverse() +
  labs(y = "", x = "") +
  scale_color_manual(name = '', values = c("#E69F00","#56B4E9", "#009E73", 
                                           "#999999")) +
  geom_smooth(method = "lm", se = F)  + 
  theme(strip.background = element_blank(), strip.text = element_blank(),
        text = element_text(size = 12), axis.text = element_text(size = 12))


plot_bias_end_t <- plot_t_end / plot_t_inc / plot_t_battle / plot_t_end_inc / 
  plot_t_end_battle +
  labs(x = "Days to election", y = "")  + 
  theme(text = element_text(size = 12), axis.text = element_text(size = 12))

ggsave(filename = 'fig4.png', 
       plot = plot_bias_end_t, 
       path = '~/results_vis/plots',
       width = 12, height = 12)

ggsave(filename = 'fig4.eps', 
       plot = plot_bias_end_t, 
       path = '~/results_vis/plots',
       width = 12, height = 12)

