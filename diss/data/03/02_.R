# frontmatter ####

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(broom)
library(ggrepel)

setwd("~/Dropbox/Dissertation/")

# Author:        David Tingle
# Date:          Thu Sep 12 11:46:16 2019
# Project:       Dissertation Chapter 2
# Description:   Voeten Data Work

load("data/out/etl_results.RData")

subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  filter(shortname %in% c("YPFB", "Petrobras", "Pertamina")) %>% 
  select(shortname, Type, Status, parent_Idealpoint, subsidiary_Idealpoint) %>% 
  mutate(idealpoint_diff = abs(parent_Idealpoint - subsidiary_Idealpoint)) %>% 
  ggplot(aes(shortname, idealpoint_diff)) +
  geom_boxplot() +
  coord_flip()

df <- subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  # filter(shortname %in% c(
  #   "Eni", "Statoil", "Gazprom", "MOL","OMV","OOC","Rosneft","SOCAR"
  #   ,"CNOOC","Petronas","DONG","Sonangol","CNPC","KPC","KNOC","PTT","Saudi Aramco"
  #   ,"Petrobras","ONGC","Sonatrach"
  # )) %>%
  select(shortname, Type, Status, parent_Idealpoint, subsidiary_Idealpoint,
         country, subsidiary_country) %>% 
  mutate(idealpoint_diff = abs(parent_Idealpoint - subsidiary_Idealpoint))

subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  #filter(grepl("Eni|CNOOC|OOC", shortname)) %>% 
  select(shortname, country, subsidiary_country, parent_Idealpoint, subsidiary_Idealpoint) %>% 
  group_by(shortname) %>% 
  mutate(count_subsidiaries = n(),
         parent_Ideal = parent_Idealpoint,
         median_Ideal = median(subsidiary_Idealpoint, na.rm = T),
         diff = abs(parent_Ideal - median_Ideal)) %>% 
  #select(shortname, country, parent_Ideal, median_Ideal, diff) %>% 
  arrange(parent_Ideal) %>% 
  gather(point_type, value, -c(1:3,6:9)) %>% 
  na.omit(parent_Idealpoint) %>% 
  mutate(group = case_when(count_subsidiaries >= 50 ~ "50+ Subsidiaries",
                           count_subsidiaries >= 10 & count_subsidiaries < 50 ~ "10-49",
                           TRUE ~ "<10")) %>% 
  mutate(group_f = factor(group, levels = c("50+ Subsidiaries",
                                            "10-49",
                                            "<10"))) %>% 
  filter(!grepl("\\+", group)) %>% 
  ggplot() +
  geom_boxplot(data = . %>% filter(point_type != "parent_Idealpoint"),
               aes(y = value,
                   x = reorder(shortname, 
                               #count_subsidiaries
                               #parent_Ideal
                               -diff
                               )),
               fill = "transparent",
               outlier.shape = NA) +
  geom_jitter(data = . %>% filter(point_type != "parent_Idealpoint"),
              aes(x = shortname,
                  y = value),
              width = .1,
              height = .05,
              alpha = .3) +
  geom_point(data = . %>% filter(point_type == "parent_Idealpoint"),
             aes(y = value,
                 x = shortname),
             size = 2, color = "red") +
  coord_flip() +
  labs(x = "",
       y = "Ideal Points") +
  facet_grid(rows = vars(group_f), scales = "free", space = "free") +
  #facet_wrap(~group, ncol = 1,scales = "free_x") +
  theme_few()
  

df %>% 
  select(-idealpoint_diff) %>% 
  gather(point_type, value, 4:5) %>% 
  ggplot(x = reorder(shortname, value),  y = value) +
  geom_point(data = . %>% filter(point_type == "parent_Idealpoint"),
               aes(reorder(shortname, value), value),
             size = 5, color = "red") +
  geom_boxplot(data = . %>% filter(point_type != "parent_Idealpoint"),
               aes(shortname, value),
               fill = "transparent",
               outlier.shape = NA) +
  geom_jitter(data = . %>% filter(point_type != "parent_Idealpoint"),
              aes(shortname, value),
              width = .1,
              height = .05,
              alpha = .3) +
  labs(x = "",
       y = "Subsidiary Ideal Point") +
  coord_flip() +
  theme_few()

subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  select(shortname, parent_Idealpoint, subsidiary_Idealpoint) %>% 
  group_by(shortname) %>% 
  summarize(parent_ideal = max(parent_Idealpoint),
            mean_ideal = mean(subsidiary_Idealpoint, na.rm = T),
            median_ideal = median(subsidiary_Idealpoint, na.rm = T),
            sd_ideal = sd(subsidiary_Idealpoint, na.rm = T),
            count = n()) %>% 
  gather(metric, value, -c(1:2,count)) %>% 
  arrange(shortname) %>% 
  filter(metric == "median_ideal" | metric == "count") %>% 
  ggplot(aes(parent_ideal, value, label = shortname, size = count)) +
  geom_smooth(method = "lm", alpha = .2) +
  geom_point(alpha = .7) +
  #facet_wrap(~metric, scales = "free") +
  geom_text_repel(size = 2) +
  labs(x = "Parent Ideal Point",
       y = "Median Subsidiary Ideal Point") +
  #xlim(-2,2) +
  #ylim(-2,2) +
  geom_abline(linetype = 3) +
  theme_bw() +
  theme(legend.position = "none")

# In the large majority of cases, the mean

subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  select(shortname, parent_Idealpoint, subsidiary_Idealpoint) %>% 
  ggplot(aes(parent_Idealpoint, subsidiary_Idealpoint)) +
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm", alpha = .2)




subsidiary_dataset_FINAL %>% 
  filter(Type == "NOC" & Status == "international") %>% 
  select(shortname, parent_Idealpoint, subsidiary_Idealpoint) %>% 
  group_by(shortname) %>% 
  summarize(parent_ideal = max(parent_Idealpoint),
            mean_ideal = mean(subsidiary_Idealpoint, na.rm = T),
            median_ideal = median(subsidiary_Idealpoint, na.rm = T),
            sd_ideal = sd(subsidiary_Idealpoint, na.rm = T),
            count = n()) %>% 
  lm(mean_ideal ~ parent_ideal + count, data = .) %>% 
  summary()
   


