# frontmatter ####

library(tidyverse)
library(ggthemes)

# Author:        David Tingle
# Date:          Sat Aug 10 17:46:55 2019
# Project:       Dissertation Chapter 1 - Exploratory Analysis
# Description:   First Analysis

setwd("/Users/davidtingle/Dropbox/_Diss/data")
load("/Users/davidtingle/Dropbox/_Diss/data/out/etl_results.RData")

theme_set(theme_few() +
            theme(legend.position="bottom",
                  legend.title = element_blank()))

# Data Load ####

df <- subsidiary_dataset_FINAL %>% 
  as_tibble() %>% 
  select(shortname, country, subsidiary_country, Type, Status, stake, industry)

# Basic summary statistics ####

# How many firms?
df %>%
  filter(Type == "NOC") %>%
  select(shortname) %>%  
  distinct()

# Across How many countries?
df %>%
  filter(Type == "NOC") %>%
  select(country) %>%  
  distinct() %>% arrange(country)

# How many NOCs have at least one international subsidiary?
df %>%
  filter(Type == "NOC") %>%
  filter(Status == "international") %>% 
  select(shortname) %>%  
  distinct()

# Comparing summary statistics for NOCs and IOCs ####
df %>%
  filter(Status == "international") %>% 
  group_by(shortname, Type) %>%
  summarize(n = n()) %>% 
  group_by(Type) %>% 
  summarize(mean = mean(n, na.rm = T),
            median = median(n, na.rm = T),
            total = sum(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T))

# Histogram of Internationalization, by firm type ####
df %>%
  filter(Status == "international") %>% 
  group_by(shortname, Type) %>%
  summarize(n = n()) %>% 
  ggplot(aes(n, group = Type, fill = Type)) +
  geom_histogram(position = "identity", bins = 15) +
  facet_wrap(~Type) +
  labs(x = "# of International Subsidiaries",
       y = "# of Firms") +
  scale_x_log10() +
  ylim(0, 15) +
  theme(legend.position="none",
        legend.title = element_blank())
  
# proportion of subsidiaries, by firm type ####
df %>%
  group_by(Type, Status) %>% 
  summarize(total = n()) %>%
  mutate(Status = ifelse(is.na(Status), "unknown", Status)) %>% 
  spread(Status, total) %>% 
  mutate(total = domestic + international + unknown,
         pct_international = international / total,
         pct_unknown = unknown / total)


