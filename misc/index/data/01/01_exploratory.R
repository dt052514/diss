# Date: Aug 8, 2019
# Description: Chapter 1 Descriptive Stats

# Prelims ####

library(tidyverse)
library(readxl)


setwd("/Users/davidtingle/Dropbox/_Diss")

theme_set(theme_classic() +
            theme(legend.position="bottom"))

# Data Load ####

df <- read_excel("data/raw/ctf_final.xlsx") %>%
  mutate(Type = ifelse(noc_status == 1, "NOC", "IOC")) %>%
  select(-noc_status)

# Number of Parents ####

df %>%
  mutate(dom_subsidiary = total_subsidiary - int_subsidiary) %>%
  select(shortname, Type, int_subsidiary, dom_subsidiary) %>%
  distinct() %>%
  group_by(Type) %>%
  summarize(n = n(),
            mean_int = median(int_subsidiary),
            mean_dom = median(dom_subsidiary))

n2 <- df %>%
  filter(Type == "NOC") %>%
  select(firm) %>%
  distinct() %>%
  arrange(firm)


# Subsidiaries, Domestic & International - All ####

read_excel("data/raw/ctf_final.xlsx") %>%
  select(shortname, noc_status, int_subsidiary, total_subsidiary) %>%
  mutate(domestic = total_subsidiary - int_subsidiary) %>%
  mutate(Type = ifelse(noc_status == 1, "NOC", "IOC")) %>%
  group_by(shortname, Type) %>%
  summarise(international = max(int_subsidiary),
            domestic = max(domestic)) %>%
  ggplot(aes(domestic, international, color = Type, shape = Type)) +
  geom_point() +
  scale_color_manual(values=c("black", "red", "blue")) +
  labs(x = "Number of Domestic Subsidiaries",
       y = "Number of International Subsidiaries") +
  ggtitle("Subsidiaries, Domestic & International (Logged)") +
  #scale_x_log10() +
  #scale_y_log10() +
  facet_wrap(~Type)
