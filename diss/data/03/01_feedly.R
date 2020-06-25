# frontmatter ####

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
#library(ggthemes)
library(tsibble)

setwd("~/Dropbox/Dissertation/")

# Author:        David Tingle
# Date:          Wed Sep  4 21:02:45 2019
# Project:       Dissertation Paper 3
# Description:   Feedly Data Analysis (Preliminary)


# Load ####

load("data/03/out/feeds.RData")


# Structure DF ####

df <- noc_stream %>% 
  select(title, updated, canonicalurl, engagement, content_content) %>% 
  mutate(year = year(updated),
         date = date(updated),
         month = month(updated, label = T))
         #week = yearweek(updated))


temp <- df %>% 
  filter(grepl("trade", title,ignore.case = T)) %>% 
  View()

# Coding Firms ####

# Coding Internationalization ####









df <- read_csv("out/feedly_df.csv") %>% 
  select(-X1) %>% 
  mutate(date = date(updated),
         week = yearweek(date)) %>% 
  filter(date < "2019-09-01")

df %>% 
  count(week) %>% 
  ggplot(aes(week, n)) +
  geom_line()

titles <- select(df, title, engagement)

df %>% 
  select(title, canonicalurl, week) %>% 
  mutate(firm = case_when(grepl("Aramco", title,ignore.case = T) ~ "Aramco",
                          grepl("Adnoc", title,ignore.case = T) ~ "ADNOC",
                          grepl("Oman", title,ignore.case = T) ~ "OOC",
                          grepl("Kuwait|KPC", title,ignore.case = T) ~ "KPC",
                          grepl("Qatar|QP", title,ignore.case = T) ~ "QP",
                     TRUE ~ "other")) %>% 
  filter(firm != "other") %>% 
  count(week, firm) %>% 
  ggplot(aes(week, n)) +
  geom_line() +
  facet_wrap(~firm)

         