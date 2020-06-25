# frontmatter ####

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(broom)
library(readxl)

setwd("~/Dropbox/Dissertation/")

# Author:        David Tingle
# Date:          Thu Aug 29 08:46:12 2019
# Project:       Dissertation Chapter 2
# Description:   Modeling Work

# To Do List ####

# Import data
# Standardize variables
# Build basic OLS models (3)
# Alternative Specifications
# Visuals
# Tables
# 

# Data Import ####

load("data/out/etl_results.RData")

base <- subsidiary_dataset_FINAL %>% 
  count(shortname, ccode, ccodecow, Status, Type) %>%
  filter(!is.na(Status)) %>% 
  spread(Status, n) %>%
  mutate(domestic = ifelse(is.na(domestic), 1, domestic)) %>% 
  mutate(international = ifelse(is.na(international), 0, international)) %>% 
  filter(Type == "NOC") %>%
  group_by(ccode, ccodecow) %>%
  summarize(domestic = mean(domestic),
            international = mean(international)) %>%
  left_join(., state_lookup %>% select(-country)) %>%
  distinct() %>%
  rename(country = country_new) %>% 
  select(country, ccode, ccodecow, international, domestic, fh_polity2)

df <- subsidiary_dataset_FINAL %>% 
  count(shortname, ccode, ccodecow, Status, Type) %>%
  filter(!is.na(Status)) %>% 
  spread(Status, n) %>%
  mutate(domestic = ifelse(is.na(domestic), 1, domestic)) %>% 
  mutate(international = ifelse(is.na(international), 0, international)) %>% 
  filter(Type == "NOC") %>%
  group_by(ccode, ccodecow) %>%
  summarize(domestic = mean(domestic),
            international = mean(international)) %>%
  left_join(., state_lookup %>% select(-country)) %>%
  distinct() %>%
  rename(country = country_new) %>% 
  select(country, ccode, ccodecow, international, domestic,
         dr_eg, bti_ep, bti_gp, bti_gi, icrg_qog, fh_polity2, 
         wdi_oilrent, ross_oil_prod) %>%
  left_join(.,
            read_csv("data/raw/EIA_International_data.csv", na = c(NA, "--")) %>%
              gather(Year, oil, 2:12) %>%
              mutate(reserves = as.numeric(oil)) %>%
              group_by(country) %>% 
              summarize(reserves = mean(reserves, na.rm = T)) %>%
              na.omit() %>%
              mutate(country = case_when(country == "Sudan" ~ "Sudan (2012-)",
                                         country == "Pakistan" ~ "Pakistan (1971-)",
                                         country == "Malaysia" ~ "Malaysia (1966-)",
                                         country == "Cote dIvoire (IvoryCoast)" ~ "Cote d'Ivoire",
                                         country == "Congo (Kinshasa)" ~ "Congo, Democratic Republic",
                                         TRUE ~ country)) %>%
              right_join(., base %>% select(country, ccode)) %>%
              left_join(., read_csv("data/raw/International_data_2000.csv", na = c(NA, "--")) %>%
                          mutate(reserves_2000 = as.numeric(reserves_2000))) %>%
              mutate(reserves = ifelse(is.na(reserves), reserves_2000, reserves))) %>% 
  left_join(., read_excel("data/raw/noc_var.xlsx") %>% 
              select(ccode, control) %>% 
              na.omit() %>%
              rename(ccodecow = ccode)) %>%
  distinct() %>%
  mutate(control = ifelse(is.na(control), 2.5, control))

rm(base)

# Ok, based on this we have a working firm level dataset with variables of interest
# The problem is that all of our predictor variables will be state level, not firm
# An interim solution may be to average the number of international subsidiaries by country (done)

# Defining Variables for Modeling ####

# Outcome Variable: international subsidiaries (international)

# Controls: 
  # Commercial Viability of the Firm 
      # bti_ep   Economic Performance
      # dr_eg    Economic Globalization
  # Governance State Control    
      # bti_gi   BTI Governance Index
      # bit_gp   BTI Governance Performance
      # icrg_qog ICRG Indicator of Quality of Government
      # wbgi_gee Governance Effectiveness Estimate
  # Democratic Status 
      # fh_polity2
  # Resource Endowment    
      # reserves
      # reserves_2000
      # wdi_oilrent
      # ross_oil_prod

tidy(summary(lm(international ~ 
             domestic + 
             control + 
             fh_polity2 + 
             reserves, 
           data = df)))

tidy(summary(lm(international ~ reserves, data = df)))

# in the regression above, given my causal model (dagitty.net/mBQp_cy) the coefficient for domestic can be interpreted as the total effect, and the coefficient on control can be thought of as the direct effect. Netiher polity nor reserves can be interpreted as causal effects.

summary(lm(international ~ reserves, data = df))
summary(lm(international ~ fh_polity2 + reserves, data = df))

# Scaling Variables, then running regressions ####

df$i_scaled <- scale(df$international)
df$d_scaled <- scale(df$domestic)
df$c_scaled <- scale(df$control)
df$p_scaled <- scale(df$fh_polity2)
df$r_scaled <- scale(df$reserves)

# regressions on scaled variables ####

tidy(summary(lm(i_scaled ~ 
             d_scaled + 
             c_scaled + 
             p_scaled + 
             r_scaled, 
           data = df)))

sd(df$international)
sd(df$domestic)

summary(lm(i_scaled ~ r_scaled, data = df))
summary(lm(i_scaled ~ p_scaled + r_scaled, data = df))


# Coefficient on scaled "capacity" : 0.46
# Coefficient on scaled control : -0.058
# Scaling: The result is that the values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
# Interpretation: The effect of scaled capacity (d_scaled) now represents the expected change in standard deviations of internationalization for a 1-standard-deviation increase in capacity; that is, for a 1-SD increase in capacity, we expect a 0.46-SD decrease in internationalization, which we could call an effect of moderate magnitude.
# Substantive effects: an increase of 70 domestic subsidiaries should lead to an increase of 54 international subsidiaries, all else equal. 

# Plots ####

library(dotwhisker)

tidy(lm(i_scaled ~ 
          d_scaled + 
          c_scaled + 
          p_scaled + 
          r_scaled, 
        data = df)) %>%
  relabel_predictors(c(d_scaled = "Firm Capacity",
                       c_scaled = "State Control",
                       p_scaled = "Polity2 Score",
                       r_scaled = "Oil Reserves")) %>% 
  dwplot() +
  geom_vline(xintercept = 0, color = "grey30", linetype = 3) +
  xlim(-1.5,1.5) +
  labs(#title = "Modeling Internationalization (Scaled Regressors)",
       x = "Coefficient Estimate, Point & 95% Interval") +
  scale_color_grey() +
  theme_bw() +
  theme(legend.position = "none")

# Importing proven oil reserves data ####

read_csv("data/raw/EIA_International_data.csv", na = c(NA, "--")) %>%
  gather(Year, oil, 2:12) %>%
  mutate(reserves = as.numeric(oil)) %>%
  group_by(country) %>% 
  summarize(reserves = mean(reserves, na.rm = T)) %>%
  na.omit() %>%
  mutate(country = case_when(country == "Sudan" ~ "Sudan (2012-)",
                             country == "Pakistan" ~ "Pakistan (1971-)",
                             country == "Malaysia" ~ "Malaysia (1966-)",
                             country == "Cote dIvoire (IvoryCoast)" ~ "Cote d'Ivoire",
                             country == "Congo (Kinshasa)" ~ "Congo, Democratic Republic",
                             TRUE ~ country)) %>%
  right_join(., df %>% select(country, ccode)) %>%
  left_join(., read_csv("data/raw/International_data_2000.csv", na = c(NA, "--")) %>%
              mutate(reserves_2000 = as.numeric(reserves_2000))) %>%
  mutate(reserves = ifelse(is.na(reserves), reserves_2000, reserves))

# Importing Mahdavi Data ####

read_excel("data/raw/noc_var.xlsx") %>% 
  select(ccode, control) %>% 
  na.omit() %>%
  rename(ccodecow = ccode)

# R for Data Viz Approach ####



df %>%
  select(international, domestic, control, fh_polity2, reserves,
         i_scaled, d_scaled, c_scaled, p_scaled, r_scaled) %>%
  mutate(linternational = log2(international),
         ldomestic = log2(domestic))
  






df1 <- df %>%
  ungroup() %>% 
  select(i_scaled, d_scaled, c_scaled, p_scaled, r_scaled)


df1 %>% 
  ggplot(aes(c_scaled, i_scaled)) +
  geom_point()

df %>%
  ggplot(aes(log10(domestic), log10(international))) + 
  geom_point()


df %>%
  ggplot(aes(control, international)) + geom_point()


df %>%
  #ggplot(aes(domestic, international)) +
  ggplot(aes(log2(domestic), log2(international))) +
  geom_point()

summary(lm(log2(international) ~ log2(domestic), data = df))

df1 %>%
  ggplot(aes(log2(d_scaled), log2(i_scaled))) +
  geom_point()

mod1 <- lm(i_scaled ~ d_scaled, data = df1)
mod2 <- lm(i_scaled ~ d_scaled + c_scaled, data = df1)
mod3 <- lm(i_scaled ~ d_scaled + c_scaled + p_scaled + r_scaled, data = df1)
mod4 <- lm(i_scaled ~ d_scaled * c_scaled, data = df1)
mod5 <- lm(i_scaled ~ d_scaled * c_scaled + p_scaled + r_scaled, data = df1)

df1 %>%
  add_residuals(mod1) %>% 
  ggplot(aes(d_scaled, resid)) + 
  geom_ref_line(h = 0) +
  geom_point()


# Testing Robust Linear Regression ####

tidy(MASS::rlm(i_scaled ~ 
          d_scaled + 
          c_scaled + 
          p_scaled + 
          r_scaled, 
        data = df)) %>%
  relabel_predictors(c(d_scaled = "Firm Capacity",
                       c_scaled = "State Control",
                       p_scaled = "Polity2 Score",
                       r_scaled = "Oil Reserves")) %>% 
  dwplot() +
  geom_vline(xintercept = 0, color = "grey30", linetype = 3) +
  #xlim(-1,1) +
  labs(#title = "Modeling Internationalization (Scaled Regressors)",
    x = "Coefficient Estimate, Point & 95% Interval") +
  scale_color_grey() +
  theme_bw() +
  theme(legend.position = "none")























