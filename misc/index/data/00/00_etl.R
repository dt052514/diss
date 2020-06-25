

## Setup ####

library(tidyverse)
library(readxl)
library(rqog)
library(ggmap)

register_google(key = "AIzaSyBg1yiESdRa_1jNXhkntfg63i3hXmlxvEE")
setwd("/Users/davidtingle/Dropbox/Dissertation/data")

# NOC & IOC Subsidiary-Level Dataset ####

noc <- read_excel("raw/original_data/capIQ_CorporateTree.xlsx") %>%
  select(firm, subsidiary, relationship, stake, industry, headquarters) %>%
  separate(headquarters, c("sub_city","subsidiary_country"), sep = " \\| ") %>%
  mutate(subsidiary_country =
           ifelse(is.na(subsidiary_country), sub_city, subsidiary_country)) %>%
  select(-sub_city) %>%
  left_join(., read_excel("raw/original_data/capIQ_firms_FINAL.xlsx")) %>%
  mutate(subsidiary_country = case_when(subsidiary == "Cnpc Venezuela, B.v." ~ "Venezuela",
                             subsidiary == "BP Colombia Pipelines Limited" ~ "Colombia",
                             subsidiary == "Agip Algeria Exploration B V" ~ "Algeria",
                             subsidiary == "Agip Oil Ecuador B.V." ~ "Ecuador",
                             subsidiary == "Agip Qatar B.v." ~ "Qatar",
                             subsidiary == "Eni Yemen Limited" ~ "Yemen",
                             subsidiary == "Eni Algeria Production B.V." ~ "Algeria",
                             subsidiary == "Eni Angola Exploration B.V." ~ "Angola",
                             subsidiary == "Eni Angola Production B.V." ~ "Angola",
                             subsidiary == "Eni Australia B.V." ~ "Australia",
                             subsidiary == "Eni Australia B.V. (Australia Branch)" ~ "Australia",
                             subsidiary == "Eni Australia Limited" ~ "Australia",
                             subsidiary == "Eni China B.V" ~ "China",
                             subsidiary == "Eni Gas & Power Lng Australia B.V." ~ "Australia",
                             subsidiary == "Eni Croatia B.V." ~ "Croatia",
                             subsidiary == "ENI Energy Russia B.V." ~ "Russia",
                             subsidiary == "Eni Iran B.V." ~ "Iran",
                             subsidiary == "Eni Iraq B.V." ~ "Iraq",
                             subsidiary == "Eni Ireland B.V." ~ "Ireland",
                             subsidiary == "Eni Oil Algeria Ltd." ~ "Algeria",
                             subsidiary == "Eni Tunisia Bek B.V." ~ "Tunisia",
                             subsidiary == "Eni Pakistan (M) Limited" ~ "Pakistan",
                             subsidiary == "Eni Pakistan Ltd" ~ "Pakistan",
                             subsidiary == "Eni Tunisia B.V." ~ "Tunisia",
                             subsidiary == "Eni Ivory Coast Limited" ~ "Cote d'Ivoire",
                             subsidiary == "Eni Ukraine Holdings BV" ~ "Ukraine",
                             subsidiary == "Eni Venezuela B.V." ~ "Venezuela",
                             subsidiary == "Eni Vietnam B.V." ~ "Vietnam",
                             subsidiary == "Galp Energia Portugal Holding, B.v.," ~ "Portugal",
                             subsidiary == "Trans Tunisian Pipeline Co Ltd" ~ "Tunisia",
                             subsidiary == "Galp Energia Portugal Holding, B.v.," ~ "Portugal",
                             subsidiary == "Dana Petroleum (Russia) Limited" ~ "Russia",
                             subsidiary == "Kufpec Indonesia (Natuna) B.V." ~ "Indonesia",
                             subsidiary == "Videocon Mozambique Rovuma 1 Limited" ~ "Mozambique",
                             subsidiary == "China Oman Energy Co., Ltd." ~ "China",
                             subsidiary == "Petrobras Venezuela Investments & Services B.V." ~ "Venezuela",
                             subsidiary == "Polish Oil & Gas Company Libya B.V." ~ "Libya",
                             subsidiary == "PTTEP Kim Long Vietnam Co., Ltd." ~ "Vietnam",
                             subsidiary == "Gazprom Neft Cuba B.V." ~ "Cuba",
                             subsidiary == "QPI Vietnam Limited" ~ "Vietnam",
                             subsidiary == "IPM Indonesia BV" ~ "Indonesia",
                             subsidiary == "Aramco Japan Holdings Company BV" ~ "Japan",
                             subsidiary == "Lundin Tunisia B.V" ~ "Tunisia",
                             subsidiary == "Statoil Banarli Turkey B.V." ~ "Turkey",
                             subsidiary == "Statoil Uruguay B.V." ~ "Uruguay",
                             subsidiary == "Rosneft Vietnam B.V." ~ "Vietnam",
                             subsidiary == "Dragon Oil (Turkmenistan) Ltd." ~ "Turkmenistan",
                             subsidiary == "Videocon Mozambique Rovuma 1 Limited" ~ "Mozambique",
                             subsidiary == "Videocon Mozambique Rovuma 1 Limited" ~ "Mozambique",
                             subsidiary == "COSL Mexico S.A. de C.V" ~ "Mexico",
                             subsidiary == "PetroChina International Iraq FZE" ~ "Iraq",
                             subsidiary == "PetroKazakhstan Inc." ~ "Kazakhstan",
                             subsidiary == "Eni Pakistan (M) Limited Sarl" ~ "Pakistan",
                             subsidiary == "PC Mauritania I Pty Ltd." ~ "Mauritania",
                             subsidiary == "PETRONAS Sierra Leone E&P Ltd." ~ "Sierra Leone",
                             subsidiary == "Cove Energy Mozambique Rovuma Offshore Limited" ~ "Mozambique",
                             subsidiary == "Cove Energy Tanzania Mnazi Bay Limited,cyprus" ~ "Tanzania",
                             subsidiary == "Neste Estonia" ~ "Estonia",
                             TRUE ~ subsidiary_country)) %>% 
  mutate(shortname = case_when(shortname == "QPI" ~ "Qatar Petroleum",
                               shortname == "KPI" ~ "KPC",
                               TRUE ~ shortname)) %>% 
  bind_rows(., read_delim("raw/supp_subsidiaries.csv", delim = "\t")) %>% 
  mutate(Type = "NOC")

# Read in hand-collected subsidiaries (Gazprom International, )

ioc <- read_excel("raw/original_data/FTSE_CorporateTree.xlsx") %>%
  select(firm, subsidiary, relationship, stake, headquarters) %>%
  separate(headquarters, c("sub_city","subsidiary_country"), sep = " \\| ") %>%
  mutate(subsidiary_country =
           ifelse(is.na(subsidiary_country), sub_city, subsidiary_country)) %>%
  select(-sub_city) %>%
  left_join(., read_excel("raw/original_data/FTSE_final.xlsx")) %>%
  mutate(Type = "IOC") %>%
  select(-ticker, -market.cap, -revenue, -close)

subsidiary_dataset <- bind_rows(noc, ioc) %>% 
  mutate(shortname = ifelse(is.na(shortname), firm, shortname))

rm(noc, ioc)

# Joining with Other Datasets & Gathering Geographic Information ####
qog <- read_qog(which_data = "standard", data_type = "cross-sectional") %>%
  #filter(cname == "Afghanistan") %>% 
  select(
  cname,
  ccode,
  ccodecow,
  ccodealp,
  bti_gi, # BTI Governance Index
  bti_gp, # BTI Governance Performance
  bti_ep, # Economic Performance
  dr_eg, # Economic Globalization
  ffp_fsi, # Fragile States Index
  fh_polity2, # Freedom House Polity Level of Democracy
  icrg_qog, # ICRG Indicator of Quality of Government
  #gcb_putil, # Corruption Perception - Utilities
  gcb_pb, # Corruption Perception - Business
  #gle_pop, # Population (1000s)
  #scip_spop1564, # Population
  wdi_pop, # Population Total
  undp_hdi, # Human Development Index
  ross_oil_exp, # Oil Exports - 1000 barrels / day
  ross_oil_prod, # Oil Production - metric tons
  wdi_oilrent, # Oil rents (% of GDP)
  wdi_eneimp, # Oil imports, net
  wdi_fdiin, # FDI inflows
  wdi_fdiout, #FDI outflows
  wdi_gini, # GINI coefficient
  #gle_gdp, # real GDP (2005)
  #gle_rgdpc, # real GDP per Capita 2005
  #gle_trade, # Total Trade
  wdi_gdpcapcon2010 # GDP per capita, constant 201
) %>% as_tibble()

# qog <- read_qog(which_data = "basic", data_type = "cross-sectional") %>%
#   select(cname, ccode, ccodecow, ccodealp, icrg_qog, p_polity2,
#          wdi_gdpcapcon2010,
#          bti_ds, ffp_fsi, p_polity2, gle_gdp, gle_pop, gle_rgdpc,
#          ross_oil_exp, ross_oil_prod, wdi_oilrent, wbgi_gee, undp_hdi)

# rename(`BTI Democracy` = bti_ds,
#        `Fragile State Index` = ffp_fsi,
#        `Polity2` = fh_polity2,
#        `Real GDP` = gle_gdp,
#        `Population` = gle_pop,
#        `Real GDP per capita` = gle_rgdpc,
#        `ICRG QoG` = icrg_qog,
#        `Oil Exports` = ross_oil_exp,
#        `Oil Production` = ross_oil_prod,
#        `Oil Rent` = wdi_oilrent,
#        `WB Gov't Effectiveness` = wbgi_gee,
#        `UNDP HDI` = undp_hdi)

state_lookup <- subsidiary_dataset %>%
  select(country) %>%
  distinct() %>%
  full_join(., subsidiary_dataset %>%
              select(subsidiary_country) %>%
              distinct() %>%
              rename(country = subsidiary_country)) %>%
  mutate(country_new = case_when(grepl("Bosnia", country) ~ "Bosnia and Herzegovina",
                             grepl("TimorLeste", country) ~ "Timor-Leste",
                             grepl("Antigua & Barbuda", country) ~ "Antigua and Barbuda",
                             grepl("Cyprus", country) ~ "Cyprus (1975-)",
                             grepl("Ethiopia", country) ~ "Ethiopia (1993-)",
                             grepl("France", country) ~ "France (1963-)",
                             grepl("GuineaBissau", country) ~ "Guinea-Bissau",
                             grepl("Ivory Coast", country) ~ "Cote d'Ivoire",
                             grepl("Malaysia", country) ~ "Malaysia (1966-)",
                             grepl("Pakistan", country) ~ "Pakistan (1971-)",
                             grepl("Republic of the Congo", country) ~ "Congo, Democratic Republic",
                             grepl("Saint Kitts & Nevis", country) ~ "St Kitts and Nevis",
                             grepl("South Korea", country) ~ "Korea, South",
                             grepl("Sudan", country) ~ "Sudan (2012-)",
                             grepl("Trinidad & Tobago", country) ~ "Trinidad and Tobago",
                             grepl("Aruba|Cura|Antilles", country) ~ "Netherlands",
                             grepl("Virgin Is|Cayman|Channel|Gibraltar|Bermuda|of Man|Anguilla",
                                   country) ~ "United Kingdom",
                             grepl("Martinique|Guadeloupe|Reunion|New Caledonia",
                                   country) ~ "France (1963-)",
                             grepl("Hong Kong|Macau", country) ~ "China",
                             TRUE ~ country)) %>%
  distinct() %>%
  left_join(., qog %>% rename(country_new = cname)) %>%
  arrange(country) %>%
  #na.omit() %>%
  left_join(., read_tsv("raw/cap_city") %>%
              select(ccodealp, capital)) %>%
  mutate(location = paste(capital, country_new, sep = " "))

geocodes <- geocode(state_lookup$location)

state_lookup <- cbind(state_lookup, geocodes)
state_lookup <- state_lookup %>%  as_tibble()

library(countrycode)
cc <- as.data.frame(countrycode::codelist) %>%
  select(iso3n, continent, region) %>% 
  as_tibble() %>% 
  na.omit()

# Fixing Country Names in the Main Dataset, and importing region ####

# state_lookup %>%
#   select(country, country_new, ccode, ccodealp, ccodecow, lat, lon) %>%
#   rename_all(function(x) paste0("subsidiary_", x))

subsidiary_dataset_FINAL <- subsidiary_dataset %>%
  left_join(., state_lookup) %>%
  mutate(country = if_else(country == country_new, country, country_new)) %>%
  left_join(., state_lookup %>%
              rename_all(function(x) paste0("subsidiary_", x))) %>%
  mutate(subsidiary_country = if_else(subsidiary_country == subsidiary_country_new,
                                      subsidiary_country, subsidiary_country_new)) %>%
  select(-country_new, -subsidiary_country_new) %>%
  mutate(Status = ifelse(country == subsidiary_country, 
                         "domestic", "international")) %>%
  left_join(., cc, by = c("ccode" = "iso3n")) %>%
  left_join(., cc %>%
              rename(subsidiary_continent = continent,
                     subsidiary_region = region), 
            by = c("subsidiary_ccode" = "iso3n")) %>% 
  left_join(.,
            read_delim("raw/IdealpointsPublished.tab", delim = "\t") %>% 
              select(year, ccode, Idealpoint) %>% 
              filter(year %in% 2000:2013) %>% 
              group_by(ccode) %>% 
              summarize(parent_Idealpoint = mean(Idealpoint, na.rm = T)),
            by = c("ccodecow" = "ccode")) %>% 
  left_join(.,
            read_delim("raw/IdealpointsPublished.tab", delim = "\t") %>% 
              select(year, ccode,Idealpoint) %>% 
              filter(year %in% 2000:2013) %>% 
              group_by(ccode) %>% 
              summarize(subsidiary_Idealpoint = mean(Idealpoint, na.rm = T)),
            by = c("subsidiary_ccodecow" = "ccode")) %>% 
  rename(lat.y = subsidiary_lat,
         lon.y = subsidiary_lon) 
  
rm(geocodes, qog, subsidiary_dataset,test, ioc, noc, cc, df)

# Recode Subsidiaries that may be offshored ####
# (not useful after the first time, kept for posterity)

# cc <- as.data.frame(countrycode::codelist) %>%
#   select(cow.name) %>% 
#   as_tibble() %>% 
#   na.omit()
# 
# subsidiary_dataset_FINAL %>%
#   filter(Type == "NOC" & Status == "international") %>% 
#   filter(grepl("Nether|United Kingdom|Panama", subsidiary_country)) %>% 
#   select(shortname, subsidiary, subsidiary_country) %>%
#   mutate(alt_sub = case_when(grepl(paste(cc$cow.name,collapse="|"), subsidiary) ~ "ALTERNATIVE",
#                              TRUE ~ "Original")) %>%
#   filter(alt_sub == "ALTERNATIVE") %>% 
#   select(-alt_sub)
# 
# df <- subsidiary_dataset_FINAL %>%
#   filter(Type == "NOC" & Status == "international") %>% 
#   filter(!grepl("Nether|United Kingdom|Panama", subsidiary_country)) %>% 
#   select(shortname, subsidiary, subsidiary_country) %>%
#   mutate(alt_sub = case_when(grepl(paste(cc$cow.name,collapse="|"), subsidiary) ~ "ALTERNATIVE",
#                              TRUE ~ "Original")) %>%
#   filter(alt_sub == "ALTERNATIVE") %>% 
#   select(-alt_sub)

# Resolving the State Lookup ####

state_lookup <- state_lookup %>% 
  left_join(.,
            read_delim("raw/IdealpointsPublished.tab", delim = "\t") %>% 
              select(year, ccode, Idealpoint) %>% 
              filter(year %in% 2000:2013) %>% 
              group_by(ccode) %>% 
              summarize(parent_Idealpoint = mean(Idealpoint, na.rm = T)),
            by = c("ccodecow" = "ccode"))

# Final Save ####

#saveRDS(subsidiary_dataset_FINAL, file = "out/subsidiary_dataset_FINAL.rds")
saveRDS(subsidiary_dataset_FINAL, file = "out/subsidiary_dataset_FINAL_full_dataset.rds")
saveRDS(state_lookup, file = "out/state_lookup.rds")
save.image("~/Dropbox/Dissertation/data/out/etl_results.RData")

