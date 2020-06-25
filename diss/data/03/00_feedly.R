# frontmatter ####

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(seymour)
library(tsibble)

setwd("~/Dropbox/Dissertation/data/03/")

# Author:        David Tingle
# Date:          Wed Sep  4 20:10:23 2019
# Project:       Dissertation Paper 3
# Description:   Feedly Data Download

# Feedly Developer User ID: efcc4b19-61d0-4833-a711-1ba4c23798b9

# Token: Ax5dpttRo6pGJPxInHSmEKLi0JFlxlEd23eAbIekSFxvPHKcDO382h4zgLD9yTXT0qMrrvv78fFNklwwb0OrhIYwmprsmSreMM9mXAXKXRWCyZ-O_U38dLnU2ZaPRUzu4n-dxArcwlwY-7mxi3ULrhE74Kvalvqgm5SyIbb9KldfSAgam7tqcdHK5wMa_sLQD4Tsb8XhK4pXY4bnHmJZF-YitQrnwqkRPuSqS2E3Cu4ARwpvLfJ_5-6SzQzn:feedlydev

token <- feedly_access_token()

coll <- feedly_collections(feedly_token = token)

coll %>% 
  select(title, label, feedId) %>% 
  filter(label == "SOEs") %>% 
  select(feedId)

noc_feed <- "feed/https://www.google.com/alerts/feeds/01147398548815012848/3178637797437048761"
ogj_feed <- "feed/http://feeds.feedburner.com/latest-news-ogj"
rigzone_feed <- "feed/http://www.rigzone.com/news/rss/rigzone_finance.aspx"


noc_stream <- feedly_stream(noc_feed, count = 1000L) %>%
  feedly_continue(., 0)

ogj_stream <- feedly_stream(ogj_feed, count = 1000L) %>%
  feedly_continue(., 0)

rigzone_stream <- feedly_stream(rigzone_feed, count = 1000L) %>%
  feedly_continue(., 0)

rm(noc_feed, ogj_feed, rigzone_feed)

save.image("~/Dropbox/Dissertation/data/03/out/feeds.RData")