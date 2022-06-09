library(tidyverse)

# read data
df <- read_csv("twitter_accounts_of_journals.csv") %>%
  select(twitter, account_description) %>%
  distinct()

# 231 (6%)
df %>%
  filter(grepl("open access|\\boa\\b", account_description, ignore.case = T))

# 595 (15.6%)
df %>%
  filter(grepl("peer.review|\\breviewed\\b|refereed", account_description, ignore.case =))

# 263 (6.8%)
df %>%
  filter(grepl("\\bJIF\\b|Impact.Factor|CiteScore|[0-9](\\.|,)[0-9][0-9]|most.cited|highly.cited", account_description, ignore.case = T))