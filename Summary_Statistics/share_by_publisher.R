library(tidyverse)

# read data
df <- read_csv("twitter_accounts_of_journals.csv")

df %>%
  select(publisher_name, journal_title, issn, has_twitter) %>%
  distinct() %>%
  group_by(publisher_name) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100,
            
            share = round(share, 1)) %>%
  select(publisher_name, total, share) %>%
  filter(total > 9) %>%
  arrange(desc(share)) 