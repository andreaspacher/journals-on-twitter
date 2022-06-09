library(tidyverse)

# read data
df <- read_csv("twitter_accounts_of_journals.csv")

DISTINCT <- df %>%
  select(journal_title, issn, twitter) %>%
  distinct() %>%
  nrow()

# create column with Web of Science index
df %>%
  pivot_longer(cols = c("ahci", "ssci", "scie"),
                   names_to = "wos_index") %>%
  filter(value == 1) %>%
  select(-value, journal_title, issn, has_twitter) %>%
  distinct() %>%
# generate summary statistics
  group_by(wos_index) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = n(),
            share = with / total * 100
  )

# summary statistics for all journals
df %>%
  select(journal_title, issn, has_twitter) %>%
  distinct() %>%
  summarise(with = sum(has_twitter),
          without = n() - with,
          total = n(),
          share = with / total * 100
)

