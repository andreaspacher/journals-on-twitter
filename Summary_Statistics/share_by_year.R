library(tidyverse)

# read data
df <- read_csv("twitter_accounts_of_journals.csv")

df <- df %>%
  mutate(has_twitter = ifelse(is.na(twitter), 0, 1)) %>%
  select(journal_title, issn, twitter, account_created_date, has_twitter) %>%
  distinct()

# reduce Twitter account creation date to year
JJ <- df %>% 
  select(twitter, account_created_date) %>%
  distinct() %>%
  filter(!is.na(twitter)) %>%
  mutate(account_created_date = format(account_created_date , "%Y-%m"),
         year = substr(account_created_date , 1, 4),
  ) %>%
  group_by(year) %>%
  summarise(new_accounts = n()) %>%
  filter(!is.na(year))

JJ$total <- cumsum(JJ$new_accounts)
JJ$missing <- nrow(df) - JJ$total

JJ$share <- round(JJ$total/nrow(df)*100,1)
JJ$share.rev <- 100-JJ$share

#df %>%
#  mutate(diff = round(share.rev - lag(share.rev), 1))

JJ %>%
  filter(year < 2022) %>%
  ggplot(aes(x = year, y = missing, group = 1)) +
  geom_step() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.1)) +
  scale_x_discrete(labels = unique(JJ$year)) +
  xlab("") +
  ylab("Journals without Twitter Accounts") +
  geom_hline(aes(yintercept = nrow(df)), linetype = "dotted") +
  geom_text(aes(x = year,
                label = ifelse(year != "2007", paste0(share.rev,"%"), "")),
            size = 2.5,
            vjust = 2)

ggsave("Graph\\step_missing_journals.png",
       width = 6,
       height = 4.5,
       units = "in",
       dpi = 300)
