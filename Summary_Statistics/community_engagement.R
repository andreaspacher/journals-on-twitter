library(tidyverse)

# read data
DFF <- arrow::read_parquet("Summary_Statistics\\twitter_data.parquet")

DF <- DFF %>%
  filter(year == 2020 | year == 2021)
library(plyr)
ALLNAMES <- plyr::ddply(DF, .(twitter), summarize, allnames = paste(uniquenames, collapse=";")) %>%
  separate_rows(allnames) %>%
  distinct() %>%
  select(twitter) %>%
  group_by(twitter) %>%
  count()
colnames(ALLNAMES) <- c("twitter", "total_names")
ALLREP <- plyr::ddply(DF, .(twitter), summarize, allnames_rep = paste(uniquenames_rep, collapse=";")) %>%
  separate_rows(allnames_rep) %>%
  distinct() %>%
  select(twitter) %>%
  group_by(twitter) %>%
  count()
colnames(ALLREP) <- c("twitter", "total_names_rep")
ALLRT <- plyr::ddply(DF, .(twitter), summarize, allnames_rt = paste(uniquenames_rt, collapse=";")) %>%
  separate_rows(allnames_rt) %>%
  distinct() %>%
  select(twitter) %>%
  group_by(twitter) %>%
  count()
colnames(ALLRT) <- c("twitter", "total_names_rt")
ALLMEN <- plyr::ddply(DF, .(twitter), summarize, allnames_mentions = paste(uniquenames_mentions, collapse=";")) %>%
  separate_rows(allnames_mentions) %>%
  distinct() %>%
  select(twitter) %>%
  group_by(twitter) %>%
  count()
colnames(ALLMEN) <- c("twitter", "total_names_mentions")
detach(package:plyr)
DF$tweets_total <- as.numeric(DF$tweets_total)
DF <- DF %>%
  select(-account_created_date, -year, -starts_with("uniquenames")) %>%
  group_by(twitter) %>%
  mutate(totaltw = sum(tweets_total),
         total_mentions = sum(unique_mentions),
         total_rt = sum(unique_rt),
         total_rep = sum(unique_rep)
  ) %>%
  #filter(totaltw >= 50) %>%
  select(-tweets_total, -unique_mentions, -unique_rt, -unique_rep) %>%
  distinct() %>%
  left_join(ALLNAMES) %>%
  left_join(ALLMEN) %>%
  left_join(ALLREP) %>%
  left_join(ALLRT) %>%
  group_by(twitter) %>%
  mutate(ratio = round(total_names / totaltw, 2),
         ratio_mentions = round(total_names_mentions / totaltw, 2),
         ratio_rep = round(total_names_rep / totaltw, 2),
         ratio_rt = round(total_names_rt / totaltw, 2)
  )
rm(ALLNAMES, ALLMEN, ALLREP, ALLRT)

JJ <- read_csv("twitter_accounts_of_journals.csv")
JJ <- JJ %>%
  pivot_longer(cols = c("ahci", "ssci", "scie"),
               names_to = "wos_index") %>%
  filter(value == 1) %>%
  select(-value)
TT <- left_join(JJ, DF)
TT <- TT %>%
  mutate(has_twitter = ifelse(is.na(twitter), 0 , 1))

Reg <- DFF %>%
  select(twitter, account_created_date) %>%
  distinct()
TT <- left_join(TT, Reg)
rm(Reg)

# prepare data 
ahci <- TT %>%
  filter(wos_index == "ahci") %>%
  select(twitter, ratio, wos_index) %>%
  filter(!is.na(twitter))
ssci <- TT %>%
  filter(wos_index == "ssci") %>%
  select(twitter, ratio, wos_index) %>%
  filter(!is.na(twitter))
scie <- TT %>%
  filter(wos_index == "scie") %>%
  select(twitter, ratio, wos_index) %>%
  filter(!is.na(twitter))

# create graph
rbind(ahci, ssci) %>%
  rbind(scie) %>%
  mutate(wos_index = case_when(
    wos_index == "ahci" ~ "AHCI",
    wos_index == "ssci" ~ "SSCI",
    wos_index == "scie" ~ "SCIE",
  )) %>%
  ggplot(aes(x = ratio)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "black", "white")) +
  scale_x_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.title=element_blank()) +
  xlab("community engagement ratio") +
  facet_wrap(~ wos_index, ncol = 1) +
  theme(strip.text.x = element_text(size = 10, face = "bold",
                                    margin = margin ( t = 5 )))

ggsave("Graph\\community_engagement.png",
       width = 6.5,
       height = 3,
       units = "in",
       dpi = 300)

# Summary Statistics
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

TT %>%
  select(twitter, ratio, wos_index) %>%
  filter(!is.na(twitter)) %>%
  group_by(wos_index) %>%
  filter(!is.na(ratio)) %>%
  summarise(
    avg = mean(ratio, na.rm = TRUE),
    med = median(ratio, na.rm = TRUE),
    mode = Mode(ratio),
    sd = sd(ratio, na.rm = TRUE),
    min = min(ratio, na.rm = TRUE),
    q1 = quantile(ratio, probs = 0.25, na.rm = TRUE),
    q3 = quantile(ratio, probs = 0.75, na.rm = TRUE),
    max = max(ratio, na.rm = TRUE)
  )
TT %>%
  select(twitter, ratio) %>%
  filter(!is.na(twitter)) %>%
  filter(!is.na(ratio)) %>%
  summarise(
    avg = mean(ratio, na.rm = TRUE),
    med = median(ratio, na.rm = TRUE),
    mode = Mode(ratio),
    sd = sd(ratio, na.rm = TRUE),
    min = min(ratio, na.rm = TRUE),
    q1 = quantile(ratio, probs = 0.25, na.rm = TRUE),
    q3 = quantile(ratio, probs = 0.75, na.rm = TRUE),
    max = max(ratio, na.rm = TRUE)
  )
