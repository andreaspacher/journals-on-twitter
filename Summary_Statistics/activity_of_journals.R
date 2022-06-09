library(tidyverse)

# read data
DFF <- arrow::read_parquet("Summary_Statistics\\twitter_data.parquet")

# function for Modal value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

DF <- DFF %>%
  filter(year == 2020 | year == 2021)
# library(plyr)
# ALLNAMES <- plyr::ddply(DF, .(twitter), summarize, allnames = paste(uniquenames, collapse=";")) %>%
#   separate_rows(allnames) %>%
#   distinct() %>%
#   select(twitter) %>%
#   group_by(twitter) %>%
#   count()
# colnames(ALLNAMES) <- c("twitter", "total_names")
# ALLREP <- plyr::ddply(DF, .(twitter), summarize, allnames_rep = paste(uniquenames_rep, collapse=";")) %>%
#   separate_rows(allnames_rep) %>%
#   distinct() %>%
#   select(twitter) %>%
#   group_by(twitter) %>%
#   count()
# colnames(ALLREP) <- c("twitter", "total_names_rep")
# ALLRT <- plyr::ddply(DF, .(twitter), summarize, allnames_rt = paste(uniquenames_rt, collapse=";")) %>%
#   separate_rows(allnames_rt) %>%
#   distinct() %>%
#   select(twitter) %>%
#   group_by(twitter) %>%
#   count()
# colnames(ALLRT) <- c("twitter", "total_names_rt")
# ALLMEN <- plyr::ddply(DF, .(twitter), summarize, allnames_mentions = paste(uniquenames_mentions, collapse=";")) %>%
#   separate_rows(allnames_mentions) %>%
#   distinct() %>%
#   select(twitter) %>%
#   group_by(twitter) %>%
#   count()
# colnames(ALLMEN) <- c("twitter", "total_names_mentions")
# detach(package:plyr)
DF$tweets_total <- as.numeric(DF$tweets_total)
DF <- DF %>%
  select(-account_created_date, -year, -starts_with("uniquenames")) %>%
  group_by(twitter) %>%
  mutate(totaltw = sum(tweets_total))

JJ <- read_csv("twitter_accounts_of_journals.csv")
JJ <- JJ %>%
  pivot_longer(cols = c("ahci", "ssci", "scie"),
               names_to = "wos_index") %>%
  filter(value == 1) %>%
  select(-value)
TT <- left_join(JJ, DF)
TT <- TT %>%
  mutate(has_twitter = ifelse(is.na(twitter), 0 , 1))

TT <- TT %>%
  mutate(wos_index = case_when(
  wos_index == "ahci" ~ "AHCI",
  wos_index == "ssci" ~ "SSCI",
  wos_index == "scie" ~ "SCIE",
))


# ===========
# Summary Statistics
# ===========
# by index
TT %>%
  filter(!is.na(totaltw)) %>%
  select(twitter, totaltw, wos_index) %>%
  distinct() %>%
  group_by(wos_index) %>%
  summarise(
    avg = mean(totaltw, na.rm = TRUE) / 2,
    med = median(totaltw, na.rm = TRUE) / 2,
    mode = Mode(totaltw) / 2,
    sd = sd(totaltw, na.rm = TRUE) / 2,
    min = min(totaltw, na.rm = TRUE) / 2,
    q1 = quantile(totaltw, probs = 0.25, na.rm = TRUE) / 2,
    q3 = quantile(totaltw, probs = 0.75, na.rm = TRUE) / 2,
    max = max(totaltw, na.rm = TRUE) / 2
  )

# total
TT %>%
  filter(!is.na(totaltw)) %>%
  select(twitter, totaltw) %>%
  distinct() %>%
  summarise(
    avg = mean(totaltw, na.rm = TRUE) / 2,
    med = median(totaltw, na.rm = TRUE) / 2,
    mode = Mode(totaltw) / 2,
    sd = sd(totaltw, na.rm = TRUE) / 2,
    min = min(totaltw, na.rm = TRUE) / 2,
    q1 = quantile(totaltw, probs = 0.25, na.rm = TRUE) / 2,
    q3 = quantile(totaltw, probs = 0.75, na.rm = TRUE) / 2,
    max = max(totaltw, na.rm = TRUE) / 2
  )

# most prolific accounts 
TT %>%
  select(journal_title, totaltw) %>%
  mutate(totaltw = totaltw/2) %>%
  arrange(desc(totaltw)) %>%
  distinct() %>%
  View()

# Tweets per year
TT %>%
  select(twitter, totaltw) %>%
  mutate(totaltw = totaltw/2) %>%
  distinct() %>%
  filter(!is.na(totaltw)) %>%
  summarise(totaltw = sum(totaltw))

# ===========
# graph
# ===========

TT <- TT %>%
  distinct(twitter, totaltw, wos_index)

ann_label <- data.frame(wos_index = c("AHCI", "SCIE", "SSCI"),
                        label = c(
                          median(TT$totaltw[TT$wos_index == "AHCI"], na.rm = T)/2,
                          median(TT$totaltw[TT$wos_index == "SCIE"], na.rm = T)/2,
                          median(TT$totaltw[TT$wos_index == "SSCI"], na.rm = T)/2
                        ))
ann_label$label = paste0("Median: ", ann_label$label)

TT %>%
  select(twitter, totaltw, wos_index) %>%
  distinct() %>%
  ggplot(aes(y = totaltw / 2)) +
  geom_histogram(binwidth = 5) +
  # geom_hline(yintercept = median(TT$totaltw, na.rm = T),
  #            linetype = "dashed") +
  geom_hline(data = filter(TT, wos_index == "AHCI"),
             aes(yintercept = median(totaltw/2, na.rm = T)),
             linetype = "dashed") +
  geom_hline(data = filter(TT, wos_index == "SCIE"),
             aes(yintercept = median(totaltw/2, na.rm = T)),
             linetype = "dashed") +
  geom_hline(data = filter(TT, wos_index == "SSCI"),
             aes(yintercept = median(totaltw/2, na.rm = T)),
             linetype = "dashed") +
  geom_text(data = ann_label,
            aes(label = label), x = 50, y = ifelse(ann_label$wos_index == "SCIE", 270,
                                                   ifelse(ann_label$wos_index == "SSCI", 200, 180)),
            size = 3) +
  xlab("Journals") +
  ylab("Tweets per Year") + 
  theme_minimal() +
  facet_wrap(~ wos_index, nrow = 3) +
  theme(strip.text.x = element_text(size = 10, face = "bold",
                                    margin = margin ( t = 5 ))) +
  scale_y_continuous(breaks = seq(0, 1500, by = 100)) +
  coord_flip()

ggsave("Graph\\active_journals_histogram_by_index.png",
       width = 6.8,
       height = 5.0,
       units = "in",
       dpi = 300)
