library(tidyverse)

# read data
df <- read_csv("twitter_accounts_of_journals.csv")

# function for Modal value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# create column with Web of Science index
df <- df %>%
  select(journal_title, issn, twitter, has_twitter, ahci, ssci, scie,
         wos_scie, wos_ahci, wos_ssci) %>%
  distinct() %>%
  pivot_longer(cols = c("ahci", "ssci", "scie"),
               names_to = "wos_index") %>%
  filter(value == 1) %>%
  select(-value)

# summary statistics for SCIE
SCIE <- df %>%
  filter(wos_index == "scie") %>%
  distinct() %>%
  separate_rows(wos_scie, sep = "\\|") %>%
  mutate(wos_scie = trimws(wos_scie))

SCIE %>%
  group_by(wos_scie) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = n(),
            share = with / total * 100) %>%
  filter(total > 9) %>%
  filter(!is.na(wos_scie)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  filter(name == "with") %>%
  summarise(
    avg = mean(share, na.rm = TRUE),
    med = median(share, na.rm = TRUE),
    mode = Mode(share),
    sd = sd(share, na.rm = TRUE),
    min = min(share, na.rm = TRUE),
    q1 = quantile(share, probs = 0.25, na.rm = TRUE),
    q3 = quantile(share, probs = 0.75, na.rm = TRUE),
    max = max(share, na.rm = TRUE)
  )

# summary statistics for SSCI
SSCI <- df %>%
  filter(wos_index == "ssci") %>%
  distinct() %>%
  separate_rows(wos_ssci, sep = "\\|") %>%
  mutate(wos_ssci = trimws(wos_ssci))

SSCI %>%
  group_by(wos_ssci) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>% 
  filter(total > 5) %>%
  filter(!is.na(wos_ssci)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  filter(name == "with") %>%
  summarise(
    avg = mean(share, na.rm = TRUE),
    med = median(share, na.rm = TRUE),
    mode = Mode(share),
    sd = sd(share, na.rm = TRUE),
    min = min(share, na.rm = TRUE),
    q1 = quantile(share, probs = 0.25, na.rm = TRUE),
    q3 = quantile(share, probs = 0.75, na.rm = TRUE),
    max = max(share, na.rm = TRUE)
  )

# summary statistics for AHCI
AHCI <- df %>%
  filter(wos_index == "ahci") %>%
  distinct() %>%
  separate_rows(wos_ahci, sep = "\\|") %>%
  mutate(wos_ahci = trimws(wos_ahci))

AHCI %>%
  group_by(wos_ahci) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>%
  filter(total > 5) %>%
  filter(!is.na(wos_ahci)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  filter(name == "with") %>%
  summarise(
    avg = mean(share, na.rm = TRUE),
    med = median(share, na.rm = TRUE),
    mode = Mode(share),
    sd = sd(share, na.rm = TRUE),
    min = min(share, na.rm = TRUE),
    q1 = quantile(share, probs = 0.25, na.rm = TRUE),
    q3 = quantile(share, probs = 0.75, na.rm = TRUE),
    max = max(share, na.rm = TRUE)
  )

# graph --- SSCI & AHCI
pp1 <- df %>%
  filter(wos_index == "ssci") %>%
  distinct() %>%
  separate_rows(wos_ssci, sep = "\\|") %>%
  mutate(wos_ssci = trimws(wos_ssci)) %>%
  group_by(wos_ssci) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>%
  filter(total > 5) %>%
  filter(!is.na(wos_ssci)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  ggplot() +
  geom_col(aes(x = value,
               y = reorder(wos_ssci, share),
               fill = name),
           position = "fill") +
  geom_text(aes(label = ifelse(name == "with", paste0(share, "%"), ""), x = value, y = reorder(wos_ssci, share)),
            size = 2.5,
            hjust = -0.1,
            position = position_fill(vjust = 1)) +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none")

pp2 <- df %>%
  filter(wos_index == "ahci") %>%
  distinct() %>%
  separate_rows(wos_ahci, sep = "\\|") %>%
  mutate(wos_ahci = trimws(wos_ahci)) %>%
  group_by(wos_ahci) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>%
  filter(total > 5) %>%
  filter(!is.na(wos_ahci)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  ggplot() +
  geom_col(aes(x = value,
               y = reorder(wos_ahci, share),
               fill = name),
           position = "fill") +
  geom_text(aes(label = ifelse(name == "with", paste0(share, "%"), ""), x = value, y = reorder(wos_ahci, share)),
            size = 2.5,
            hjust = -0.1,
            position = position_fill(vjust = 1)) +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none")

pp <- cowplot::plot_grid(pp1, pp2, labels = c("SSCI", "AHCI"))

ggsave("Graph\\categories_ahci_ssci.png",
       pp,
       width = 9,
       height = 13,
       units = "in",
       dpi = 300
)

# grpah -- SCIE
scie.prep <- df %>%
  filter(wos_index == "scie") %>%
  distinct() %>%
  separate_rows(wos_scie, sep = "\\|") %>%
  mutate(wos_scie = trimws(wos_scie)) %>%
  group_by(wos_scie) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>%
  filter(total > 5) %>%
  filter(!is.na(wos_scie)) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share))

scie.df <- scie.prep %>% select(wos_scie, share) %>% arrange(desc(share)) %>% distinct()


p1 <- scie.prep %>%
  top_n(176, share) %>%
  ggplot() +
  geom_col(aes(x = value,
               y = reorder(wos_scie, share),
               fill = name),
           position = "fill") +
  geom_text(aes(label = ifelse(name == "with", paste0(share, "%"), ""), x = value, y = reorder(wos_scie, share)),
            size = 2.5,
            hjust = -0.1,
            position = position_fill(vjust = 1)) +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  #labs(title = "SCIE") +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none",
        #plot.title.position = "plot",
        #plot.title = element_text(size = 14, face = "bold")
  )

p2 <- scie.prep %>%
  top_n(-176, share) %>%
  ggplot() +
  geom_col(aes(x = value,
               y = reorder(wos_scie, share),
               fill = name),
           position = "fill") +
  geom_text(aes(label = ifelse(name == "with", paste0(share, "%"), ""), x = value, y = reorder(wos_scie, share)),
            size = 2.5,
            hjust = -0.1,
            position = position_fill(vjust = 1)) +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "none")

scieplot <- cowplot::plot_grid(p1, p2, labels = "SCIE")

ggsave("Graph\\categories_scie.png",
       scieplot,
       width = 9,
       height = 13,
       units = "in",
       dpi = 300)


# ==========
# summary statistics for all categories
# ==========
AHCI %>%
  bind_rows(SCIE) %>%
  bind_rows(SSCI) %>%
  mutate(wos_scie = paste0("scie_", wos_scie),
         wos_ssci = paste0("ssci_", wos_ssci),
         wos_ahci = paste0("ahci_", wos_ahci),
  ) %>%
  pivot_longer(c(wos_scie, wos_ahci, wos_ssci)) %>%
  separate_rows(value, sep = "\\|") %>%
  mutate(value = trimws(value)) %>%
  filter(!grepl("NA", value)) %>%
  group_by(value) %>%
  summarise(with = sum(has_twitter),
            without = n() - with,
            total = with + without,
            share = with / total * 100) %>%
  filter(grepl("^(ssci|scie|ahci)", value)) %>%
  filter(total > 5) %>%
  select("category" = value, with, without, total, share) %>%
  pivot_longer(cols = c(with, without)) %>%
  mutate(name = factor(name, levels = c("without", "with")),
         share = round(share, 1)) %>%
  arrange(desc(share)) %>%
  filter(name == "with") %>%
  summarise(
    avg = mean(share, na.rm = TRUE),
    med = median(share, na.rm = TRUE),
    mode = Mode(share),
    sd = sd(share, na.rm = TRUE),
    min = min(share, na.rm = TRUE),
    q1 = quantile(share, probs = 0.25, na.rm = TRUE),
    q3 = quantile(share, probs = 0.75, na.rm = TRUE),
    max = max(share, na.rm = TRUE)
  )
