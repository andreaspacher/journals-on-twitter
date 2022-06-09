library(tidyverse)
library(httr)
library(jsonlite)
library(rtweet)

# read data
JJ <- read_csv("twitter_accounts_of_journals.csv")

twaccounts <- JJ %>%
  select(twitter) %>%
  filter(!is.na(twitter)) %>%
  distinct()

twaccounts$twitter <- stringr::str_remove(twaccounts$twitter, "https://twitter.com/")
# =================
# Twitter API -- your API access data here!
# =================

# App name
an <- ""
# API key
ak <- ""
# API secret key
ask <- ""
# Access token
at <- ""
# Access token secret
ats <- ""

t <- create_token(an, ak, ask, at, ats)

# =====================
# Acccess API
# =====================

DF <- list()

for (i in 1:nrow(twaccounts)) {
  handle <- as.character(twaccounts[i, 1])
  
  printtext <- paste0(i, "/", nrow(twaccounts), ": ", twaccounts[i, 1], " (@", handle, ")")
  print(printtext)
  
  twtimeline <- rtweet::get_timeline(handle, n = 3200, token = t)
  
  # total nr of tweets
  total_nr_tweets <- nrow(twtimeline)
  
  if (total_nr_tweets == 0) {
    
    xyz <- rtweet::lookup_users(handle, token = t)
    
    xyz <- xyz %>%
      select(account_created_at, description, followers_count)
    
    DFJ <- data.frame(
      twaccounts[twaccounts$twitter == handle, ],
      "year" = "all",
      "tweets_total" = 0,
      "unique_mentions" = 0,
      "unique_rt" = 0,
      "unique_rep" = 0,
      "uniquenames_mentions" = NA,
      "uniquenames_rt" = NA,
      "uniquenames_rep" = NA,
      "uniquenames" = NA,
      "account_created_date" = xyz$account_created_at,
      "account_description" = xyz$description,
      "account_followers" = xyz$followers_count,
      "date" = Sys.Date()
    )
  } else {
    twtimeline <- twtimeline %>%
      mutate(year = substr(created_at, 1, 4))
    
    allyears <- twtimeline %>%
      select(year) %>%
      distinct() %>%
      rbind(year = "all") %>%
      unlist() %>%
      unname()
    
    DF_J <- list()
    
    for (ii in 1:length(allyears)) {
      if (allyears[ii] != "all") {
        twt2 <- twtimeline %>%
          filter(year == allyears[ii])
      } else {
        twt2 <- twtimeline
      }
      
      allusers <- twt2 %>%
        filter(is_retweet == FALSE) %>%
        select(mentions_screen_name) %>%
        filter(!is.na(mentions_screen_name)) %>%
        unlist() %>%
        unname()
      allusers <- allusers[!allusers == handle]
      
      rtusers <- twt2[twt2$is_retweet == TRUE, ] %>%
        select(retweet_screen_name) %>%
        filter(!is.na(retweet_screen_name)) %>%
        unlist() %>%
        unname()
      rtusers <- rtusers[!rtusers == handle]
      
      repusers <- twt2[!is.na(twt2$reply_to_user_id), ] %>%
        select(reply_to_screen_name) %>%
        filter(!is.na(reply_to_screen_name)) %>%
        unlist() %>%
        unname
      repusers <- repusers[!repusers == handle]
      
      uniquenames <- c(rtusers, allusers, repusers)
      uniquenames <- paste(uniquenames, collapse = ";")
      
      uniquenames_mentions <- paste(allusers, collapse = ";")
      uniquenames_rt <- paste(rtusers, collapse = ";")
      uniquenames_rep <- paste(repusers, collapse = ";")
      
      DF_J[[ii]] <- data.frame(
        twaccounts[twaccounts$twitter == handle, ],
        "year" = allyears[ii],
        "tweets_total" = nrow(twt2),
        "unique_mentions" = length(unique(allusers)),
        "unique_rt" = length(unique(rtusers)),
        "unique_rep" = length(unique(repusers)),
        "uniquenames_mentions" = uniquenames_mentions,
        "uniquenames_rt" = uniquenames_rt,
        "uniquenames_rep" = uniquenames_rep,
        "uniquenames" = uniquenames,
        "account_created_date" = substr(twtimeline$account_created_at[1], 1, 10),
        "account_description" = twtimeline$description[1],
        "account_followers" = twtimeline$followers_count[1],
        "date" = Sys.Date()
      )
    }
    
    DFJ <- dplyr::bind_rows(DF_J)
    rm(DF_J)
  }
  
  DF[[i]] <- DFJ
  
  print("----- done.")
  
  Sys.sleep(1)
}


DFF <- do.call(rbind, DF)

arrow::write_parquet(DFF, "Summary_Statistics\\twitter_data.parquet")
