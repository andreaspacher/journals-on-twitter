library(tidyverse)

# read file with Twitter accounts
df <- read_csv("twitter_accounts_of_journals.csv") %>%
  select(journal_title, twitter) %>%
  filter(!is.na(twitter)) %>%
  distinct()

# prepare
DF <- list()

# open the Twitter API to get follower counts
for(i in 3388:nrow(df)) {

  print(paste0(i, "/", nrow(df), ": ", df$journal_title[i], " (@", df$twitter[i], ")"))
  
  URL <- paste0("https://cdn.syndication.twimg.com/widgets/followbutton/info.json?screen_names=", df$twitter[i])
  js <- rjson::fromJSON(readLines(URL, warn = FALSE))
  
  FOLL <- js[[1]][["followers_count"]]
  
  print(paste0("Followers: ", FOLL))
  
  # save  
  DF[[i]] <- data.frame(
    "journal_title" = df$journal_title[i],
    "twitter" = df$twitter[i],
    "followers" = FOLL,
    "date" = Sys.Date()
  )
  
  #Sys.sleep(0.5)
  
  rm(js)
  
}

# bind all data together
DFF <- data.table::rbindlist(DF)

# save as CSV-file
write_csv(DFF, paste0("Follower_Counts\\followers-", Sys.Date(), ".csv"))
