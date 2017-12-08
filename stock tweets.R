library(jsonlite)
library(RCurl)
library(twitteR)
library(tidytext)
library(tidyverse)
library(stringr)
library(lubridate)
library(rvest)
library(RCurl)
library(mongolite)
library(stringi)

url <- "mongodb://pgdridge:datascience@cluster0-shard-00-00-xa7nx.mongodb.net:27017,cluster0-shard-00-01-xa7nx.mongodb.net:27017,cluster0-shard-00-02-xa7nx.mongodb.net:27017/test?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin"
mongo<- mongolite::mongo(collection = "tweets", db = "stockTweets", url = url)

all_tweets <- mongo$find()

no_sentiment <- c('shares')

sentiment <- c('downgrade', 'upgrade', 'fall', 'rise', 'bullish', 'bearish', 'bull', 'bear')
scores <- c(-4, 4, -2, 2, 4, -4, 4, -4)
new_sentiment <- data.frame(word = sentiment, score = scores, stringsAsFactors = F)
sentiment_words <- get_sentiments("afinn") %>%
  filter(!(word %in% no_sentiment))%>%
  dplyr::union(new_sentiment)

clean_tweet <- all_tweets %>%
  arrange(created) %>%
  mutate(
    clean_text = str_replace_all(text, 'http(s)?://[\\S]+', 'URL'),
    clean_text = str_replace_all(clean_text, '@[\\S]+', 'USER'),
    clean_text = str_replace_all(clean_text, '[[:punct:]]', ''),
    clean_text = str_replace_all(clean_text, '[^[:alpha:] .]', ''),
    clean_text = tolower(clean_text)
  )

dupe_tweets <- clean_tweet %>%
  group_by(clean_text) %>%
  summarise(count = n()) %>%
  filter(count >= 2)

tweets_final <- clean_tweet %>%
  anti_join(dupe_tweets, "clean_text")

tweets_sentiment <- tweets_final %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiment_words, by = "word") %>%
  group_by(id, created, company) %>%
  summarise(sentiment = sum(score)) %>%
  ungroup () %>%
  mutate(
    index = cumsum(sentiment),
    date = floor_date(created, "day")
  ) %>%
  inner_join(tweets_final, by = "id") %>%
  select(company.x, id,created.x, sentiment, index, date, clean_text) %>%
  rename(company = company.x, created = created.x)

counts <- tweets_sentiment %>%
  filter(sentiment < 0 | sentiment >1) %>%
  mutate(direction = if_else(sentiment > 0, "positive", "negative")) %>%
  group_by(company, date, direction) %>%
  summarize(count = n()) %>%
  ungroup () %>%
  spread(direction, "count")

