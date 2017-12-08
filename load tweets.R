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



api_key <- "Jtn5o3HtOrFsCg8GeAHavkEIT"
api_secret <- "fE76ZkrLIc8CKHSDdQZNgc4s8cASM6RJdACiVsjhfJAH3KK8Ao"
token <- "935672346089852928-TH3Hct40BCCkBbd2qhaes68BpZCeUqO"
token_secret <- "6dklF15aLh2JxwfUdZZgczTDbnkgnwKAKHx5Dwyzzk5kU"
setup_twitter_oauth(api_key, api_secret, token, token_secret)

queries <- c('#tesla', '#microsoft', 'pfizer', 'general electric', '#netflix', 'citrix', '#starbucks')

url <- "mongodb://pgdridge:datascience@cluster0-shard-00-00-xa7nx.mongodb.net:27017,cluster0-shard-00-01-xa7nx.mongodb.net:27017,cluster0-shard-00-02-xa7nx.mongodb.net:27017/test?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin"
mongo<- mongolite::mongo(collection = "tweets", db = "stockTweets", url = url)


mongo_query <- '[{"$group":{"_id": "$company", "max_id" : {"$max": "$id"}}}]'
maxes <- mongo$aggregate(mongo_query)

new_tweets <- mapply(searchTwitteR, searchString = maxes$`_id`, sinceID = maxes$max_id, n = 50000, lang = "en")


df_exist <- F
for (i in 1:length(queries)){
  if (!is_empty(new_tweets[[i]])){
    if(df_exist == F){ 
      df <- twListToDF(new_tweets[[i]]) %>%
        mutate(
          company = queries[i]
        )
      df_exist <- T
    }else{
      temp <- twListToDF(new_tweets[[i]]) %>%
        mutate(
          company = queries[i]
        )
      df <- rbind(df, temp)
    }
  }
}
df$text <-stri_encode(df$text, "", "UTF-8" )


mongo$insert(df)