library(RCurl)
library(tidyverse)
library(jsonlite)
library(mongolite)
url <- "mongodb://pgdridge:datascience@cluster0-shard-00-00-xa7nx.mongodb.net:27017,cluster0-shard-00-01-xa7nx.mongodb.net:27017,cluster0-shard-00-02-xa7nx.mongodb.net:27017/test?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin"
mongo<- mongolite::mongo(collection = "quotes", db = "stockTweets", url = url)
mongo_query <- '[{"$group":{"_id": "$company", "max_time" : {"$max": "$time"}}}]'
maxes <- mongo$aggregate(mongo_query)


vantage_key <- "ND86A8SH1H0SISRH"
tickers <- c("nflx", "msft", "tsla", "pfe", "ge", "ctxs", "sbux")
prices_base <- data.frame (open = double(), close = double(), character (), company = character (), stringsAsFactors = F)
urls <- str_c("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=" , tickers, "&interval=30min&outputsize=full&apikey=", vantage_key)
function (index){
  open <- my_list[[index]][[1]]
  close <- my_list[[index]][[4]]
  c(open =open, close = close)
}
for (i in 1:length(urls)){
  response <- fromJSON(urls[i])
  my_list <- response$`Time Series (30min)`
  time <- names(my_list)
  vec <- 1:length(my_list)
  prices <- sapply(vec, get_prices)
  company <- rep(tickers[i], length(vec))
  prices_df <- data.frame(t(prices), time, company, stringsAsFactors = F)
  prices_base <- rbind(prices_base, prices_df)
}
inserts <- prices_base %>%
  inner_join(maxes, by = c("company" = "_id")) %>%
  filter(time > max_time) %>%
  select(-max_time)
  
mongo$insert(inserts)
all_quotes <- mongo$find ()