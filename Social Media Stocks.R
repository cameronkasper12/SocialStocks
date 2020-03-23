library("twitteR")
library("ROAuth")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")

consumer_key <- 'PIkNDOuOP1XncLI9Mtp4MEZrO'
consumer_secret <- 'Qk4efz3C703F087zOIQtoNwD0pheSVRNg0C6jKacWRRyI0yBb9'
access_token <- '3165956396-Kt1D2Klnl9eEIbAhNmCTGLsGK5f5RvA8mRlRuTi'
access_secret <- '1saWbKiOulpRfvxNqISBGavlX5Bh8fQDiggoLhiStuXdK'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_t <- searchTwitter("#Tesla", n=1000,lang = "en")

Tesla_tweets <- twListToDF(tweets_t)

View(Tesla_tweets)

Tesla_text<- Tesla_tweets$text

#convert all text to lower case
Tesla_text<- tolower(Tesla_text)

# Replace blank space ("rt")
Tesla_text <- gsub("rt", "", Tesla_text)

# Replace @UserName
Tesla_text <- gsub("@\\w+", "", Tesla_text)

# Remove punctuation
Tesla_text <- gsub("[[:punct:]]", "", Tesla_text)

# Remove links
Tesla_text <- gsub("http\\w+", "", Tesla_text)

# Remove tabs
Tesla_text <- gsub("[ |\t]{2,}", "", Tesla_text)

# Remove blank spaces at the beginning
Tesla_text <- gsub("^ ", "", Tesla_text)

# Remove blank spaces at the end
Tesla_text <- gsub(" $", "", Tesla_text)

#clean up by removing stop words
Tesla_tweets.text.corpus <- tm_map(Tesla_tweets.text.corpus, function(x)removeWords(x,stopwords()))

library("wordcloud")
#generate wordcloud
wordcloud(Tesla_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

