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
library("ggplot2")

consumer_key <- 'PIkNDOuOP1XncLI9Mtp4MEZrO'
consumer_secret <- 'Qk4efz3C703F087zOIQtoNwD0pheSVRNg0C6jKacWRRyI0yBb9'
access_token <- '3165956396-Kt1D2Klnl9eEIbAhNmCTGLsGK5f5RvA8mRlRuTi'
access_secret <- '1saWbKiOulpRfvxNqISBGavlX5Bh8fQDiggoLhiStuXdK'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#tweets_Tesla <- searchTwitter("#Tesla", n=1000,lang = "en")

Tesla_tweets <- twListToDF(tweets_Tesla)

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

View(Tesla_text)

#clean up by removing stop words


library("wordcloud")
#generate wordcloud
wordcloud(Tesla_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function
mysentiment_Tesla <-get_nrc_sentiment((Tesla_text))

#calculationg total score for each sentiment
Sentimentscores_Tesla<-data.frame(colSums(mysentiment_Tesla[,]))

names(Sentimentscores_Tesla)<-"Score"
Sentimentscores_Tesla<-cbind("sentiment"=rownames(Sentimentscores_Tesla),Sentimentscores_Tesla)
rownames(Sentimentscores_Tesla)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_Tesla,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla")
