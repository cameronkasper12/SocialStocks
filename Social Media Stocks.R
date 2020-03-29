################################################################################################
#                                Libraries and API Access                                      #

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

################################################################################################
#                                      Twitter Search                                          #

#Word Search Beank
# Tesla
# Elon Musk
# SpaceX
# TSLA
# Model 3

tweets_Tesla <- searchTwitter("Tesla", n=1000,lang = "en")
tweets_Elon <- searchTwitter("Elon Musk",n=1000, lang="en")
tweets_SpaceX<- searchTwitter("SpaceX",n=1000, lang="en")
tweets_TSLA <- searchTwitter("TSLA",n=1000, lang="en")
tweets_Model3 <- searchTwitter("Model 3",n=1000, lang="en")
tweets_CyberTruck <- searchTwitter("Cyber Truck",n=1000, lang="en")
tweets_Stock <- searchTwitter("Stock + Tesla",n = 1000, lang = "en")

################################################################################################
#                                      Text Formatting                                         #

#Create a data frame of the tweets 
Tesla_tweets <- twListToDF(tweets_Tesla)
Elon_tweets <- twListToDF(tweets_Elon)
SpaceX_tweets <- twListToDF(tweets_SpaceX)
TSLA_tweets <- twListToDF(tweets_TSLA)
Model3_tweets <- twListToDF(tweets_Model3)
CyberTruck_tweets <- twListToDF(tweets_CyberTruck)
Stock_tweets <- twListToDF(tweets_Stock)


#Create a varibale for just the text of the tweet
Tesla_text <- Tesla_tweets$text
Elon_text <- Elon_tweets$text
SpaceX_text <- SpaceX_tweets$text
TSLA_text <- TSLA_tweets$text
Model3_text <- Model3_tweets$text
CyberTruck_text <- CyberTruck_tweets$text
Stock_text <- Stock_tweets$text

#convert all text to lower case
Tesla_text <- tolower(Tesla_text)
Elon_text <- tolower(Elon_text)
SpaceX_text <- tolower(SpaceX_text)
TSLA_text <- tolower(TSLA_text)
Model3_text <- tolower(Model3_text)
CyberTruck_text <- tolower(CyberTruck_text)
Stock_text <- tolower(Stock_text)


# Replace blank space ("rt")
Tesla_text <- gsub("rt", "", Tesla_text)
Elon_text <- gsub("rt", "", Elon_text)
SpaceX_text <- gsub("rt", "", SpaceX_text)
TSLA_text <- gsub("rt", "", TSLA_text)
Model3_text <- gsub("rt", "", Model3_text)
CyberTruck_text <- gsub("rt", "", CyberTruck_text)
Stock_text <- gsub("rt", "", Stock_text)


# Replace @UserName
Tesla_text <- gsub("@\\w+", "", Tesla_text)
Elon_text <- gsub("@\\w+", "", Elon_text)
SpaceX_text <- gsub("@\\w+", "", SpaceX_text)
TSLA_text <- gsub("@\\w+", "", TSLA_text)
Model3_text <- gsub("@\\w+", "", Model3_text)
CyberTruck_text <- gsub("@\\w+", "", CyberTruck_text)
Stock_text <- gsub("@\\w+", "", Stock_text)


# Remove punctuation
Tesla_text <- gsub("[[:punct:]]", "", Tesla_text)
Elon_text <- gsub("[[:punct:]]","", Elon_text)
SpaceX_text <- gsub("[[:punct:]]", "", SpaceX_text)
TSLA_text <- gsub("[[:punct:]]", "", TSLA_text)
Model3_text <- gsub("[[:punct:]]", "", Model3_text)
CyberTruck_text <- gsub("[[:punct:]]", "", CyberTruck_text)
Stock_text <- gsub("[[:punct:]]", "", Stock_text)


# Remove links
Tesla_text <- gsub("http\\w+", "", Tesla_text)
Elon_text <- gsub("http\\w+","", Elon_text)
SpaceX_text <- gsub("http\\w+", "", SpaceX_text)
TSLA_text <- gsub("http\\w+", "", TSLA_text)
Model3_text <- gsub("http\\w+", "", Model3_text)
CyberTruck_text <- gsub("http\\w+", "", CyberTruck_text)
Stock_text <- gsub("http\\w+", "", Stock_text)

# Remove tabs
Tesla_text <- gsub("[ |\t]{2,}", "", Tesla_text)
Elon_text <- gsub("[ |\t]{2,}","", Elon_text)
SpaceX_text <- gsub("[ |\t]{2,}", "", SpaceX_text)
TSLA_text <- gsub("[ |\t]{2,}", "", TSLA_text)
Model3_text <- gsub("[ |\t]{2,}", "", Model3_text)
CyberTruck_text <- gsub("[ |\t]{2,}", "", CyberTruck_text)
Stock_text <- gsub("[ |\t]{2,}", "", Stock_text)


# Remove blank spaces at the beginning
Tesla_text <- gsub("^ ", "", Tesla_text)
Elon_text <- gsub("^ ","", Elon_text)
SpaceX_text <- gsub("^ ", "", SpaceX_text)
TSLA_text <- gsub("^ ", "", TSLA_text)
Model3_text <- gsub("^ ", "", Model3_text)
CyberTruck_text <- gsub("^ ", "", CyberTruck_text)
Stock_text <- gsub("^ ", "", Stock_text)


# Remove blank spaces at the end
Tesla_text <- gsub(" $", "", Tesla_text)
Elon_text <- gsub(" $","", Elon_text)
SpaceX_text <- gsub(" $", "", SpaceX_text)
TSLA_text <- gsub(" $", "", TSLA_text)
Model3_text <- gsub(" $", "", Model3_text)
CyberTruck_text <- gsub(" $", "", CyberTruck_text)
Stock_text <- gsub(" $", "", Stock_text)


View(Tesla_text)
View(Elon_text)
View(SpaceX_text)
View(TSLA_text)
View(Model3_text)
View(CyberTruck_text)
View(Stock_text)


#Investigate more text formatting by removing stop words

################################################################################################
#                                     Singular Data Visulaization                              #

library("wordcloud")
#generate wordcloud
wordcloud(Tesla_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(Elon_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(SpaceX_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(TSLA_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(Model3_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(CyberTruck_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(Stock_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function
mysentiment_Tesla <-get_nrc_sentiment((Tesla_text))
mysentiment_Elon <-get_nrc_sentiment((Elon_text))
mysentiment_SpaceX <-get_nrc_sentiment((SpaceX_text))
mysentiment_TSLA <-get_nrc_sentiment((TSLA_text))
mysentiment_Model3 <-get_nrc_sentiment((Model3_text))
mysentiment_CyberTruck <-get_nrc_sentiment((CyberTruck_text))
mysentiment_Stock <-get_nrc_sentiment((Stock_text))


#calculationg total score for each sentiment
Sentimentscores_Tesla<-data.frame(colSums(mysentiment_Tesla[,]))
Sentimentscores_Elon<-data.frame(colSums(mysentiment_Elon[,]))
Sentimentscores_SpaceX<-data.frame(colSums(mysentiment_SpaceX[,]))
Sentimentscores_TSLA<-data.frame(colSums(mysentiment_TSLA[,]))
Sentimentscores_Model3<-data.frame(colSums(mysentiment_Model3[,]))
Sentimentscores_CyberTruck<-data.frame(colSums(mysentiment_CyberTruck[,]))
Sentimentscores_Stock<-data.frame(colSums(mysentiment_Stock[,]))

#Scoring
names(Sentimentscores_Tesla)<-"Score"
Sentimentscores_Tesla<-cbind("sentiment"=rownames(Sentimentscores_Tesla),Sentimentscores_Tesla)
rownames(Sentimentscores_Tesla)<-NULL

names(Sentimentscores_Elon)<-"Score"
Sentimentscores_Elon<-cbind("sentiment"=rownames(Sentimentscores_Elon),Sentimentscores_Elon)
rownames(Sentimentscores_Elon)<-NULL

names(Sentimentscores_SpaceX)<-"Score"
Sentimentscores_SpaceX<-cbind("sentiment"=rownames(Sentimentscores_SpaceX),Sentimentscores_SpaceX)
rownames(Sentimentscores_SpaceX)<-NULL

names(Sentimentscores_TSLA)<-"Score"
Sentimentscores_TSLA <-cbind("sentiment"=rownames(Sentimentscores_TSLA),Sentimentscores_TSLA)
rownames(Sentimentscores_TSLA)<-NULL

names(Sentimentscores_Model3)<-"Score"
Sentimentscores_Model3<-cbind("sentiment"=rownames(Sentimentscores_Model3),Sentimentscores_Model3)
rownames(Sentimentscores_Model3)<-NULL

names(Sentimentscores_CyberTruck)<-"Score"
Sentimentscores_CyberTruck<-cbind("sentiment"=rownames(Sentimentscores_CyberTruck),Sentimentscores_CyberTruck)
rownames(Sentimentscores_CyberTruck)<-NULL

names(Sentimentscores_Stock)<-"Score"
Sentimentscores_Stock<-cbind("sentiment"=rownames(Sentimentscores_Stock),Sentimentscores_Stock)
rownames(Sentimentscores_Stock)<-NULL


#plotting the sentiments with scores
ggplot(data=Sentimentscores_Tesla,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla")

ggplot(data=Sentimentscores_Elon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Elon Musk")

ggplot(data=Sentimentscores_SpaceX,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on SpaceX")

ggplot(data=Sentimentscores_TSLA,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla Stock ID")

ggplot(data=Sentimentscores_Model3,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla Model 3")

ggplot(data=Sentimentscores_CyberTruck,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla Cyber Truck")

ggplot(data=Sentimentscores_Stock,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments of people behind the tweets on Tesla stock")

################################################################################################
#                                     Combining Data                                           #
