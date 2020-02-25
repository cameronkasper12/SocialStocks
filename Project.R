#install.packages("rtweet")
#install.packages("tidytext")
#install.packages("ggplot2")
#install.packages("dplyr")
####################################################################################################
                                                                                   ## store api keys
appname <- "MoldSearch"
key <- "PIkNDOuOP1XncLI9Mtp4MEZrO"                                                       ## api key 
secret <- "Qk4efz3C703F087zOIQtoNwD0pheSVRNg0C6jKacWRRyI0yBb9"                        ## api secret
access_token <- "3165956396-Kt1D2Klnl9eEIbAhNmCTGLsGK5f5RvA8mRlRuTi"
access_secret <- "1saWbKiOulpRfvxNqISBGavlX5Bh8fQDiggoLhiStuXdK"
####################################################################################################
# load twitter library - the rtweet library is recommended 
library(rtweet)
####################################################################################################
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
####################################################################################################
# text mining library
library(tidytext)
####################################################################################################
# create token named "twitter_token"
twitter_token <- create_token(app = appname,consumer_key = key,consumer_secret = secret,access_token = access_token,access_secret = access_secret)
####################################################################################################
#Searching tweets
#mold_tweets <- search_tweets("#mold", n = 3000, type = "recent", include_rts = FALSE)
####################################################################################################
#Search Users talking about something
#users <- search_users("mold", n = 3000)
#length(unique(users$location))
#users %>%
#  ggplot(aes(location))+geom_bar()+coord_flip()+labs(x = "Count", y = "Location", title = "Twitter users - unique locations")
#Search Users talking about something
users <- search_users("Asthma", n = 3000)
length(unique(users$location))
users %>%
  ggplot(aes(location))+geom_bar()+coord_flip()+labs(x = "Count", y = "Location", title = "Twitter users - unique locations")
####################################################################################################
#Search and display top locations where something is being said
users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  na.omit() %>%
  top_n(20)%>%
  ggplot(aes(x = location, y = n)) + 
  geom_col() + 
  coord_flip() + 
      labs(x = "Location",
      y = "Count", title = "Twitter users - unique locations")
