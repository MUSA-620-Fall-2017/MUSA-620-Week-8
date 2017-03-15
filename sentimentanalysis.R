library(dplyr)
library(tidyr)

library(ROAuth) #Twitter api authorization
library(streamR) #Streaming api
library(twitteR) #Rest api

library(lubridate) #functions for date/time data
library(scales) #scales for data visualization

library(ggplot2)
library(stringr) #string manipulation

library(tidytext) #for text mining


regexExample <- "(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$)"
str_detect("myemail@mydomain.com", regexExample)


access_token <- ""
access_secret <-""
consumer_key <- ""
consumer_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

trump_tweets <- userTimeline("realDonaldTrump", n = 3200)

trumpDf <- twListToDF(trump_tweets)

tweets <- trumpDf %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))



# Comparison 1: Tweets by hour of day

tweets %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")




# Comparison 2: Quoted tweets

tweets %>%
  count(source, quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')




# Comparison 3: Tweets with images

tweet_picture_counts <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source, picture = ifelse(str_detect(text, "t.co"), "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")





reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%    #does the tweet begin with a quote
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))


tweet_words %>%
  filter(source == 'iPhone') %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

tweet_words %>%
  filter(source == 'Android') %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()




android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))



iPhoneTweets <- count(tweet_words,word, source) %>%
  filter(source == 'iPhone')
#  select(word, iPhoneCount = n)

androidTweets <- count(tweet_words,word, source) %>%
  filter(source == 'Android')
#  select(word, androidCount = n)


iPhoneSentiment <- get_nrc_sentiment(iPhoneTweets$word)

androidSentiment <- get_nrc_sentiment(androidTweets$word)

summarise_each(iPhoneSentiment, funs(mean))

summarise_each(androidSentiment, funs(mean))

summarise(sentiment, anger=mean(anger), anticipation=mean(anticipation), anger=mean(disgust), anticipation=mean(anticipation))




