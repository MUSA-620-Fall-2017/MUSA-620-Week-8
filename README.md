# MUSA-620-Week-8

An overview of text mining ([notes](https://github.com/MUSA-620-Fall-2017/MUSA-620-Week-8/blob/master/week-8-text-mining.pptx))

![Trump tweet words](http://metrocosm.com/trump-tweet-words.png "Trump tweet words")
Trump tweets: which are the words most likely to be from Android and most likely from iPhone?

## Links

The example we replicated in class, worth a read: [Text analysis of Trump's tweets confirms he writes only the (angrier) Android half](http://varianceexplained.org/r/trump-tweets/)

[NRC Word-Association Lexicon](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) -- the sentiment classifier we used in class (via the syuzhet R package)

[Stanford's Core NLP library](http://stanfordnlp.github.io/CoreNLP/) -- the standard programming library for natural language processing

[D3 examples](https://github.com/MUSA-620-Fall-2017/d3)

## Assignment

This assignment is not required. You may turn it in by email (galkamaxd at gmail) or in person at class.

**Due:** 22-Mar


### Task:

Choose 3 major brands that have an active presence on Twitter (e.g. Coca Cola, McDonald's, Apple, etc) and compare the sentiment of tweets that mention them. The sample size for each should be at least 1,000.

To collect the tweets, you may use either the Twitter streaming or rest API, searching for the brand's Twitter handle as a keyword (e.g. "searchTwitter('@CocaCola', n=4000)"). Retweets should be excluded from the analysis (any tweet where "isRetweet" = true).

To classify the sentiment, use the syuzhet R package as we did in class. In that example, we classified the sentiment of individual words. In this case, the items you are classifying are the full text of each tweet. The command for running the classification should look something like this: get_nrc_sentiment(tweetDf$text).

### Deliverable:

The final results and all code used.

