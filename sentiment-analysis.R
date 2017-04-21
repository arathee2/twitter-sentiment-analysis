## Sentiment Analysis

library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(ROAuth)
library(stringr)
library(plyr)
library(ggplot2)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

consumer.key <- ""
consumer.secret <- ""
access.token <- ""
access.secret <- ""
setup_twitter_oauth(consumer.key, consumer.secret, access.token, access.secret)

# get tweets
tweets <- searchTwitter("#topic", n = 8000, lang = "en")

# to publish data frame on kaggle.
tweets.df <- ldply(money, function(x) x$toDataFrame())

# to directly use tweets.
tweets <- laply(tweets, function(x) x$getText())
str(tweets)
tweets <- tweets[-c(19,26,29,30,49,94)] # each time we use str(tweets) it will return an integer value. Remove that from tweets.

write.csv(as.data.frame(tweets), "tweets-file.csv")

# to run rmarkdown, code starts here.
tweets.df <- read.csv("tweets-file.csv")
tweets <- as.character(tweets.df$tweets)

sentiment.score <- function(sentences, positive.words, negative.words, .progress='none')
{
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
    # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
    scores <- laply(sentences, function(sentence, positive.words, negative.words)
    {
		        
        ## clean up sentences with R's regex-driven global substitute, gsub():
	        
        sentence <- gsub('[[:cntrl:]]', '', sentence)
		        
        # remove retweets
        sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)

        # remove hashtags
        sentence = gsub('#\\S+', '', sentence)

        # remove mentions
        sentence <- gsub('@\\w+', '', sentence)
		        
        # remove punctuations
        sentence <- gsub('[[:punct:]]', '', sentence)
		        
        # remove numbers
        sentence <- gsub('[[:digit:]]', '', sentence)
		        
        # remove URLs
        sentence <- gsub('http[s]?\\w+', '', sentence)
		        
        # remove extra spaces
        sentence <- gsub('[ \t]{2,}', '', sentence)
        sentence <- gsub('^\\s+|\\s+$', '', sentence)
		        
        # removing NA's
        sentence <- sentence[!is.na(sentence)]
		        
        # convert to lower case:
        sentence <- tolower(sentence)
		        
        # split into words. str_split is in the stringr package
		        
        word.list <- str_split(sentence, '\\s+')
		        
        # sometimes a list() is one level of hierarchy too much
		        
        words <- unlist(word.list)
	        
        # compare our words to the dictionaries of positive & negative terms
		        
        negative.matches <- match(words, negative.words)
        positive.matches <- match(words, positive.words)
		        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
		        
        positive.matches <- !is.na(positive.matches)
        negative.matches <- !is.na(negative.matches)
		        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		        
        score <- sum(positive.matches) - sum(negative.matches)
		        
        return(score)
    }, positive.words, negative.words, .progress=.progress )
		    
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
}

positive <- scan("positive-words.txt", what= "character", comment.char= ";")
negative <- scan("negative-words.txt", what= "character", comment.char= ";")

tweets.analysis <- sentiment.score(tweets, positive, negative, .progress="none")
str(tweets.analysis)

tweets.analysis$sentiment[tweets.analysis$score == 0] <- "Neutral" 
tweets.analysis$sentiment[tweets.analysis$score < 0] <- "Negative"
tweets.analysis$sentiment[tweets.analysis$score > 0] <- "Positive"
tweets.analysis$sentiment <- factor(tweets.analysis$sentiment)

bag <- tweets.analysis$text
wordcloud(bag)
