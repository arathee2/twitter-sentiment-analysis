## Sentiment Analysis

		library(twitteR)
		library(tm)
		library(SnowballC)
		library(wordcloud)
		library(RCurl)
		library(ROAuth)
		library(stringr)
		# install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
		library(tm.lexicon.GeneralInquirer)
		library(quantmod)
		# install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org")
		library(tm.plugin.sentiment)

		options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

		consumer.key <- ""
		consumer.secret <- ""
		access.token <- ""
		access.secret <- ""

		setup_twitter_oauth(consumer.key, consumer.secret, access.token, access.secret)

		tweets <- searchTwitter("#topic", n = 8000, lang = "en")
		tmp <- tweets
		tmp <- laply(tmp, function(x) x$getText()) # to directly use tweets don't use tmp.
		str(tmp)
		tmp <- tmp[-c(19,26,29,30,49,94)] # each time we use str(tmp) it will return an integer value. Remove that from tmp.

		# now remove those same values from "tweets"
		
		tweets <- tweets[-c(19,26,29,30,49,94)]
		tweets <- ldply(tweets, function(x) x$toDataFrame())
		write.csv(tweets, "tweets.csv")
		tweets <- read.csv("tweets.csv") # reading from csv necessary in order to post on github.All the work before this not to be published.

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
		        
		        # remove at people
		        sentence <- gsub('@\\w+', '', sentence)
		        
		        # remove punctuations
		        sentence <- gsub('[[:punct:]]', '', sentence)
		        
		        # remove numbers
		        sentence <- gsub('[[:digit:]]', '', sentence)
		        
		        # remove html links
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

		tweets.analysis <- sentiment.score(money, positive, negative, .progress="none")
		str(tweets.analysis)
		bag <- tweets.analysis$text
