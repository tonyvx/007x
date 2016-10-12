
consumer_key <- "OvgLoSeWF2Rly2AHEePxHEz46"
consumer_secret <- "8viYlXZqBhRWkd7L6oI9ySl5NLjn1xHLDF6cTF8dHo2UjnipQU"
twitteR::setup_twitter_oauth(consumer_key, consumer_secret,"","")
twitteR::searchTwitter('apartment hunting', geocode='40.7361,-73.9901,5mi',  n=40, retryOnRateLimit=1)