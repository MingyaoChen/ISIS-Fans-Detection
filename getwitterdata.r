setwd("/home/mingyao/Dropbox/R/ImbalancedClassification")

library('ROAuth')
library("twitteR")
library('streamR')
library('RCurl')
library('RJSONIO')
library('stringr')

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerkey <- 'DSEo8Tmt68Ku8CDgcJ3Mko4as'
consumersecret <- 'tjxVTChJLb19RLkaikWfWpneNnVO7PCFp2wxukj0syyjx8h3qJ'
accesstoken <- '497716652-uokaQZ1JtyPsONyFYXcFkPKC5NcQ6oTMXxv1nhA5'
accesssecret <- 'oaTrNJXwUnCDr8RpFi689t28AxGqlB0G9w9ia6JCoXwtq'

my_oauth <- OAuthFactory$new(consumerKey = consumerkey, 
                             consumerSecret = consumersecret, 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "dataset/my_oauth.Rdata")
load("dataset/my_oauth.Rdata")

file = "dataset/normal.json"
track = NULL
follow = NULL
loc = c(-125, 30, -114, 42)
lang = NULL
minutes = 0.5
time = 0
tweets = 2000
filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             timeout = time, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)

normalTweets <- parseTweets(file, verbose = TRUE)

tuser <- getUser("LesaPR")
