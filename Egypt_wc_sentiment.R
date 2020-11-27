###Sentiment Analyis of Egypt World Cup Reactions:
library(SnowballC)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(graph)
library(cluster)
library(slam)
library(topicmodels)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(purrr)
library(stringr)
install.packages("tidytext", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(tidytext)
library(devtools)
install_github("juliasilge/tidytext")
#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(ggplot2)

#install.packages("twitteR")
library(twitteR)

getwd()
setwd("dirctory)


# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "xxxx"
consumer_secret <- "xxxx"
access_token <- "xxxxx"
access_secret <- "xxxxxs"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- twitteR::searchTwitter('#WorldCup + #Egypt', n = 500, lang ="en",  since = '2018-06-20')
#strip retweets
strip_retweets(tweets)

tweets[1:5]

#Text Cleaning:
# convert tweets to a data frame
# tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
tweets.df <- twListToDF(tweets)
dim(tweets.df)

write.csv(tweets.df, file = "tweets.csv")

text = tweets.df$text   
# remove emoticons
text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub=""))
corpus = Corpus(VectorSource(text))
myCorpus <- corpus

library(tm)
# build a corpus, and specify the source to be character vectors
#myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via", "rt", "vs", "m", "us", "will", "live", "predict"
                 , "iflix", "monaeltahawi", "app", "tun", "kappilinho", "wait", "left", "amp")

# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}


#Stem Completeion:
myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

## Freqency words and Association
idx <- which(dimnames(tdm)$Terms == "Egypt")
inspect(tdm[idx + (0:5), 101:110])

#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=25))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=25)
df <- data.frame(term = names(term.freq), freq = term.freq)


library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

# which words are associated with 'egypt'?
findAssocs(tdm, "egypt", 0.2)
# which words are associated with 'mosalah'?
findAssocs(tdm, "mosalah", 0.25)


# Plot Word Association:
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.25, weighting = T)

# Create a Word cloud:
library(wordcloud)
## Set color
library(RColorBrewer)
pal <- brewer.pal(8,"Dark2")
pal <- pal[-(1:2)]
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 15,
          random.order = F, colors = pal)

#topic modeling:
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 5) # find 4 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
require(data.table) #fore IDate
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")


#sentiment Analysis with tidy:
library(tidytext)
library(stringr)
library(dplyr)
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets.df %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+@#|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tidy_tweets$word <- gsub("#","",  tidy_tweets$word)
tidy_tweets$word <- gsub("@","",  tidy_tweets$word)

#another way: 
#detect stop words:

library(tidytext)
stop_words
my_stop_words <- tibble(
  word = c(
    "https",
    "t.co",
    "rt",
    "amp",
    "rstats",
    "gt"
  ),
  lexicon = "twitter"
)

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)

suppressWarnings({
  no_numbers <- tidy_tweets %>%
    filter(is.na(as.numeric(word)))
})

no_stop_words <- no_numbers %>%
  anti_join(all_stop_words, by = "word")

tibble(
  total_words = nrow(tidy_tweets),
  after_cleanup = nrow(no_stop_words)
)

#get top words:

top_words <- no_stop_words %>%
  group_by(word) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

top_words

nrc_words <- no_stop_words %>%
  inner_join(get_sentiments("nrc"), by = "word")

nrc_words 

#arrange emotions:
nrc_words %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))

nrc_words %>%
  group_by(id) %>%
  tally %>%
  ungroup %>%
  count %>%
  pull

#Visulaize:

library(ggjoy)

ggplot(nrc_words) +
  geom_joy(aes(
    x = created,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Twitter #Egypt + #WorldCup sentiment analysis",
       x = "Tweet Date",
       y = "Sentiment") + 
  scale_fill_discrete(guide=FALSE)

#Get a dictionary for sentiments
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)
nrc

#Sentiment analysis
sources <- tidy_tweets %>%
  group_by(statusSource) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, word)

#Sentiment by words
tidysentiment <- tidy_tweets %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id, created, statusSource, word) %>%
  ungroup()



library(scales)
library(lubridate)
tidysentiment %>%
  count(sentiment, time = hour(with_tz(created, "EST"))) %>%
  mutate(counts = nn ) %>%
  ggplot(aes(time, counts, color = sentiment)) +
  geom_line() +
  labs(x = "Hour of day (EST)",
       y = "intensity of emotion",
       color = "")

library(scales)
library(lubridate)
tidysentiment %>%
  count(sentiment, time = created) %>%
  mutate(counts = nn ) %>%
  ggplot(aes(time, counts, color = sentiment)) +
  geom_line() +
  labs(x = "time",
       y = "intensity of emotion",
       color = "")


