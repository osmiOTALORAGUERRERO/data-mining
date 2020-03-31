install.packages("tm")
install.packages("dplyr")
install.packages("rtweet")
install.packages("SnowballC")
# Alternatively, install just readr:
install.packages("readr")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/readr")

library(tm)
library(dplyr)
library(rtweet)
library(SnowballC)
library(ggplot2)

#Dataset downloaded from http://www.trumptwitterarchive.com/archive
#dataset import like data.frame with name tweets_Trump
tweets_Trump <- read.csv("~/Uniminuto Ing.Sistemas/SeventhSemester/data-mining/DonaldTrumpTweets/datacsv.csv", quote="", stringsAsFactors=FALSE)

head(tweets_Trump, 10)

sample_tweets <- head(tweets_Trump, 100)

# you can change this var for analysis 
target_tweets <- tweets_Trump # or tweets_Trump


# view caracteristics from dataframe
head(target_tweets)
str(target_tweets)
summary(target_tweets)


# -------- Frequencie without stopwords and steamwords analysis ---------

tweets_corpus_bad <- Corpus(VectorSource(target_tweets$text))
inspect(tweets_corpus_bad)
tdm_bad <- TermDocumentMatrix(tweets_corpus_bad)
tdm_matrix_bad <- as.matrix(tdm_bad)
Docs(tdm_bad)
nDocs(tdm_bad)
Terms(tdm_bad)
nTerms(tdm_bad)

# Frequencie of words in the tweets
freq_Words_bad <- data.frame(word = rownames(tdm_matrix_bad),
                        Freq = rowSums(tdm_matrix_bad),
                        row.names = NULL)
# order words frequencie
freq_Words_bad <- freq_Words_bad %>%
  arrange(desc(Freq))

findFreqTerms(tdm_bad, lowfreq = 1, highfreq = Inf)


# ---------- Frequencie without stopwords and steamwords analysis ----------

corpus_tweets <- Corpus(VectorSource(target_tweets$text))
# cleaning tweets 
corpus_tweets <- tm_map(corpus_tweets, content_transformer(tolower)) #text to lower case
# Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus_tweets <- tm_map(corpus_tweets, content_transformer(removeURL))
# remove punctuation
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus_tweets <- tm_map(corpus_tweets, content_transformer(removeNumPunct))
# remove stopwords
corpus_tweets <- tm_map(corpus_tweets, removeWords, tm::stopwords("en"))
# remove extra spaces
corpus_tweets <- tm_map(corpus_tweets, stripWhitespace)
# stemwords
corpus_tweets <- tm_map(corpus_tweets, stemDocument, language="english")

# count frequencie
tdm_ct <- TermDocumentMatrix(corpus_tweets)
# View caracteristics of courpus tweet
Docs(tdm_ct)
nDocs(tdm_ct)
Terms(tdm_ct)
nTerms(tdm_ct)

# frequencie
freq_terms_ct <- findFreqTerms(tdm_ct, lowfreq = 1, highfreq = Inf)

term_freq_ct <- rowSums(as.matrix(tdm_ct))
term_freq_ct <- subset(term_freq_ct, term_freq_ct >= 1)
df_freq_words <- data.frame(term = names(term_freq_ct),
                            freq = term_freq_ct)

df_freq_words <- df_freq_words %>% 
  arrange(desc(freq))

# Graphic frequencie
df_freq_words %>% 
  top_n(20) %>%
  ggplot(aes(x=reorder(term, freq), y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

df_freq_words %>%
  tail(20) %>%
  ggplot(aes(x=reorder(term, freq), y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))
 
