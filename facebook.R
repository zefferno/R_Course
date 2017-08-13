library(devtools)
library(tm)
library(wordcloud)
library(topicmodels)
library(stats)
library(sentimentr)
require (Rfacebook)

############
# Constants
############

# Facebook App ID
FB_APP_ID <- "PUT_APP_ID_HERE"

# Facebook Secret
FB_SECRET <- "PUT_SECRET_HERE"

# Group name to search and analyze
GROUP_NAME <- "PUT_GROUP_NAME"

# Unwanted words to be removed from dataset
UNWANTED_WORDS <- c(
  "http", 
  "https"
)

# Number of samples to fetch
NUM_OF_SAMPLES <- 200

############################
# Fetch data from Facebook
############################

# Request oauth token from Facebook
fb_oauth <- fbOAuth(
  app_id=FB_APP_ID, 
  app_secret=FB_SECRET,
  extended_permissions = FALSE
)

# Get n defined size samples of recent posts without reactions from page
data.df <- getPage(
  GROUP_NAME, 
  fb_oauth, 
  n=NUM_OF_SAMPLES, 
  feed=TRUE, 
  reactions=FALSE, 
  verbose=TRUE
)

#########################
# Build corpus
#########################

# Remove posts with empty message
data.df = data.df[complete.cases(data.df$message), ]
# Build vector from page posts
messages <- VectorSource(data.df$message)
# Build corpus from vector of messages
data_corpus <- Corpus(messages)
# Transform words to lower-case
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
# Remove English stopwords
data_corpus <- tm_map(data_corpus, removeWords, stopwords("english"))
# Remove specific words
data_corpus <- tm_map(data_corpus, removeWords, UNWANTED_WORDS)
# Remove whitespace
data_corpus <- tm_map(data_corpus, stripWhitespace)
# Remove numbers
data_corpus <- tm_map(data_corpus, removeNumbers)
# Remove punctuation
data_corpus <- tm_map(data_corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
# Stem - remove common word endings
data_corpus <- tm_map(data_corpus, stemDocument, language = "english")

#################################
# Build Term Document Martrix
#################################

tdm <- TermDocumentMatrix(
  data_corpus, 
  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
)

term.freq <- rowSums(as.matrix(tdm))
top.terms <- term.freq[order(term.freq, decreasing = T)][1:10]

#############################################
# Analyze discussion 
#############################################

# Most common words
barplot(top.terms, names.arg = names(top.terms))
wordcloud(words = names(term.freq), freq = term.freq, min.freq = 20, random.order = T)

# Answer for Q3.1: answer: senate, court, care, climate ...

# 3 most discussed topics
dtm <- DocumentTermMatrix(data_corpus)
# Estimate Latent Dirichlet Allocation (LDA) model
lda <- LDA(dtm, k = 3)
term <- terms(lda, 7)
term

# Answer for Q3.2: care, court, climate, senate

#############################################
# Analyze sentiment of recent post comments
#############################################
# Get the most recent post 1000 comments
post.df <- getPost(data.df$id[1], fb_oauth, n = 1000, likes = FALSE, comments = TRUE)

sentiments <- sentiment(post.df$comment$message, polarity_dt = lexicon::hash_sentiment_sentiword)
hist(sentiments$sentiment)
table(sign(sentiments$sentiment))
