library(tidytext)
library(tidyverse)
library(quanteda)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

#Graphing unigrams, bigrams
txt <- data.frame(txt = train$text,
                      stringsAsFactors = FALSE)
unigram<-txt %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

unnest_tokens(tbl=txt, output=word,input=txt)

bigram<-txt %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) 

#graph
txt %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>%
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams of Medium iOS App Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: itunesr - iTunes App Store")

# read in training data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# combine training and testing data to produce unified corpus
tweets <- rbind(train[,-ncol(train)], test)

# get indices specifying start and stop of train/test data
# use to subset when training and testing classifiers
index_train <- c(1, nrow(train))
index_test <- c((nrow(train) + 1), (nrow(train) + nrow(test)))

# make tokens
tweet_corpus <- corpus(char_tolower(tweets$text))

# remove punctuation, urls, and symbols
# note, we are still able to keep hashtags and usernames
tweet_tokens <- tokens(tweet_corpus, 
                       remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE)

tweet_tokens_no_stop <- tokens_remove(tweet_tokens, pattern = stopwords("en"))

# get unigrams and bigrams
tweet_ngrams <- tokens_ngrams(x = tweet_tokens_no_stop, n = 1:2)

# make the design matrix (i.e., document term matrix)
tweet_dfmat <- dfm(tweet_ngrams)
dim(tweet_dfmat)

# split training set into training and test set
set.seed(123)
prop_train <- .2
train_ids <- sample(1:nrow(train), size = round(prop_train * nrow(train)),
                    replace = FALSE)
test_ids <- (index_train[1]:index_train[2])[-train_ids]
foldid <- sample(rep(1:10,length.out = length(train_ids))) # control which obs go to which folds in CV

# subset design matrix according to train-test
tweet_train <- tweet_dfmat[train_ids,]
tweet_test <- tweet_dfmat[test_ids,]

# subset known target values 
tweet_train_target <- train$target[train_ids]
tweet_test_target <- train$target[test_ids]

nrow(tweet_train) == length(tweet_train_target)
## [1] TRUE
nrow(tweet_test) == length(tweet_test_target)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

pc <- prcomp(tweet_train,
             center = TRUE,
             scale. = F)

plot(summary(pc)$importance[3,], xlab="PCs", ylab="% Var Explained", type="l")

summary(pc)$importance[3,1000]

trg <-tweet_train %*% pc$rotation[,1:1000]
tst <-tweet_test %*% pc$rotation[,1:1000]
"
#logistic regression
library(glmnet)

#cv.glmnet requires inputs to be data-matrices
tst<-data.matrix(tst)
tweet_test_target <-data.matrix(tweet_test_target)

fitLR <- cv.glmnet(tst, tweet_test_target, family = 'binomial')
summary(fitLR)

predLR <- predict(fitLR, tst, type = 'class')

head(predLR)

LRtable <- table(predLR,  tweet_test_target) 

accuracyLR <- 1- mean( predLR  != tweet_test_target) 
accuracyLR

recallLR = LRtable[2,2]/sum(LRtable[2,2], LRtable[1,2])
recallLR

precisionLR = LRtable[2,2]/sum(LRtable[2,2], LRtable[2,1])
precisionLR

specificityLR = LRtable[1,1]/sum(LRtable[1,1], LRtable[2,1])
specificityLR
"
#Trying Random Forest
#library(randomForest)

#fitRF = randomForest(tst, tweet_test_target, 
#                       importance=TRUE)
  
#predrftest = predict(fitRF, tst)
#rftable = table(predrftest, tweet_test_target)

#Trying Classification Tree
library(rpart)
tst<-as.data.frame(tst)
tweet_test_target <-as.data.frame(tweet_test_target)
class(tst)
class(tweet_test_target)
fitCT <- rpart(tweet_test_target~., 
             data = data.frame(tst, tweet_test_target), 
                               method = 'class')
predCTtest = predict(fitCT, tst)
CTtable = table(predCTtest, tweet_test_target)
CTtable