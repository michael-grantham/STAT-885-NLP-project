library(tidytext)
library(tidyverse)
library(quanteda)

#Read in training data
train=read.csv("train.csv")
test=read.csv("test.csv")

#Graphing unigrams, bigrams
txt = data.frame(txt = train$text,
                 stringsAsFactors = FALSE)
#Graph Unigrams
txt %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>%
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Unigrams of Kaggle Twitter contest")

#Graph Bigrams
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
  labs(title = "Top Bigrams of Kaggle Twitter contest")

#Make unigram and bigram objects
unigram=txt %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

bigram=txt %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE)

############
#### This code from David
############
# combine training and testing data to produce unified corpus
tweets = rbind(train[,-ncol(train)], test)

# get indices specifying start and stop of train/test data
# use to subset when training and testing classifiers
index_train = c(1, nrow(train))
index_test = c((nrow(train) + 1), (nrow(train) + nrow(test)))

# make tokens
tweet_corpus = corpus(char_tolower(tweets$text))

# remove punctuation, urls, and symbols
# note, we are still able to keep hashtags and usernames
tweet_tokens = tokens(tweet_corpus, 
                      remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE)

tweet_tokens_no_stop = tokens_remove(tweet_tokens, pattern = stopwords("en"))

# get unigrams and bigrams
tweet_ngrams = tokens_ngrams(x = tweet_tokens_no_stop, n = 1:2)

# make the design matrix (i.e., document term matrix)
tweet_dfmat = dfm(tweet_ngrams)
dim(tweet_dfmat)

# split training set into training and test set
set.seed(123)
prop_train = .2
train_ids = sample(1:nrow(train), size = round(prop_train * nrow(train)),
                   replace = FALSE)
test_ids = (index_train[1]:index_train[2])[-train_ids]
foldid = sample(rep(1:10,length.out = length(train_ids))) # control which obs go to which folds in CV

# subset design matrix according to train-test
tweet_train = tweet_dfmat[train_ids,]
tweet_test = tweet_dfmat[test_ids,]

# subset known target values 
tweet_train_target = train$target[train_ids]
tweet_test_target = train$target[test_ids]
############
#### End code from David
############



# Do PCA Analysis
# set parallel backend for performance boost
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

pc = prcomp(tweet_train,
            center = TRUE,
            scale. = F) 
#Scale set to F b/c there are probably columns with no entries in the sample, meaning variance is 0, which messes with PCA

#Visually see how many PC's are required
plot(summary(pc)$importance[3,], xlab="PCs", ylab="% Var Explained", type="l")

#See how good 1000 PC's will do
summary(pc)$importance[3,1000]

trg =tweet_train %*% pc$rotation[,1:1000]
tst =tweet_test %*% pc$rotation[,1:1000]

#logistic regression
library(glmnet)

#cv.glmnet requires inputs to be data-matrices
tst=data.matrix(tst)
tweet_test_target =data.matrix(tweet_test_target)

fitLR = cv.glmnet(trg, tweet_train_target, family = 'binomial')
summary(fitLR)

predLR = predict(fitLR, tst, type = 'class')

LRtable = table(predLR,  tweet_test_target) 
LRtable

accuracyLR = 1- mean( predLR  != tweet_test_target) 
accuracyLR

recallLR = LRtable[2,2]/sum(LRtable[2,2], LRtable[1,2])
recallLR

precisionLR = LRtable[2,2]/sum(LRtable[2,2], LRtable[2,1])
precisionLR

specificityLR = LRtable[1,1]/sum(LRtable[1,1], LRtable[2,1])
specificityLR

F1LR=2*precisionLR*recallLR(precisionLR+recallLR)
F1LR