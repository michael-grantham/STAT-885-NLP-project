library(tidytext)
library(tidyverse)
library(quanteda)
library(MLmetrics)
library(glmnet)
library(caret)

rm(list=ls()) 

#Read in training data
train=read.csv("train.csv")
test=read.csv("test.csv")

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

# use tfidf weighting
tweet_dfmat_tfidf <- dfm_tfidf(tweet_dfmat)

# split training set into training and test set
set.seed(123)
prop_train <- 1
train_ids <- sample(index_train[1]:index_train[2], size = round(prop_train * nrow(train)),replace = FALSE)
test_ids <- (index_train[1]:index_train[2])[-train_ids]
foldid <- sample(rep(1:10,length.out = length(train_ids))) # control which obs go to which folds in CV

# subset design matrix according to train-test
tweet_train_tfidf <- tweet_dfmat_tfidf[train_ids,]
tweet_test_tfidf <- tweet_dfmat_tfidf[test_ids,]

# subset known target values 
tweet_train_target <- train$target[train_ids]
tweet_test_target <- train$target[test_ids]

nrow(tweet_train_tfidf) == length(tweet_train_target)

############
#### End code from David
############

trainX<-tweet_train_tfidf
trainY<-as.factor(tweet_train_target)

#########################################################################
#####################        10-fold CV         #########################
#########################################################################
library(MLmetrics)
library(glmnet)
library(caret)

# Define F1_Score from Mlmetrics package
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = lev[1])
  c(F1 = f1_val)
}

# Using caret to perform CV
cctrl1 <- trainControl(method="cv", number=10, returnResamp="all",
                       classProbs=F, summaryFunction=f1,
                       search="grid")


set.seed(849)
test_class_cv_model <- train(trainX, trainY, method = "glmnet", 
                             trControl = cctrl1,metric = "F1",
                             tuneGrid = expand.grid(alpha = seq(0.085,.09, by=.001),
                                                    lambda = seq(0.085,.09, by = 0.001)))

test_class_cv_model$results$F1SD
test_class_cv_model$results

saveRDS(test_class_cv_model, "trainoutput.rds")
F1SD<-readRDS("F1SD.rds")

# best parameter
test_class_cv_model$bestTune

# best coefficient
k<-as.data.frame(as.matrix(coef(test_class_cv_model$finalModel, test_class_cv_model$bestTune$lambda)))
k
dim(k)

# see biggest coefficients
coefnames<-as.data.frame(rownames(k))
coefs<-cbind(k,coefnames)
colnames(coefs)<-c("coef","label")
df<-coefs %>% mutate(magnitude=abs(coef)) %>% arrange(desc(magnitude)) 
df<-df[2:nrow(df),2:3]
head(df, 15)
dim(df)

length(unique(df$magnitude))

# produce predictions
testset<-data.matrix(tweet_dfmat_tfidf[index_test[1]:index_test[2],])
dim(testset)

fit<-glmnet(trainX, trainY, family="binomial",
            alpha=.087,
            lambda=.086)

saveRDS(fit, file="savedfit.rds")

# If fit is taking too long, just uncomment and read in the RDS:
fit <- readRDS("savedfit.rds")

outputs<-predict(fit, newx=testset,
                 s=.086, type="class")
head(outputs)

x<-test2$results$alpha
y<-test2$results$lambda
z<-as.matrix(test2$results$F1)
plotframe<-data.frame(alpha=x, lambda=y, F1=z)

plotframe %>% ggplot() + geom_tile(aes(x=alpha, y=lambda, fill=F1)) +
  geom_text(aes(x=alpha, y=lambda, label=round(F1, digits=4)))

plotframe<-data.frame(alpha=x, lambda=y, F1=z)

plotframe %>% ggplot() + geom_tile(aes(x=alpha, y=lambda, fill=F1)) 

test2 <- train(trainX, trainY, method = "glmnet", 
                             trControl = cctrl1,metric = "F1",
                             tuneGrid = expand.grid(alpha = seq(0,.1, by=.01),
                                                    lambda = seq(0,.1, by = 0.01)))

x<-test2$results$alpha
y<-test2$results$lambda
z<-as.matrix(test2$results$F1)




            
            