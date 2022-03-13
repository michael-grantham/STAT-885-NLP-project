library(glmnet)
#Trying LASSO to get keywords
#perform k-fold cross-validation to find optimal lambda value
tweet_train = data.matrix(tweet_train)
tweet_train_target = data.matrix(tweet_train_target)
cv_model <- cv.glmnet(tweet_train, tweet_train_target, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model = glmnet(tweet_train, tweet_train_target, alpha = 1, lambda = best_lambda)
lasso.coef = coef(best_model)[-1,]

lasso.coef = as.data.frame(as.matrix(lasso.coef))
indices = which(!lasso.coef == 0)
keywords = rownames(lasso.coef)[indices]
keywords

tweet_designmatrix_keywords_only = tweet_dfmat[,keywords]
tweet_designmatrix_keywords_only
