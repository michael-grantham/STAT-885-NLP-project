library(tidytext)
library(tidyverse)
library(superml)


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

class(txt)
head(txt, 3)

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

#

cfv <- CountVectorizer$new(remove_stopwords = TRUE,
                           ngram_range = c(1, 2))
cf_mat <- cfv$fit_transform(txt)



mtcars.pca <- prcomp(cf_mat, center = TRUE,scale. = TRUE)
