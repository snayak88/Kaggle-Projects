setwd("D:\\E books\\Machine Learning Resources\\Kaggle\\sms-spam-collection-dataset")
sms_raw <- read.csv("spam.csv")
str(sms_raw)

############Create a Corpus - A collection of text documents###################
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$v2))
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])

##################Clean up the data#############

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #####Convert to lower case
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)#######Remove Numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())#####Remove Stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

################Word Stemming##################
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
################################################

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)#############Strip whitespace
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

###############Data Preparation################

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5572,]
sms_train_labels <- sms_raw[1:4169,]$v1
sms_test_labels <- sms_raw[4170:5572,]$v1
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

##############Visualisation with wordcloud#####
library(wordcloud)
spam <- subset(sms_raw, v1 == "spam")
ham <- subset(sms_raw, v1 == "ham")

wordcloud(spam$v2, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$v2, max.words = 40, scale = c(3, 0.5))

###########Creating indicator features for frequent words#############
library(e1071)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)

#############View Results##################
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

############Improving Performance using laplace estimator#########
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

