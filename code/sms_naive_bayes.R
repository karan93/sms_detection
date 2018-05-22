library("tm")
library("SnowballC")
library("e1071")
library("gmodels")
data = read.csv("sms_spam.csv",stringsAsFactors = FALSE)
str(data)

#cleaning
data$type <- factor(data$type) 
sms_corpus = VCorpus(VectorSource(data$text))
getTransformations()
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)

#creating document term matrix
sms_dtm = DocumentTermMatrix(sms_corpus_clean)

#splittting of data
sms_train <- sms_dtm[1:4460,]
sms_test <- sms_dtm[4461:5574,]
sms_train_labels <- data[1:4460,]$type
sms_test_labels <- data[4461:5574,]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

sms_freq_words <- findFreqTerms(sms_dtm,5)

sms_dtm_freq_train <- sms_train[,sms_freq_words]
sms_dtm_freq_test <- sms_test[,sms_freq_words]

convertCounts <- function(x){
  x<- ifelse(x>0,"Yes","No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convertCounts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convertCounts)
sms_classifier <- naiveBayes(sms_train,sms_train_labels,laplace = 0)
sms_pred <- predict(sms_classifier,sms_test)

CrossTable(x=sms_test_labels,y = sms_pred,prop.chisq = FALSE , prop.t = FALSE)
