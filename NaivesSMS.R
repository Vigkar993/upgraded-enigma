setwd("E:/R")
sms_raw<-read.csv('sms_spam.csv')
str(sms_raw)
str(sms_raw$type)
table(sms_raw$type)
install.packages('tm')
library(tm)
sms_corpus<-Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:5])
#convert to lower case 
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
#remove filler words , we use stop words()
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
#remove punctiations
corpus_clean<-tm_map(corpus_clean,removePunctuation)
#remove whitespaces and have onlu one space in between
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
inspect(corpus_clean[1:3])
inspect(sms_corpus[1:3])
#print(vignette("tm"))
#tokenization
sms_dtm<-DocumentTermMatrix(corpus_clean)
#data preparation-creating training and test dataset
sms_raw_train<-sms_raw[1:4169,]
sms_raw_test<-sms_raw[4170:5559,]
#document-Term Matriz
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
#corpus
sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]
#to dtermine the percentage of spam and ham
prop.table(table(sms_raw_train$type))
?prop.table()
prop.table(table(sms_raw_test$type))
#visuaizing text data
install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_train,min.freq = 40,random.order = FALSE)
#Subsetting spam data
spam<-subset(sms_raw_train,type=="spam")
ham<-subset(sms_raw_train,type=='ham')
#visualizing only top 40 words
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))
#data preparation-creating indiacator feature for frequent words atleast 5 times
findFreqTerms(sms_dtm_train,5)
#to save list of frequent terms for later use, we'll use Dictionary() function
sms_dict<-c(findFreqTerms(sms_dtm_train,5))
?Dictionary()
sms_dict
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))

convert_counts<-function(x) {
  x <- ifelse (x > 0,1,0) 
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}  

sms_train<-apply(sms_train, MARGIN =2, convert_counts)
sms_test<-apply(sms_test,MARGIN=2,convert_counts) 

#TRAINING THE DATASET
#statistical pacage
install.packages("e1071")
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)
sms_test_pred<-predict(sms_classifier,sms_test)
install.packages('gmodels')
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,
           prop.chisq = FALSE,prop.t = FALSE,
           dnn = c('predicted','actual'))
#Improving the model by changing lapalace values

sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq = FALSE,
           prop.r = FALSE,dnn = c('predicted','actual'))
