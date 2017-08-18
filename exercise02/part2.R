#Q2 
library(dplyr)
library(tm)
#Get train and test data
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

file_list_train = Sys.glob('data/ReutersC50/C50train/*/*.txt')
file_list_test = Sys.glob('data/ReutersC50/C50test/*/*.txt')
all_train = lapply(file_list_train, readerPlain) 
all_test = lapply(file_list_test, readerPlain) 

# Some more concise document names via basic string manipulation
names(all_train) = file_list_train
names(all_train) = substring(names(all_train),first=26)
names(all_train) = t(data.frame(strsplit(names(all_train),'/')))[,1]

names(all_test) = file_list_test
names(all_test) = substring(names(all_test),first=25)
names(all_test) = t(data.frame(strsplit(names(all_test),'/')))[,1]

## once you have documents in a vector, you 
## create a text mining 'corpus' with: 
my_documents_train = Corpus(VectorSource(all_train))
my_documents_test = Corpus(VectorSource(all_test))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_documents_train = tm_map(my_documents_train, content_transformer(tolower)) # make everything lowercase
my_documents_train = tm_map(my_documents_train, content_transformer(removeNumbers)) # remove numbers
my_documents_train = tm_map(my_documents_train, content_transformer(removePunctuation)) # remove punctuation
my_documents_train = tm_map(my_documents_train, content_transformer(stripWhitespace)) ## remove excess white-space

## Remove stopwords.  Always be careful with this: one man's trash is another one's treasure.
my_documents_train = tm_map(my_documents_train, content_transformer(removeWords), stopwords("en"))


my_documents_test = tm_map(my_documents_test, content_transformer(tolower)) # make everything lowercase
my_documents_test = tm_map(my_documents_test, content_transformer(removeNumbers)) # remove numbers
my_documents_test  = tm_map(my_documents_test, content_transformer(removePunctuation)) # remove punctuation
my_documents_test  = tm_map(my_documents_test, content_transformer(stripWhitespace)) ## remove excess white-space


my_documents_test  = tm_map(my_documents_test , content_transformer(removeWords), stopwords("en"))


DTM_tfidf_train = DocumentTermMatrix(my_documents_train, control = list(weighting = weightTfIdf))
DTM_tfidf_train = removeSparseTerms(DTM_tfidf_train, 0.95)

DTM_tfidf_test = DocumentTermMatrix(my_documents_test, control = list(weighting = weightTfIdf))
DTM_tfidf_test = removeSparseTerms(DTM_tfidf_test, 0.95)

term_freq_train = as.data.frame(as.matrix(DTM_tfidf_train))
names(term_freq_train) = paste(names(term_freq_train),'.w',sep='')
author_train = factor(names(all_train))

term_freq_test = as.data.frame(as.matrix(DTM_tfidf_test))
names(term_freq_test) = paste(names(term_freq_test),'.w',sep='')
author_test = factor(names(all_test))

intersection = intersect(names(term_freq_train),names(term_freq_test))
term_freq_train = term_freq_train[,intersection]
term_freq_test = term_freq_test[,intersection]


X_train = term_freq_train
X_train$author = author_train
X_test = term_freq_test
X_test$author = author_test

#Model 1 Naive Bayes
library(naivebayes)
nb.listing = naive_bayes(author ~ ., data = X_train)
nb.pred = data.frame(predict(nb.listing,X_test))
compare_nb = data.frame(cbind(nb.pred,X_test$author))
compare_nb$correct = compare_nb$predict.nb.listing..X_test. == compare_nb$X_test.author
mean(compare_nb$correct)




#Model 2 RF
library(randomForest)
rf.listing = randomForest(author ~ ., data = X_train,
                          distribution = 'multinomial',
                          n.trees=500)
rf.pred = data.frame(predict(rf.listing,newdata = X_test))
compare_rf = data.frame(cbind(rf.pred,X_test$author))
compare_rf$correct = compare_rf$predict.rf.listing..newdata...X_test. == compare_rf$X_test.author
mean(compare_rf$correct)

#RESULT
authors = unique(names(all_train))
n_authors = length(authors)

#NB
correctness_nb = data.frame(matrix(ncol = 2, nrow = n_authors))
colnames(correctness_nb) = c('author', 'accuracy_nb')
correctness_nb$author = authors

for (i in 1:n_authors){
  set = subset(compare_nb,compare_nb$X_test.author == authors[i])
  correctness_nb[i,2] = mean(set$correct)
}

result_nb = correctness_nb[order(correctness_nb$accuracy_nb,decreasing = TRUE),]
result_nb[1:10,]
result_nb[41:50,]


#RF
correctness_rf = data.frame(matrix(ncol = 2, nrow = n_authors))
colnames(correctness_rf) = c('author', 'accuracy_rf')
correctness_rf$author = authors

for (i in 1:n_authors){
  set = subset(compare_rf,compare_rf$X_test.author == authors[i])
  correctness_rf[i,2] = mean(set$correct)
}

result_rf = correctness_rf[order(correctness_rf$accuracy_rf,decreasing = TRUE),]
result_rf[1:10,]
result_rf[41:50,]



result = merge(correctness_nb,correctness_rf)
result
