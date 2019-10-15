load("C:\\Users\\hyunho\\Shiny\\study\\3-1\\univ_class\\MachineLearning\\final_project\\homeworkdata.RData")
#head(wordmatrix)
#head(we_data)
# change to dataframe
word <- data.frame(wordmatrix)
#wordmatrix <- data.frame(wordmatrix)
#ncol(word)


## Random Forest
library(randomForest)
MB <- randomForest(cat ~., data=word,importance=T, na.action=na.omit)
print(MB)
varImpPlot(MB)
#i = sample(1:nrow(word), round(nrow(word)*0.7))
#word.train = word[i,]
#word.test = word[-i,]
#rf.word <- randomForest(cat ~. , data = word.train, ntree=100, mtry=5, importance=T, na.action=na.omit)

pred.rf.word <- predict(MB, newdata=word)
tab=table(word$cat, pred.rf.word, dnn=c("Actual", "Predicted"))
tab
error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

# accuracy
(1-error_rate)*100

## Naivebayes
NB = naiveBayes(cat ~., data=word)
NB

NBpred =predict(NB,word)
NBpred
tab=table(word$cat, NBpred, dnn=c("Actual", "Predicted"))
print(tab)

error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

# accuracy
(1-error_rate)*100

# Here!!
## SVM
library(e1071)

#i = sample(1:nrow(word), round(nrow(word)*0.7))
#word.train = word[i,]
#word.test = word[-i,]

svm.model = svm(cat ~ ., data=word, type='C-classification',
                kernel='linear', cost=10000, scale=FALSE)
svm.model

#svm.model <- svm(cat ~., data=word, cost=100, gamma =1)
svm_pred_train <- predict(svm.model, word)
tab=table(word$cat, svm_pred_train)
tab
error_rate = 1-sum(diag(tab)/sum(tab))
# accuracy
(1-error_rate)*100

## SVM -2
library(e1071)

svm.model = svm(cat ~ ., data=word,cost=1000, gamma =100, scale=FALSE)
svm.model

svm_pred_train <- predict(svm.model, word)
tab=table(word$cat, svm_pred_train)
tab
error_rate = 1-sum(diag(tab)/sum(tab))
# accuracy
(1-error_rate)*100

############### k-fold cross validation - decision tree
library(caret)

folds <- createFolds(wordmatrix$cat, k = 100)

forest.results <- lapply(folds, function(x) {
  forest.train <- word[x,]
  forest.test <- word[-x,]
  forest.model <- randomForest(cat~., data = forest.train)
  forest.predict <- predict(forest.model, forest.test)
  forest.actual <- forest.test$cat
  forest.predicval <- confusionMatrix(forest.predict, forest.test$cat)$overall[1]
  return (forest.predicval)
})
mean(unlist(forest.results))

## k-fold svm linear
library(caret)

folds <- createFolds(wordmatrix$cat, k = 2)

svm.results <- lapply(folds, function(x) {
  svm.train <- word[x,]
  svm.test <- word[-x,]
  svm.model = svm(cat ~ ., data=svm.train,kernel='linear',scale=FALSE)
  svm.predict <- predict(svm.model, svm.test)
  svm.actual <- svm.test$cat
  svm.predicval <- confusionMatrix(svm.predict, svm.test$cat)$overall[1]
  return (svm.predicval)
})
mean(unlist(svm.results))

## k-fold svm radial
library(caret)

folds <- createFolds(wordmatrix$cat, k = 2)

svm.results <- lapply(folds, function(x) {
  svm.train <- word[x,]
  svm.test <- word[-x,]
  svm.model = svm(cat ~ ., data=svm.train,kernel='radial',scale=FALSE)
  svm.predict <- predict(svm.model, svm.test)
  svm.actual <- svm.test$cat
  svm.predicval <- confusionMatrix(svm.predict, svm.test$cat)$overall[1]
  return (svm.predicval)
})
mean(unlist(svm.results))

##########
#train_control <- trainControl(method="repeatedcv",number=10,savePredictions = TRUE)
#svm_pred_train=data.frame(svm_pred_train)
#pred.rf.word<-data.frame(pred.rf.word)
#validation_model <- train(word$cat~.,data=pred.rf.word,trControl=train_control,method="nb")

#validation_predict <- predict(validtion_model,word)
#tab=table(word$cat, validation_predict)
#tab
#error_rate = 1-sum(diag(tab)/sum(tab))
# accuracy
#(1-error_rate)*100
###############

############################ Neural Network
# train whole data
library("neuralnet")
set.seed(2019)

i = sample(1:nrow(word), round(nrow(word)*0.7))
word.train = word[i,]
word.test = word[-i,]

# calculate whole data
fit <- neuralnet(cat ~ ., data=word)
pred.fit <- compute(fit,word)
head(pred.fit)
neural_result=pred.fit$net.result[,2]


neural_result <- ifelse(pred.fit$net.result[,2]>0.5, 1, 0)

neural_result<-as.factor(neural_result)
str(neural_result)
head(neural_result)
head(word$cat)

tab=table(word$cat, neural_result, dnn=c("Actual", "Predicted"))
tab
error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

# accuracy
(1-error_rate)*100

## nn trian-train

# make the model
fit <- neuralnet(cat ~ ., data=word.train)
pred.fit <- compute(fit,word.train)
#head(pred.fit)
neural_result=pred.fit$net.result[,2]

neural_result <- ifelse(pred.fit$net.result[,2]>0.5, 1, 0)

neural_result<-as.factor(neural_result)
str(neural_result)
head(neural_result)
head(word$cat)

tab=table(word.train$cat, neural_result, dnn=c("Actual", "Predicted"))
tab
error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

# accuracy
(1-error_rate)*100


## nn trian-test

# make the model
fit <- neuralnet(cat ~ ., data=word.train)
pred.fit <- compute(fit,word.test)
#head(pred.fit)
neural_result=pred.fit$net.result[,2]

neural_result <- ifelse(pred.fit$net.result[,2]>0.5, 1, 0)

neural_result<-as.factor(neural_result)

tab=table(word.test$cat, neural_result, dnn=c("Actual", "Predicted"))
tab
error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

# accuracy
(1-error_rate)*100


############### glove
library(data.table)
library(text2vec)
library(tm)

tokens <- strsplit(we_data, split = " ", fixed = T)
vocab <- create_vocabulary(itoken(tokens), ngram = c(1,1))
vocab <- prune_vocabulary(vocab, term_count_min = 5)

iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(iter, vectorizer, skip_grams_window = 5)


glove = GlobalVectors$new(word_vectors_size = 50, vocabulary=vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter=20)

dim(wv_main)

wv_context = glove$components
word_vectors = wv_main + t(wv_context)



good.walks = word_vectors["사업", , drop = FALSE] + 
  word_vectors["대통령", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = good.walks, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
