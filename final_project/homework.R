load("C:\\Users\\hyunho\\Shiny\\study\\3-1\\univ_class\\MachineLearning\\final_project\\homeworkdata.RData")
#head(wordmatrix)
#head(we_data)
# change to dataframe
word <- data.frame(wordmatrix)
#ncol(word)

# go to svm model!

library(randomForest)
MB <- randomForest(cat ~., data=word,importance=T, na.action=na.omit)
print(MB)
varImpPlot(MB)
i = sample(1:nrow(word), round(nrow(word)*0.7))
word.train = word[i,]
word.test = word[-i,]
rf.word <- randomForest(cat ~. , data = word.train, ntree=100, mtry=5, importance=T, na.action=na.omit)

pred.rf.word <- predict(rf.word, newdata=word.test)
tab=table(word.test$cat, pred.rf.word, dnn=c("Actual", "Predicted"))
tab
error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

## Naivebayes
NB = naiveBayes(cat ~., data=word)
NB

NBpred =predict(NB,word)
NBpred
tab=table(word$cat, NBpred, dnn=c("Actual", "Predicted"))
print(tab)

error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

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


