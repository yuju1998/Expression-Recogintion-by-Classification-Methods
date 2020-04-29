# read csv

library(data.table)
setwd("C:/Users/Ruby/Desktop/專題")
data <- fread("pcdata.csv", sep = ',', header = T)
jaffe <- fread("pcdata_jaffe.csv", sep = ',', header = T)
student <- fread("pcdata_student.csv", sep = ',', header = T)
acc <- fread("Accuracy.csv", sep = ',', header = T)

data$expression <- as.factor(data$expression)
levels(data$expression)[1:5] <- c("angry", "happy", "sad", "disgust", "surprise")
jaffe$expression <- as.factor(jaffe$expression)
levels(jaffe$expression)[1:5] <- c("angry", "happy", "sad", "disgust", "surprise")
student$expression <- as.factor(student$expression)
levels(student$expression)[1:5] <- c("angry", "happy", "sad", "disgust", "surprise")


# other stuffs
library(e1071)
library(caret)
library(randomForest)
library(RColorBrewer)
mycol <- brewer.pal(11, "RdBu")


# Random Forest

forest <- function(x){ # x is one of the three datasets: data.pca, jaffe, student

  set.seed(123)
  train.ind <- createDataPartition(x$expression, p=0.8, list = F)
  
  train <- x[train.ind, ]
  test <- x[-train.ind, ]
  
  # 10-fold cross validation to do parameters tuning
  set.seed(123)
  rf.tune <- tune(randomForest, 
                  expression ~., 
                  data = train, 
                  ranges = list(mtry=1:5, 
                                ntree=c(400, 500, 600))
                  )
  
  RFM <- randomForest(
    expression ~ . ,
    data = train,
    scale = T,
    importance = T,
    proximity = T,
    ntree = rf.tune$best.parameters$ntree,
    mtry = rf.tune$best.parameters$mtry
  )
  
  # plot of error rate in mtry=1:5, find the best mtry wth smallest error rate
  x11()
  plot(rf.tune, main = 'Cross Validation Error Rate - Random Forest')

  ipt <- round(importance(RFM), 2)
  ypred <- predict(RFM, test)
  confuse <-confusionMatrix(ypred, test$expression, positive = NULL)
    #relative.col(confusionMatrix(ypred, test$expression, positive = "Yes"))
  
  return(list(model_results = RFM, 
           confusion_matrix = confuse, 
           importance = ipt))
  
}

forest(data)  #mtry=4
forest(jaffe)           #mtry=1
forest(student)         #mtry=5


# SVM


supvector <- function(x){
  
  set.seed(123)
  train.ind <- createDataPartition(x$expression, p=0.8, list = F)
  
  train <- x[train.ind, ]
  test <- x[-train.ind, ]
  
  # find best parameters cost/gamma
  set.seed(123)
  svm.tune <- tune(
    svm,
    expression ~ .,
    data = train,
    kernel = "radial",
    # RBF kernel function
    range = list(cost = c(0.1, 0.5, 1:10), 
                 gamma = seq(0, 3, by=.1))# 調參數的最主要一行
  )
  
  x11()
  plot(svm.tune)
   
  # use best parameter do svm
  svmfit <- svm(
    expression ~ .,
    data = train,
    scale = T,
    cost = svm.tune$best.parameters$cost,
    gamma = svm.tune$best.parameters$gamma
  )
  
  ypred <- predict(svmfit, test)
  confuse <- confusionMatrix(ypred, test$expression, positive = "Yes")
  
  return(list(model_results=summary(svmfit), 
              confusion_matrix=confuse))
  
}


supvector(data)
supvector(jaffe)
supvector(student)


# KNN

library(kknn)

#選擇k的折線圖
x11(height = 15, width = 15)
ggplot(acc, aes(x = k, y = data, colour = group)) + 
  geom_line(linetype = "solid",size=1) +
  scale_x_continuous(breaks=c(3:10), labels = c(3:10))+
  labs(title = 'Cross Validation Error Rate - KNN', y= 'error') +
  theme(plot.title = element_text(size = 15, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size=12, color = 'black'),
        axis.title = element_text(size=15, color = 'black'),
        legend.text = element_text(size=15, color = 'black'),
        legend.title = element_text(size=15, color = 'black'),
        panel.grid.major=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'))+
  geom_point(shape = 20, size = 3) 


knear <- function(x){
  
  set.seed(123)
  train.ind <- createDataPartition(x$expression, p=0.8, list = F)
  
  train <- x[train.ind, ]
  test <- x[-train.ind, ]
  
  # use best parameter do knn
  bestGrid <- expand.grid(kmax = 3, 
                          distance = 2, 
                          kernel = 'triweight')
  
  set.seed(123)
  kknnfit <- train(expression ~., 
                   data = train, 
                   method = 'kknn', 
                   preProcess = c('center', 'scale'), 
                   tuneGrid = bestGrid)
  
  
  ypred <- predict(kknnfit, test)
  confuse <- confusionMatrix(ypred, test$expression, positive = "Yes")
  
  return(list(model_results = kknnfit, 
              confusion_matrix = confuse))
  
}

knear(data)
knear(jaffe)
knear(student)
