library(SDMTools) # (For confusion matrix using confusion.matrix()
library(caret)    # For svm, and createDataPartition function as in problem
library(mlbench)
library(ROCR)     # For getting ROC curve for each model
library(e1071)    # For naiveBayes() function to create appropriate model
require(rpart) 

# compute, accuracy and recall
compute.function = function(conf.matrix){
  
  tp = conf.matrix[2,2]
  fp =  conf.matrix[2,1]
  fn = conf.matrix[1,2]
  tn = conf.matrix[1,1]
  # printing this way since the default print is undetailed
  cat("\nObs\\Pred\tT\tF\n") 
  cat(paste0("True\t\t",tp,"\t",fn,"\n"))
  cat(paste0("False\t\t",fp,"\t",tn,"\n"))  
  cat("\n")
  acc = (tp+tn)/(tp+tn+fp+fn)
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  print(paste0("Accuracy = ",acc))
  print(paste0("Precision = ",precision))
  print(paste0("Recall = ",recall))  
}

# common function for svm and rpart
predict.function = function(arg){
  
  loc = "Customer-Churn-Data.csv" 
  tree <- read.csv(file=loc,header=TRUE,sep=",") 
  trainIndex <- createDataPartition(tree$churn, p=0.9, list=FALSE)
  drops = c('area_code')
  tree = tree[,!(names(tree) %in% drops)]
  
  tree1 <- sapply(tree, as.numeric)
  data_train <- tree1[trainIndex,]
  data_test <- tree1[-trainIndex,]
  
  m <- arg(churn~.,as.data.frame(data_train))
  newvariable <- predict(m,as.data.frame(data_test))
  predr <- prediction(newvariable,as.data.frame(data_test)$churn)
  perf <- performance(predr,"tpr","fpr")
  
  plot(perf,col="black",lty=3,lwd=3) # create ROC
  auc = performance(predr,"auc")
  area = round(as.numeric(auc@y.values),4)

    print(paste0("Area under curve : ",area))
  train = as.data.frame(data_test)$churn;
  train = train - 1;
  conf.matrix = confusion.matrix((train), newvariable-1)
  compute.function(conf.matrix) # compite acc, prec, recall
}


bayesj = function(){
  
  arg = naiveBayes;
  loc = "Customer-Churn-Data.csv" 
  tree <- read.csv(file=loc,header=TRUE,sep=",") 
  trainIndex <- createDataPartition(tree$churn, p=0.9, list=FALSE)
  drops = c('area_code')
  tree = tree[,!(names(tree) %in% drops)]
  
  #tree1 <- sapply(tree, as.numeric)
  tree1 <- as.matrix(tree)
  data_train <- tree[trainIndex,]
  data_test <- tree[-trainIndex,]
  
  m <- arg(churn~.,data_train)
  newvariable <- predict(m,data_test)
  newvariable <- sapply(newvariable,as.numeric)
  tr_churn <- sapply(data_test$churn,as.numeric)
  predr <- prediction(newvariable,tr_churn)
  perf <- performance(predr,"tpr","fpr")
  
  plot(perf,col="black",lty=3,lwd=3) # plot ROC
  auc = performance(predr,"auc")
  area = round(as.numeric(auc@y.values),4)
  print(paste0("Area under curve : ",area))

  tr_churn = tr_churn - 1;
  table <- table((tr_churn), newvariable-1) # actual x prediction
  conf.matrix = confusion.matrix((tr_churn), newvariable-1)
  compute.function(conf.matrix) # compute acc, prec, recall
}

## -- calling functions
## NOTE : Different outputs will be obtained for different runs
cat("\n\n+--- SVM\n")
predict.function(svm)
cat("\n\n+--- Decision tree classifier using rpart\n")
predict.function(rpart)
cat("\n\n+--- Naive Bayes classifier\n")
bayesj()
cat("\n")
print("By comparing the area under curve, we can arrive at the best method")