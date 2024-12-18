#' Weighted Random Forest
#'
#' @description This function utilizes the randomForest library to implement a weighted random forest model on the data.
#' This function fits a weighted random forest model using k-fold CV on the training data.
#' Finally, it returns the fitted model and the confusion matrix of the test data.
#'
#' @param data A data frame that contains the data, with the response variable y being a binary variable of 0 and 1 with 1 being the minority class.
#' @param weights A vector that contains the weights that we want to test on. The function uses k-fold CV to find best weight.
#' @param k Indicates the number of folds to create during cross validation process. The default is set to 5.
#' @param ntree The number of trees used in the balanced random forest model. The default is set to 100.
#' @param prob The ratio of training set to the whole data. The default is set to 0.8
#'
#' @return A vector of length 2 that contains the fitted balanced random forest model and the confusion matrix using the test data.
#'
#' @examples
#' # Example Usage
#' data <- caret::twoClassSim(1000, intercept = -15, linearVars = 15, noiseVars = 5)
#' names(data)[names(data)=="Class"]="y"
#' data$y = as.factor(ifelse(data$y=="Class1",0,1))
#'
#' result <- WRF(data,weights=c(1000,10000,100000),ntree=100,prob=0.8)
#'
#'
WRF <- function(data,weights,k=5,ntree=100,prob=0.8){
  set.seed(123)
  trainIndex <- caret::createDataPartition(data$y, p = prob, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  testFeatures <- testData[, -which(names(testData) == "y")]

  folds <- caret::createFolds(trainData$y, k = k)
  bestwt <- 1
  best_bal_acc <- 0
  bal_acc_weight=rep(0,length(weights))
  count <- 1
  weights<-as.vector(weights)

  for(wt in weights){
    bal_acc=rep(0,k)
    for (fold in folds){
      train_fold <- trainData[-fold, ]
      test_fold <- trainData[fold, ]

      wrf_model <- randomForest::randomForest(
        y ~ .,
        data = train_fold,
        importance = TRUE,
        ntree=ntree,
        classwt=c(wt,1)
      )

      predictions <- stats::predict(wrf_model, test_fold)
      bal_acc[count] <- (sum(predictions == 1 & test_fold$y==1)/sum(test_fold$y==1)+ sum(predictions == 0 & test_fold$y==0)/sum(test_fold$y==0))/2
    }
    mean_bal_acc <- mean(bal_acc)

    if(bestwt==1 || mean_bal_acc>best_bal_acc){
      bestwt <- wt
      best_bal_acc <-mean_bal_acc
    }
    count <- count+1
  }
  wrf_model <- randomForest::randomForest(
    y ~ .,
    data = trainData,
    importance = TRUE,
    ntree=ntree,
    classwt=c(bestwt,1)
  )

  predictions <- stats::predict(wrf_model, newdata = testFeatures)
  conf_matrix <- caret::confusionMatrix(predictions, testData$y)

  result <- list("model"=wrf_model,"matrix"=conf_matrix)


}





