#'  Balanced Random Forest
#'
#' @description  This function utilizes the randomForest library to implement a balanced random forest model on the data.
#' This function partitions the data into a training set and test set, then fits a balanced random forest model using the training data.
#' Finally, it returns the fitted model and the confusion matrix of the test data.
#'
#' @param data A data frame that contains the data, with the response variable y being a binary variable of 0 and 1 with 1 being the minority class.
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
#' result <- BRF(data,ntree=100,prob=0.8)
#'
BRF <- function(data,ntree=100,prob=0.8){
  set.seed(123)
  trainIndex <- caret::createDataPartition(data$y, p = prob, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  testFeatures <- testData[, -which(names(testData) == "y")]
  nmin <- sum(trainData$y==1)

  brf_model <- randomForest::randomForest(
    y ~ .,
    data = trainData,
    importance = TRUE,
    ntree=ntree,
    strata=trainData$y,
    sampsize=c(nmin,nmin)
  )

  predictions <- stats::predict(brf_model, newdata = testFeatures)
  conf_matrix <- caret::confusionMatrix(predictions, testData$y)

  result <- list("model"=brf_model,"matrix"=conf_matrix)

}
