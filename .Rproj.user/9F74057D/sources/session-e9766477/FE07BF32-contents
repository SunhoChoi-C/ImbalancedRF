#' Threshold Random Forest
#'
#' @param data A data frame that contains the data, with the response variable y being a binary variable of 0 and 1 with 1 being the minority class.
#' @param ntree The number of trees used in the balanced random forest model. The default is set to 3000.
#' @param prob The ratio of training set to the whole data. The default is set to 0.8
#'
#' @return A vector of length 2 that contains the fitted mixed random forest model and the confusion matrix using the test data.
#'
#' @examples
#'
#' # Example Usage
#' data <- caret::twoClassSim(1000, intercept = -15, linearVars = 15, noiseVars = 5)
#' names(data)[names(data)=="Class"]="y"
#' data$y = as.factor(ifelse(data$y=="Class1",0,1))
#'
#' result <- ThresholdRF(data,ntree=100,prob=0.8)
#'
ThresholdRF <- function(data,ntree=3000,prob=0.8){
  set.seed(123)
  trainIndex <- caret::createDataPartition(data$y, p = prob, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  testFeatures <- testData[, -which(names(testData) == "y")]

  rf <- randomForestSRC::rfsrc(y ~., trainData, rfq =  TRUE, ntree = ntree, perf.type = "g.mean", importance = TRUE)
  threshold_rf<-stats::predict(rf,newdata=testData)

  threshold=1-(sum(trainData$y==1)/length(trainData$y))
  predic <- ifelse(threshold_rf$predicted[,1] > threshold,0,1)
  conf_matrix=caret::confusionMatrix(as.factor(predic),as.factor(testData$y))

  result <- list("model"=rf,"matrix"=conf_matrix)
}
