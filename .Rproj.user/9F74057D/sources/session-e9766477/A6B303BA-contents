#' Mixed Random Forest
#'
#' @description This function utilizes the randomForest library to implement a mixed random forest.
#' The mixed random forest takes a parameter alpha and does downsampling on the original data so that the ratio is 1:alpha.
#' Then it uses weighted random forest for the downsampled data to fit a model. Finally, it returns the fitted model and the confusion matrix of the test data.
#'
#' @param data A data frame that contains the data, with the response variable y being a binary variable of 0 and 1 with 1 being the minority class.
#' @param weights A vector that contains the weights that we want to test on. The function uses k-fold CV to find best weight.
#' @param alphas A vector that contains the alphas that we want to test on. The function uses k-fold CV to find the best alpha.
#' @param k Indicates the number of folds to create during cross validation process. The default is set to 5.
#' @param ntree The number of trees used in the balanced random forest model. The default is set to 100.
#' @param prob The ratio of training set to the whole data. The default is set to 0.8
#'
#' @return A vector of length 2 that contains the fitted mixed random forest model and the confusion matrix using the test data.
#'
#' @examples
#' # Example Usage
#' data <- caret::twoClassSim(1000, intercept = -15, linearVars = 15, noiseVars = 5)
#' names(data)[names(data)=="Class"]="y"
#' data$y = as.factor(ifelse(data$y=="Class1",0,1))
#'
#' result <- MixedRF(data,weights=c(100,1000,10000,100000),alphas=c(2,3,4,5),k=5,ntree=100,prob=0.8)
#'
#'
MixedRF <- function(data,weights,alphas,k=5,ntree=100,prob=0.8){
  set.seed(123)
  trainIndex <- caret::createDataPartition(data$y, p = prob, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  testFeatures <- testData[, -which(names(testData) == "y")]

  folds <- caret::createFolds(trainData$y, k = k)
  bestwt <- 1
  best_bal_acc <- 0
  bestalpha<-1
  best_bal_alpha<-0
  bal_acc_weight=rep(0,length(weights))
  count <- 1
  weights<-as.vector(weights)
  count2<-1

  #split the CV into two parts since double for loop takes too much time
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

  for(alpha in alphas){
    bal_acc=rep(0,k)
    for (fold in folds){
      train_fold <- trainData[-fold, ]
      test_fold <- trainData[fold, ]
      nmin<-sum(train_fold$y==1)
      nalpha <- as.integer(alpha*nmin)

      mix_model <- randomForest::randomForest(
        y ~ .,
        data = train_fold,
        importance = TRUE,
        ntree=ntree,
        strata=train_fold$y,
        sampsize=c(nalpha,nmin),
        classwt=c(wt,1)
      )

      predictions <- stats::predict(mix_model, test_fold)
      bal_acc[count2] <- (sum(predictions == 1 & test_fold$y==1)/sum(test_fold$y==1)+ sum(predictions == 0 & test_fold$y==0)/sum(test_fold$y==0))/2
    }
    mean_bal_acc <- mean(bal_acc)

    if(bestalpha==1 || mean_bal_acc>best_bal_alpha){
      bestalpha <- alpha
      best_bal_alpha <-mean_bal_acc
    }
    count2 <- count2+1
  }

  nmin <- sum(trainData$y==1)

  mix_model <- randomForest::randomForest(
    y ~ .,
    data = trainData,
    importance = TRUE,
    ntree=ntree,
    strata=trainData$y,
    sampsize=c(as.integer(bestalpha*nmin),nmin),
    classwt=c(bestwt,1)
  )

  predictions <- stats::predict(mix_model, newdata = testFeatures)
  conf_matrix <- caret::confusionMatrix(predictions, testData$y)

  result <- list("model"=mix_model,"matrix"=conf_matrix)
}

