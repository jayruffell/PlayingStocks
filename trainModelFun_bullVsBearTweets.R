
#__________________________________________________________________________________________________________________________________

# Function below takes some rows of sharesDF as input (could be particular folds of data rather than all of sharesDF) and produces a model that predicts the prob of an increase in the sharemarket the next day, given an input dataframe with same schema as sharesDF. Returns a list containing the trained model ('$trainedModel'). Also returns performance metrics of final model - '$trainedModelOutputs$trainedModelAcc' and '$trainedModelOutputs$trainedModelAcc_stdev'
#__________________________________________________________________________________________________________________________________

trainModelFun_bullVsBearTweets <- function(inputDF){
  
  require('caret')
  
  # # testing function *HASH OUT*
  # inputDF <- sharesDF; cat('WARNING - HASH OUT TEST VALUES OF TRAIN MODEL FUNCTION\n')

  #++++++++++++++
  # Use caret's createTimeSlices cross validation method to find best model
  #++++++++++++++
  
  # See for implementing in model building:  http://stackoverflow.com/questions/24758218/time-series-data-spliting-and-model-evaluation
  # See here for what the createTimeSlices params are:  https://r-norberg.blogspot.co.nz/2016/08/data-splitting-time-slices-with.html
  
  timeControl <- trainControl(# classProbs=TRUE, summaryFunction=twoClassSummary, # want accuracy instead for now
    method = "timeslice",  # this is better way of calling createTimeSlices function
    initialWindow = initialWindow, 
    horizon = horizon,      
    fixedWindow = TRUE) 

  myModel <- train(priceChange ~ bullToBearRatio, data=inputDF, method="glm", family='binomial', trControl = timeControl)
  
  # Return a list of outputs 
  out <- list(trainedModel=myModel$finalModel,
                              trainedModelAcc=mean(myModel$resample$Accuracy),
                              trainedModelAcc_stdev=sd(myModel$resample$Accuracy))
  return(out)
  }
