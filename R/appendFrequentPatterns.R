#' @export
appendFrequentPatternsSettings <- function(frequentPatternsObject, inputData, transactionsRowId, frequentPatternMiningSettings, outputFolder = getwd(), fileName){
  #add checks
  
  featureEngineeringSettings <- list(
    support = frequentPatternMiningSettings$support,
    maxlen =frequentPatternMiningSettings$maxlen,
    maxsize = frequentPatternMiningSettings$maxsize,
    temporalPlpData = frequentPatternMiningSettings$temporalPlpData,
    removeLengthOnePatterns = frequentPatternMiningSettings$removeLengthOnePatterns,
    transactionsObject = frequentPatternMiningSettings$transactionsObject,
    savePatterns = frequentPatternMiningSettings$savePatterns,
    classification = frequentPatternMiningSettings$classification,
    # plpDataSettings = plpDataSettings,
    frequentPatternsObject = frequentPatternsObject,
    inputData = inputData,
    transactionsRowId = transactionsRowId,
    outputFolder = outputFolder, 
    fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "appendFrequentPatterns"
  
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  
}

#' @export
appendFrequentPatterns <- function(trainData, featureEngineeringSettings, covariateIDsInclude = NULL){
  
  support = featureEngineeringSettings$support
  maxlen =featureEngineeringSettings$maxlen
  maxsize = featureEngineeringSettings$maxsize
  temporalPlpData = featureEngineeringSettings$temporalPlpData
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  transactionsObject = featureEngineeringSettings$transactionsObject
  savePatterns = featureEngineeringSettings$savePatterns
  classification = featureEngineeringSettings$classification
  # plpDataSettings = plpDataSettings
  frequentPatternsObject <- featureEngineeringSettings$frequentPatternsObject
  inputData <- featureEngineeringSettings$inputData
  transactionsRowId <- featureEngineeringSettings$transactionsRowId
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  
  nameMinimumSupport <- gsub(pattern = "\\.", replacement = "_", x = support)

  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation)
  }
  
  if (is.null(covariateIDsInclude)){
    
    cov <- AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
                                                                           fileWithFPs = frequentPatternsObject,
                                                                           objectWithIds = inputData, 
                                                                           transactionsRowId = transactionsRowId,
                                                                           fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinimumSupport, "_PL_", maxlen, "_plpData")))
    covariateIDsInclude <- list(trainPatterns = frequentPatternsObject, 
                                trainCovariateRef = cov$covariateData$covariateRef)
  } else {
    
    testDataRowId <- trainData$labels$rowId
    
    covariateDataTest <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
    
    covariateDataTest$covariates <- covariateDataTest$covariates %>%
      filter(rowId %in% testDataRowId)
    
    inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateDataTest, fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinimumSupport, "_PL_", maxlen, "testSet.txt")))
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "_MS_", nameMinimumSupport, "_PL_", maxlen, "testSet.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
    
    # Extracting matching transactions
    patternsTrain <- covariateIDsInclude$trainPatterns
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactions)
    
    if (savePatterns){
      saveRDS(patternsTest, file.path(dirLocation, paste0(fileName,"_MS_", nameMinimumSupport, "_PL_", maxlen,  "_minedFPs_testSet.Rds")))
    }
    
    trainCovariateRef <- covariateIDsInclude$trainCovariateRef
    
    transactionsRowId <- unique(transactionInfo(transactions)$sequenceID)
    
    if(dim(patternsTest)[1]== 0){
      cov <- trainData
      ParallelLogger::logInfo("FP mining on the test set returned 0 FPs, therefore returning testData.")
    } else {
      
      cov <- AssociationRuleMining::addTestSetPatternsToAndromedaFromCSpade(plpDataObject = trainData, 
                                                                            fileWithFPs = patternsTest, 
                                                                            objectWithIds = inputDataTest,
                                                                            plpDataTrain = trainCovariateRef,
                                                                            transactionsRowId = transactionsRowId,
                                                                            fileToSave = file.path(dirLocation, "_MS_", nameMinimumSupport, "_PL_", maxlen,  fileName))
      FPsNumber <- cov$covariateData$covariateRef %>% 
        filter(analysisId == 999) %>% 
        count() %>%
        pull(n)
      
      allCovsNumber <- 
        cov$covariateData$covariateRef %>% 
        count() %>%
        pull(n)
      ParallelLogger::logInfo(paste0("testData created with ", FPsNumber, "fps and ", allCovsNumber, "total covariates."))
      
    }
    
    covariateIDsInclude <- list(trainPatterns = patternsTrain, 
                                trainCovariateRef = trainCovariateRef, 
                                testPatterns = patternsTest, 
                                testCovariateRef = cov$covariateData$covariateRef)
  }
  
  featureEngeering <- list(
    funct = 'appendFrequentPatterns',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings, 
      covariateIDsInclude = covariateIDsInclude
    )
  )
  
  attr(cov, 'metaData')$featureEngineering = listAppend(
    attr(cov, 'metaData')$featureEngineering,
    featureEngeering
  )
  return(cov)
}



