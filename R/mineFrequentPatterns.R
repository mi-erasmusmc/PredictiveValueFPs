#' @export
mineFrequentPatternsSettings <- function(minimumSupport, 
                                         maximumPatternLength,
                                         maximumItemSize, 
                                         removeLengthOnePatterns = FALSE, 
                                         temporalPlpData, 
                                         transactionsObject = NULL, 
                                         savePatterns = FALSE, 
                                         classification = FALSE, 
                                         outputFolder = getwd(), 
                                         fileName){
  
  miningFPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            temporalPlpData = temporalPlpData, 
                            transactionsObject = transactionsObject,
                            savePatterns = savePatterns, 
                            classification = classification, 
                            outputFolder = outputFolder, 
                            fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(miningFPsSettings, "fun") <- "mineFrequentPatterns"
  
  class(miningFPsSettings) <- "mineFrequentPatternsSettings"
  return(miningFPsSettings) 
}

#' @export
mineFrequentPatterns <- function(trainData, 
                                 featureEngineeringSettings, 
                                 covariateIdsInclude = NULL){
  # frequent pattern mining settings
  minimumSupport = featureEngineeringSettings$support
  patternLength = featureEngineeringSettings$maxlen
  itemSize = featureEngineeringSettings$maxsize
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  temporalPlpData = featureEngineeringSettings$temporalPlpData
  transactionsObject = featureEngineeringSettings$transactionsObject
  savePatterns = featureEngineeringSettings$savePatterns
  classification = featureEngineeringSettings$classification
  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation, recursive = TRUE)
  }
  
    trainDataRowId <- trainData$Train$labels$rowId
    
    covariateData <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
    
    
    ParallelLogger::logTrace("\nPreparing data for Frequent Pattern mining...")
    
    covariateData$covariates <- covariateData$covariates %>%
      dplyr::filter(rowId %in% trainDataRowId)
    
    start <- Sys.time()
    
    if (classification == FALSE){
      inputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, 
                                                                fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
      
      transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
                                                    info = c("sequenceID","eventID","SIZE"))
      
      delta <- Sys.time() - start
      ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
      s0 <- arulesSequences::cspade(data = transactions,
                                    parameter = list(support = minimumSupport,
                                                     maxlen = patternLength,
                                                     maxsize = itemSize),
                                    control = list(verbose = TRUE, tidLists = TRUE))
      
      nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
      namePatternLength <- as.numeric(patternLength)
      # nameLookBackPeriod <- max(inputData$timeId)
      if (savePatterns){
        saveRDS(s0, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_FPs_train.Rds")))
      }
      
    } else {
      trInputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateData, 
                                                                  fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
      labels <- data.frame(rowId = trainData$labels$rowId, 
                           outcomeCount = trainData$labels$outcomeCount)
      inputData <- AssociationRuleMining::getInputFileForCSpadeWithClass(studyPopulation = labels, 
                                                                         transactions = trInputData, 
                                                                         outputFolder = file.path(dirLocation, paste0(fileName, "class.txt")))
      
      transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "class.txt")), sep = ";", 
                                                    info = c("sequenceID","eventID","SIZE", "classID"))
      
      delta <- Sys.time() - start
      ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
      s0 <- arulesSequences::cspade(data = transactions, 
                                    parameter = list(support = minimumSupport, 
                                                     maxlen = patternLength, 
                                                     maxsize = itemSize), 
                                    control = list(verbose = TRUE, tidLists = TRUE))
      nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
      namePatternLength <- as.numeric(patternLength)
      # nameLookBackPeriod <- max(inputData$timeId)
      if (savePatterns){
        saveRDS(s0, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_classes_FPs_train.Rds")))
      }
      
    }
    
    transactionsRowId <- unique(arules::transactionInfo(transactions)$sequenceID)
    
    initialFPs <- as.numeric(dim(s0)[1])
    ParallelLogger::logInfo(paste("The set of FPs extracted were", initialFPs, "."))
    
    result <- list(inputData = inputData, 
                   transactions = transactions, 
                   transactionsRowId = transactionsRowId,
                   frequentPatternsObject = s0)
    return(result)
    
}

#' @export
mineTotalFrequentPatterns <- function(trainData, 
                                 featureEngineeringSettings, 
                                 covariateIdsInclude = NULL){
  # frequent pattern mining settings
  minimumSupport = featureEngineeringSettings$support
  patternLength = featureEngineeringSettings$maxlen
  itemSize = featureEngineeringSettings$maxsize
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  temporalPlpData = featureEngineeringSettings$temporalPlpData
  transactionsObject = featureEngineeringSettings$transactionsObject
  savePatterns = featureEngineeringSettings$savePatterns
  classification = featureEngineeringSettings$classification
  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation, recursive = TRUE)
  }
  
  ParallelLogger::logInfo("Starting mining train set.")
  
  trainDataRowId <- trainData$Train$labels$rowId
  
  trainCovariateData <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
  
  
  ParallelLogger::logInfo("\nPreparing train data for Frequent Pattern mining...")
  
  trainCovariateData$covariates <- trainCovariateData$covariates %>%
    dplyr::filter(rowId %in% trainDataRowId)
  
  start <- Sys.time()
  
  if (classification == FALSE){
    inputData <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = trainCovariateData, 
                                                              fileToSave = file.path(dirLocation, paste0(fileName, ".txt")))
    
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
                                                  info = c("sequenceID","eventID","SIZE"))
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arulesSequences::cspade(data = transactions,
                                  parameter = list(support = minimumSupport,
                                                   maxlen = patternLength,
                                                   maxsize = itemSize),
                                  control = list(verbose = TRUE, tidLists = TRUE))
    
    # The following is to correct for the arulesSequences misreading of rowIds 
    tidLists <- arules::supportingTransactions(s0, transactions = transactions)
    s0@tidLists <- tidLists
    
    nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
    namePatternLength <- as.numeric(patternLength)
    # nameLookBackPeriod <- max(inputData$timeId)
    if (savePatterns){
      saveRDS(s0, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_FPs_train.Rds")))
    }
    
  } else {
    
    labels <- data.frame(rowId = trainData$labels$rowId, 
                         outcomeCount = trainData$labels$outcomeCount)
    inputData <- AssociationRuleMining::getInputFileForCSpadeWithClass(studyPopulation = labels, 
                                                                       transactions = trInputData, 
                                                                       outputFolder = file.path(dirLocation, paste0(fileName, "class.txt")))
    
    transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "class.txt")), sep = ";", 
                                                  info = c("sequenceID","eventID","SIZE", "classID"))
    
    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
    s0 <- arulesSequences::cspade(data = transactions, 
                                  parameter = list(support = minimumSupport, 
                                                   maxlen = patternLength, 
                                                   maxsize = itemSize), 
                                  control = list(verbose = TRUE, tidLists = TRUE))
    # The following is to correct for the arulesSequences misreading of rowIds 
    tidLists <- supportingTransactions(s0, transactions = transactions)
    s0@tidLists <- tidLists
    ###
    nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
    namePatternLength <- as.numeric(patternLength)
    # nameLookBackPeriod <- max(inputData$timeId)
    if (savePatterns){
      saveRDS(s0, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_classes_FPs_train.Rds")))
    }
    
  }
  
  transactionsRowId <- unique(arules::transactionInfo(transactions)$sequenceID)
  
  initialFPs <- as.numeric(dim(s0)[1])
  ParallelLogger::logInfo(paste("The set of FPs extracted were", initialFPs, "."))
  
  if (removeLengthOnePatterns == TRUE){
    s0 <- arulesSequences::subset(s0, size(x) > 1)
    remainingFPs <- as.numeric(dim(s0)[1])
    ParallelLogger::logInfo(paste("After removing length one FPs there were", remainingFPs, "remaining."))
  }
  
  
  if(dim(s0)[1]== 0){
    trainCov = trainData$Train
    ParallelLogger::logInfo("FP mining returned 0 FPs therefore returning trainData.")
  } else {
  
  trainCov <- AssociationRuleMining::addFrequentPatternsToAndromedaFromCSpade(plpDataObject = trainData$Train, 
                                                                         fileWithFPs = s0,
                                                                         objectWithIds = inputData, 
                                                                         transactionsRowId = transactionsRowId,
                                                                         fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
  }
  ### End of Train
  
  ParallelLogger::logInfo("Starting mining test set.")
  
  if(dim(s0)[1]== 0){
    testCov = trainData$Test
    ParallelLogger::logInfo("FP mining returned 0 FPs therefore returning trainData.")
  } else {
  testDataRowId <- trainData$Test$labels$rowId
  
  testCovariateData <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
  
  testCovariateData$covariates <- testCovariateData$covariates %>%
    dplyr::filter(rowId %in% testDataRowId)
  
  ParallelLogger::logInfo("Creating input data for test set.")
  inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = testCovariateData, fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "testSet.txt")))
  testTransactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "testSet.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
  
  ParallelLogger::logInfo("Mining test set...")
  # Extracting matching transactions
  patternsTest <- arules::supportingTransactions(s0, transactions = testTransactions)
  ParallelLogger::logInfo("Done.")
  
  if (savePatterns){
    saveRDS(patternsTest, file.path(dirLocation, paste0(fileName,"_MS_", nameMinSup, "_PL_", namePatternLength,  "_minedFPs_testSet.Rds")))
  }
  
  ParallelLogger::logInfo("Appending to Andromeda...")
  testTransactionsRowId <- unique(arules::transactionInfo(testTransactions)$sequenceID)
  testCov <- AssociationRuleMining::addTestSetPatternsToAndromedaFromCSpade(plpDataObject = trainData$Test, 
                                                                            plpDataTrain  = trainCov$covariateData$covariateRef,
                                                                            fileWithFPs = patternsTest,
                                                                            objectWithIds = inputDataTest, 
                                                                            transactionsRowId = testTransactionsRowId,
                                                                            fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
  }
  
  # featureEngeering <- list(
  #   funct = 'appendFrequentPatterns',
  #   settings = list(
  #     featureEngineeringSettings = featureEngineeringSettings, 
  #     covariateIDsInclude = covariateIDsInclude
  #   )
  # )
  # 
  # attr(trainCov, 'metaData')$featureEngineering = listAppend(
  #   attr(trainCov, 'metaData')$featureEngineering,
  #   featureEngeering
  # )
  
  attr(trainCov$covariateData, "patternLength") <- patternLength
  attr(trainCov$covariateData, "minimumSupport") <- minimumSupport
  attr(testCov$covariateData, "patternLength") <- patternLength
  attr(testCov$covariateData, "minimumSupport") <- minimumSupport
  
  result  = list(
  Train = trainCov,
  Test = testCov
  )
  
  return(result)
  
}
