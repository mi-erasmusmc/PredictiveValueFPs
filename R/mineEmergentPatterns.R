#' @export
mineTotalEmergentPatterns <- function(trainData, 
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
  absoluteDifference = featureEngineeringSettings$absoluteDifference
  keepDiscriminative = featureEngineeringSettings$keepDiscriminative
  
  nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
  namePatternLength <- as.numeric(patternLength)
  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation, recursive = TRUE)
  }
  
  ParallelLogger::logInfo("Starting mining train set.")
  
  trainDataRowId <- trainData$Train$labels$rowId
  
  ParallelLogger::logInfo(paste("Preparing study population of train set..."))
  # trainStudyPopulation <- trainData$labels$outcomeCount
  
  ParallelLogger::logInfo(paste("Getting negative train set row ids..."))
  negativeRowId <- trainData$Train$labels %>% 
    dplyr::filter(outcomeCount == 0) %>%
    dplyr::pull(rowId)
  ParallelLogger::logInfo(paste("Getting positive train set row ids..."))
  positiveRowId <- trainData$Train$labels %>% 
    dplyr::filter(outcomeCount == 1) %>%
    dplyr::pull(rowId)
  
  covariateData <- temporalPlpData$covariateData
  
  ParallelLogger::logInfo(paste("Splitting covariateData..."))
  negativeCovariateData <- Andromeda::copyAndromeda(covariateData)
  negativeCovariateData$covariates <- negativeCovariateData$covariates %>%
    dplyr::filter(rowId %in% negativeRowId)
  positiveCovariateData <- Andromeda::copyAndromeda(covariateData)
  positiveCovariateData$covariates <- positiveCovariateData$covariates %>%
    dplyr::filter(rowId %in% positiveRowId)
  
  ParallelLogger::logTrace("\nPreparing data for Frequent Pattern mining...")
  
  start <- Sys.time()
  # if (file.exists(file.path(dirLocation, paste0(fileName, ".txt"))) == FALSE){
  inputDataNegative <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = negativeCovariateData, 
                                                                    fileToSave = file.path(dirLocation, paste0(fileName, "negative.txt")))
  
  inputDataPositive <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = positiveCovariateData, 
                                                                    fileToSave = file.path(dirLocation, paste0(fileName, "positive.txt")))
  
  transactionsNegative <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "negative.txt")), sep = ";", 
                                                        info = c("sequenceID","eventID","SIZE"))
  # transactionsNegative@itemsetInfo$sequenceID <- inputDataNegative$rowId
  transactionsPositive <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, "positive.txt")), sep = ";", 
                                                        info = c("sequenceID","eventID","SIZE"))
  # transactionsPositive@itemsetInfo$sequenceID <- inputDataPositive$rowId
  # } else {
  #   transactions <- arulesSequences::read_baskets(con =  file.path(dirLocation, paste0(fileName, ".txt")), sep = ";", 
  #                                                 info = c("sequenceID","eventID","SIZE"))
  # }
  
  delta <- Sys.time() - start
  ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
  s0 <- arulesSequences::cspade(data = transactionsNegative, 
                                parameter = list(support = minimumSupport, 
                                                 maxlen = patternLength, 
                                                 maxsize = itemSize), 
                                control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
  
  s1 <- arulesSequences::cspade(data = transactionsPositive, 
                                parameter = list(support = minimumSupport, 
                                                 maxlen = patternLength, 
                                                 maxsize = itemSize), 
                                control = list(verbose = TRUE, tidLists = TRUE, numpart = 1))
  if (savePatterns){
    saveRDS(s0, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_negative_FPs_train.Rds")))
    saveRDS(s1, file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_positive_FPs_train.Rds")))
    
  }
  
  initialFPsNegative <- as.numeric(dim(s0)[1])
  initialFPsPositive <- as.numeric(dim(s1)[1])
  
  ParallelLogger::logInfo(paste0("The set of FPs extracted for the negative outcome class were ", initialFPsNegative, ".", " The set of FPs extracted for the positive outcome class were ", initialFPsPositive, "."))
  
  if (removeLengthOnePatterns == TRUE){
    s0 <- arulesSequences::subset(s0, size(x) > 1)
    s1 <- arulesSequences::subset(s1, size(x) > 1)
    remainingFPsNegative <- as.numeric(dim(s0)[1])
    remainingFPsPositive <- as.numeric(dim(s1)[1])
    ParallelLogger::logInfo(paste("After removing length one FPs from the negative class, there were", remainingFPsNegative, "remaining.", "After removing length one FPs from the positive class, there were", remainingFPsPositive, "remaining."))
  }
  
  #if (nrow(s0) < 5000) {
  #Need new function to combine results
  commonPatterns <- AssociationRuleMining::filterCommonPatterns(s0, 
                                                                s1, 
                                                                transactionsNegative = transactionsNegative, 
                                                                transactionsPositive = transactionsPositive,
                                                                absoluteDifference = absoluteDifference, 
                                                                keepDiscriminative = keepDiscriminative)
  ParallelLogger::logInfo(paste("After removing redundant FPs from the negative class, there were", dim(commonPatterns[[1]])[1], "remaining.", 
                                "After removing length one FPs from the positive class, there were", dim(commonPatterns[[2]])[1], "remaining."))
  
  if(dim(commonPatterns[[1]])[1]== 0){
    cov0 <- negativeCovariateData
    transactionsRowIdNegative <- NULL
    ParallelLogger::logInfo("FP mining returned 0 FPs for negative class therefore returning trainData.")
  } else {
    cov0 <- commonPatterns[[1]]
    transactionsRowIdNegative <- unique(transactionInfo(transactionsNegative)$sequenceID)
  }
  
  if (dim(commonPatterns[[2]])[1]== 0) {
    cov1 <- positiveCovariateData
    transactionsRowIdPositive <- NULL
    ParallelLogger::logInfo("FP mining returned 0 FPs for positive class therefore returning trainData.")
  } else {
    cov1 <- commonPatterns[[2]]
    transactionsRowIdPositive <- unique(transactionInfo(transactionsPositive)$sequenceID)
  }
  
  trainCov <- AssociationRuleMining::addEmergentPatternsToAndromeda(plpDataObject = trainData$Train,
                                             fileWithFPsNegative = cov0, 
                                             fileWithFPsPositive = cov1,
                                             transactionsRowIdNegative = transactionsRowIdNegative, 
                                             transactionsRowIdPositive = transactionsRowIdPositive, 
                                             objectWithIdsNegative = inputDataNegative,
                                             objectWithIdsPositive = inputDataPositive, 
                                             fileToSave = file.path(dirLocation, fileName))
  

  ### End of Train
  
  ParallelLogger::logInfo("Starting mining test set.")
  
  if(dim(s0)[1]== 0){
    testCov = trainData$Test
    ParallelLogger::logInfo("FP mining returned 0 FPs therefore returning trainData.")
  } else {
    ParallelLogger::logInfo("Creating input data for test set.")
    testDataRowId <- trainData$Test$labels$rowId

    covariateDataTest <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
    
    covariateDataTest$covariates <- covariateDataTest$covariates %>%
      dplyr::filter(rowId %in% testDataRowId)
    
    inputDataTest <- AssociationRuleMining::getInputFileForCSpade(covariateDataObject = covariateDataTest, 
                                                                  fileToSave = file.path(dirLocation, 
                                                                                         paste0(fileName, "testSet.txt")))
    #browser()
    transactionsTest <- arulesSequences::read_baskets(con =  file.path(dirLocation, 
                                                                   paste0(fileName, "testSet.txt")), sep = ";", info = c("sequenceID","eventID","SIZE"))
    ParallelLogger::logInfo("Mining test set...")
    patternsTrain <- c(commonPatterns[[1]], commonPatterns[[2]])
    duplicated <- arulesSequences::duplicated(patternsTrain)
    patternsTrain <- patternsTrain[!duplicated]
    patternsTest <- arules::supportingTransactions(patternsTrain, transactions = transactionsTest)
    ParallelLogger::logInfo("Done.")
    
    if (savePatterns){
      saveRDS(patternsTest, file.path(dirLocation, paste0(fileName, "_FPs_test.Rds")))
    }
    
    trainCovariateRef <- trainCov$covariateData$trainCovariateRef
    
    transactionsRowIdTest <- unique(transactionInfo(transactionsTest)$sequenceID)
    
    if(dim(patternsTest)[1]== 0){
      cov <- trainData$Test
      ParallelLogger::logInfo("FP mining on the test set returned 0 FPs, therefore returning testData.")
    } else {
      
      ParallelLogger::logInfo("Appending to Andromeda...")
      testCov <- AssociationRuleMining::addTestSetPatternsToAndromedaFromCSpade(plpDataObject = trainData$Test, 
                                                                                plpDataTrain  = trainCov$covariateData$covariateRef,
                                                                                fileWithFPs = patternsTest,
                                                                                objectWithIds = inputDataTest, 
                                                                                transactionsRowId = transactionsRowIdTest,
                                                                                fileToSave = file.path(dirLocation, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
      
    }
    
  }
  
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

#' @export
extractEPs <- function(runFrequentPatternsSettings, 
                       inputFolder,
                       outputFolder,
                       fileName
){
  
  # Settings required for running FPM
  temporalPlpData = runFrequentPatternsSettings$temporalPlpData
  minMinimumSupport = min(runFrequentPatternsSettings$minimumSupportValues)
  maxPatternLength = max(runFrequentPatternsSettings$patternLengthValues)
  minimumSupportValues = runFrequentPatternsSettings$minimumSupportValues
  patternLengthValues = runFrequentPatternsSettings$patternLengthValues
  absoluteDifference = runFrequentPatternsSettings$absoluteDifference
  keepDiscriminative = runFrequentPatternsSettings$keepDiscriminative
  
  FPs_directory <- file.path(outputFolder, "data", "inputs", "minedFPs")
  plpData_directory <- file.path(outputFolder, "data", "inputs", "plpData")
  inputDirectory <- file.path(inputFolder, "data", "inputs", "predictorSets", fileName)
  
  output1 <- loadBakedData(file.path(outputFolder, "data", "processedData"))
  temporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(inputDirectory, paste0(fileName, "_temporal")))
  
  settings <- mineEmergentPatternsSettings(minimumSupport = minMinimumSupport, 
                                           maximumPatternLength = maxPatternLength, 
                                           maximumItemSize = 1, 
                                           removeLengthOnePatterns = TRUE,
                                           temporalPlpData = temporalPlpData,
                                           transactionsObject = NULL, 
                                           savePatterns = TRUE,
                                           classification = FALSE, 
                                           keepDiscriminative = keepDiscriminative,
                                           absoluteDifference = absoluteDifference,
                                           outputFolder = FPs_directory,
                                           fileName = "MOTHER")
  
  output2 = list(
    population = output1$population
  )
  output2$plpData <- PredictiveValueFPs::mineTotalEmergentPatterns(trainData = output1$plpData, featureEngineeringSettings = settings)
  
  ParallelLogger::logInfo(paste("Done extracting fps with minimum support value", minMinimumSupport, "(the lowest), and max pattern length", maxPatternLength,"(the highest)."))
  
  nameMinSup = gsub(x = minMinimumSupport, pattern = "\\.",replacement =  "_")
  namePatternLength = maxPatternLength
  saveBakedData(object = output2, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData_raw")))
  
}