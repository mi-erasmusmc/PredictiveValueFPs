#' @export
mineEmergentPatternsSettings <- function(minimumSupport, 
                                         maximumPatternLength,
                                         maximumItemSize, 
                                         removeLengthOnePatterns = FALSE,
                                         temporalPlpData,
                                         frequentPatternsObject, 
                                         transactionsObject = NULL, 
                                         savePatterns = FALSE, 
                                         absoluteDifference, 
                                         keepDiscriminative = FALSE, 
                                         classification = FALSE, 
                                         outputFolder = getwd(), 
                                         fileName){
  
  featureEngineeringSettings <- list(
    support = minimumSupport,
    maxlen = maximumPatternLength,
    maxsize = maximumItemSize,
    temporalPlpData = temporalPlpData,
    removeLengthOnePatterns = removeLengthOnePatterns,
    transactionsObject = transactionsObject,
    absoluteDifference = absoluteDifference,
    keepDiscriminative = keepDiscriminative,
    savePatterns = savePatterns,
    outputFolder = outputFolder, 
    fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "mineEmergentPatterns"
  
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "mineEmergentPatternSettings"
  return(featureEngineeringSettings)
  
}

#' @export
mineEmergentPatterns <- function(trainData, 
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
  absoluteDifference = featureEngineeringSettings$absoluteDifference
  keepDiscriminative = featureEngineeringSettings$keepDiscriminative
  populationSettings = featureEngineeringSettings$populationSettings
  savePatterns = featureEngineeringSettings$savePatterns
  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation)
  }
  
  trainDataRowId <- trainData$labels$rowId
  
  ParallelLogger::logInfo(paste("Preparing study population of train set..."))
  # trainStudyPopulation <- trainData$labels$outcomeCount
  
  ParallelLogger::logInfo(paste("Getting negative train set row ids..."))
  negativeRowId <- trainData$labels %>% 
    filter(outcomeCount == 0) %>%
    pull(rowId)
  ParallelLogger::logInfo(paste("Getting positive train set row ids..."))
  positiveRowId <- trainData$labels %>% 
    filter(outcomeCount == 1) %>%
    pull(rowId)
  
  covariateData <- temporalPlpData$covariateData
  
  ParallelLogger::logInfo(paste("Splitting covariateData..."))
  negativeCovariateData <- copyAndromeda(covariateData)
  negativeCovariateData$covariates <- negativeCovariateData$covariates %>%
    filter(rowId %in% negativeRowId)
  positiveCovariateData <- copyAndromeda(covariateData)
  positiveCovariateData$covariates <- positiveCovariateData$covariates %>%
    filter(rowId %in% positiveRowId)
  
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
    nameMinSup <- gsub(pattern = ".", replacement = "_", x = minimumSupport)
    namePatternLength <- as.numeric(patternLength)
    nameLookBackPeriod <- max(inputData$timeId)
    
    saveRDS(s0, file.path(dirLocation, paste0(fileName, "_", nameMinSup, "_", namePatternLength, "_negative_FPs_train.Rds")))
    saveRDS(s1, file.path(dirLocation, paste0(fileName, "_", nameMinSup, "_", namePatternLength, "_positive_FPs_train.Rds")))
  }
  
  initialFPsNegative <- as.numeric(dim(s0)[1])
  initialFPsPositive <- as.numeric(dim(s1)[1])
  
  ParallelLogger::logInfo(paste0("The set of FPs extracted for the negative outcome class were ", initialFPsNegative, ".", " The set of FPs extracted for the positive outcome class were ", initialFPsPositive, "."))
  
  result = list(negativeClassFPs = s0, 
                positiveClassFPs = s1)
  return(result)
  
}
