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
      filter(rowId %in% trainDataRowId)
    
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
        saveRDS(s0, file.path(dirLocation, paste0(fileName, "_", nameMinSup, "_", namePatternLength, "_FPs_train.Rds")))
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
        saveRDS(s0, file.path(dirLocation, paste0(fileName, "_", nameMinSup, "_", namePatternLength, "_classes_FPs_train.Rds")))
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
