#' @export
prepareFrequentPatternsObjectSettings <- function(minimumSupport, 
                                                  maximumPatternLength,
                                                  maximumItemSize, 
                                                  removeLengthOnePatterns = FALSE, 
                                                  # frequentPatternsObject, 
                                                  transactionsObject = NULL, 
                                                  savePatterns = FALSE, 
                                                  classification = FALSE, 
                                                  outputFolder = getwd(), 
                                                  fileName){
  
  miningFPsSettings <- list(support = minimumSupport, 
                            maxlen = maximumPatternLength, 
                            maxsize = maximumItemSize, 
                            removeLengthOnePatterns = removeLengthOnePatterns,
                            # frequentPatternsObject = frequentPatternsObject, 
                            transactionsObject = transactionsObject,
                            savePatterns = savePatterns, 
                            classification = classification, 
                            outputFolder = outputFolder, 
                            fileName = fileName
  )
  
  # specify the function that will implement the sampling
  attr(miningFPsSettings, "fun") <- "prepareFrequentPatternsObjects"
  
  class(miningFPsSettings) <- "prepareFrequentPatternsObjectsSettings"
  return(miningFPsSettings) 
}

#' @export
prepareFrequentPatternsObjects <- function(frequentPatternsObject, 
                                           featureEngineeringSettings, 
                                           covariateIdsInclude = NULL){
  # frequent pattern mining settings
  minimumSupport = featureEngineeringSettings$support
  patternLength = featureEngineeringSettings$maxlen
  itemSize = featureEngineeringSettings$maxsize
  outputFolder = featureEngineeringSettings$outputFolder
  fileName = featureEngineeringSettings$fileName
  removeLengthOnePatterns = featureEngineeringSettings$removeLengthOnePatterns
  # frequentPatternsObject = featureEngineeringSettings$frequentPatternsObject
  transactionsObject = featureEngineeringSettings$transactionsObject
  savePatterns = featureEngineeringSettings$savePatterns
  classification = featureEngineeringSettings$classification
  
  dirLocation <- file.path(outputFolder)
  
  if (!dir.exists(dirLocation)){
    dir.create(dirLocation)
  }
  
  # if (is.null(covariateIdsInclude) == TRUE){
  #   
  #   trainDataRowId <- trainData$labels$rowId
  #   
  #   covariateData <- Andromeda::copyAndromeda(temporalPlpData$covariateData)
  #   
  #   if (!all.equal(trainDataRowId, sequenceInfo(frequentPatternsObject))){
  #     stop("Patient IDs not the same in training data and frequentPatternsObject")
  #   }
    
    initialFPs <- as.numeric(dim(frequentPatternsObject)[1])
    ParallelLogger::logInfo(paste("The set of intial FPs were", initialFPs, "."))
    
    start <- Sys.time()
      
      delta <- Sys.time() - start
      ParallelLogger::logTrace(paste("Preparing data for mining took", signif(delta, 3), attr(delta, "units")))
      
      # s1 <- arules::supportingTransactions(freaquentPatternsObject, transactions = transactions)
      s0 <- arulesSequences::subset(frequentPatternsObject, size(frequentPatternsObject) <= patternLength)
      ParallelLogger::logInfo(paste("After removing FPs with length less than or equal to", patternLength, "there were", length(s0), "remaining."))
      s0 <- arulesSequences::subset(s0, support >= minimumSupport)
      ParallelLogger::logInfo(paste("After removing FPs with minimum support greater than or equal to", minimumSupport, "there were", length(s0), "remaining."))
    
    if (savePatterns){
      nameMinSup <- gsub(pattern = "\\.", replacement = "_", x = minimumSupport)
      namePatternLength <- as.numeric(patternLength)
      
      saveRDS(s0, file.path(dirLocation, paste0(fileName,"_MS_", nameMinSup, "_PL_", namePatternLength, "_FPs_train.Rds")))
    }
    
    if (removeLengthOnePatterns == TRUE){
      s0 <- arulesSequences::subset(s0, size(s0) > 1)
      remainingFPs <- as.numeric(dim(s0)[1])
      ParallelLogger::logInfo(paste("After removing length one FPs there were", remainingFPs, "remaining."))
    }
    

      if(dim(s0)[1]== 0){
        result <- trainData
        ParallelLogger::logInfo("FP mining returned 0 FPs therefore returning trainData.")
      } else {
        result = s0
      }
      
      return(result)
}
 