#' @export
filterCovariateData <- function(plpData, minimumSupport, patternLength, createSets = c("freqPatsOnly", "mix")){
  
  
  metaData <- attr(plpData$Train$covariateData, "metaData")
  covariateData <- Andromeda::copyAndromeda(plpData$Train$covariateData)
  minSup = minimumSupport
  patLen = patternLength
  
  atemporalCovariates <- covariateData$covariateRef %>% 
    dplyr::filter(analysisId != 999) %>%
    dplyr::pull(covariateId)
  
  keepCovariates <- covariateData$covariateRef %>% 
    dplyr::filter(analysisId == 999 & patternLength > 1 & patternLength <= patLen & support >= minSup) %>%
    dplyr::pull(covariateId)
  
  if (createSets == "freqPatsOnly"){
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% keepCovariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      dplyr::filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
  covariates <- c(atemporalCovariates, keepCovariates)
  
  covariateData$covariateRef <- covariateData$covariateRef %>%
    dplyr::filter(covariateId %in% covariates)
  
  covariateData$covariates <- covariateData$covariates %>%
    dplyr::filter(covariateId %in% covariates)
  }
  
  result <-  plpData
  class(covariateData) = 'CovariateData'
  attr(class(covariateData), "package") <- "FeatureExtraction"
  attr(covariateData, "metaData") <- metaData
  attr(covariateData, "patternLength") <- patternLength
  attr(covariateData, "minimumSupport") <- minimumSupport
  result$Train$covariateData <- covariateData
  
  return(result)
}

#' @export
filterPlpData <- function(plpData, minimumSupport, patternLength, createSets = c("freqPatsOnly", "mix")){
  
  # Train set
  ParallelLogger::logInfo("Preparing train set...")
  metaData <- attr(plpData$Train$covariateData, "metaData")
  covariateData <- Andromeda::copyAndromeda(plpData$Train$covariateData)
  minSup = minimumSupport
  patLen = patternLength
  
  atemporalCovariates <- covariateData$covariateRef %>% 
    dplyr::filter(analysisId != 999) %>%
    dplyr::pull(covariateId)
  
  keepCovariates <- covariateData$covariateRef %>% 
    dplyr::filter(analysisId == 999 & patternLength <= patLen & support >= minSup) %>%
    dplyr::pull(covariateId)
  
  if (createSets == "freqPatsOnly"){
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% keepCovariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      dplyr::filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
    covariates <- c(atemporalCovariates, keepCovariates)
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% covariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      dplyr::filter(covariateId %in% covariates)
  }
  
  result <-  plpData
  class(covariateData) = 'CovariateData'
  attr(class(covariateData), "package") <- "FeatureExtraction"
  attr(covariateData, "metaData") <- metaData
  attr(covariateData, "patternLength") <- patternLength
  attr(covariateData, "minimumSupport") <- minimumSupport
  result$Train$covariateData <- covariateData
  
  
  # test Set
  ParallelLogger::logInfo("Preparing test set...")
  testMetaData <- attr(plpData$Test$covariateData, "metaData")
  testCovariateData <- Andromeda::copyAndromeda(plpData$Test$covariateData)
  
  if (createSets == "freqPatsOnly"){
    
    testCovariateData$covariateRef <- testCovariateData$covariateRef %>%
      dplyr::filter(covariateId %in% keepCovariates)
    
    testCovariateData$covariates <- testCovariateData$covariates %>%
      dplyr::filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
    testCovariateData$covariateRef <- testCovariateData$covariateRef %>%
      dplyr::filter(covariateId %in% covariates)
    
    testCovariateData$covariates <- testCovariateData$covariates %>%
      dplyr::filter(covariateId %in% covariates)
  }
  
  class(testCovariateData) = 'CovariateData'
  attr(class(testCovariateData), "package") <- "FeatureExtraction"
  attr(testCovariateData, "metaData") <- testMetaData
  attr(testCovariateData, "patternLength") <- patternLength
  attr(testCovariateData, "minimumSupport") <- minimumSupport
  result$Test$covariateData <- testCovariateData
  
  return(result)
}
