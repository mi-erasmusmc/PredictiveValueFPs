#' @export
filterCovariateData <- function(plpData, minimumSupport, patternLength, createSets = c("freqPatsOnly", "mix")){
  
  
  metaData <- attr(plpData$Train$covariateData, "metaData")
  covariateData <- Andromeda::copyAndromeda(plpData$Train$covariateData)
  minSup = minimumSupport
  patLen = patternLength
  
 # # This fix should go to AssociationRuleMining where the vectors are created in the first place
 #  covariateData$covariateRef <- covariateData$covariateRef %>%
 #    mutate(patternLength = as.numeric(patternLength),
 #           support = as.numeric(support))
  
  atemporalCovariates <- covariateData$covariateRef %>% 
    filter(analysisId != 999) %>%
    pull(covariateId)
  
  keepCovariates <- covariateData$covariateRef %>% 
    filter(analysisId == 999 & patternLength > 1 & patternLength <= patLen & support >= minSup) %>%
    pull(covariateId)
  
  if (createSets == "freqPatsOnly"){
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      filter(covariateId %in% keepCovariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
  covariates <- c(atemporalCovariates, keepCovariates)
  
  covariateData$covariateRef <- covariateData$covariateRef %>%
    filter(covariateId %in% covariates)
  
  covariateData$covariates <- covariateData$covariates %>%
    filter(covariateId %in% covariates)
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
    filter(analysisId != 999) %>%
    pull(covariateId)
  
  keepCovariates <- covariateData$covariateRef %>% 
    filter(analysisId == 999 & patternLength <= patLen & support >= minSup) %>%
    pull(covariateId)
  
  if (createSets == "freqPatsOnly"){
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      filter(covariateId %in% keepCovariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
    covariates <- c(atemporalCovariates, keepCovariates)
    
    covariateData$covariateRef <- covariateData$covariateRef %>%
      filter(covariateId %in% covariates)
    
    covariateData$covariates <- covariateData$covariates %>%
      filter(covariateId %in% covariates)
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
  
  # testCovariateData$covariateRef <- testCovariateData$covariateRef %>%
  #   mutate(patternLength = as.numeric(patternLength),
  #          support = as.numeric(support))
  
  # atemporalCovariates <- testCovariateData$covariateRef %>% 
  #   filter(analysisId != 999) %>%
  #   pull(covariateId)
  # 
  # keepCovariates <- testCovariateData$covariateRef %>% 
  #   filter(analysisId == 999 & patternLength <= patLen & support >= minSup) %>%
  #   pull(covariateId)
  
  if (createSets == "freqPatsOnly"){
    
    testCovariateData$covariateRef <- testCovariateData$covariateRef %>%
      filter(covariateId %in% keepCovariates)
    
    testCovariateData$covariates <- testCovariateData$covariates %>%
      filter(covariateId %in% keepCovariates)
  }
  
  if (createSets == "mix"){    
    
    testCovariateData$covariateRef <- testCovariateData$covariateRef %>%
      filter(covariateId %in% covariates)
    
    testCovariateData$covariates <- testCovariateData$covariates %>%
      filter(covariateId %in% covariates)
  }
  
  class(testCovariateData) = 'CovariateData'
  attr(class(testCovariateData), "package") <- "FeatureExtraction"
  attr(testCovariateData, "metaData") <- testMetaData
  attr(testCovariateData, "patternLength") <- patternLength
  attr(testCovariateData, "minimumSupport") <- minimumSupport
  result$Test$covariateData <- testCovariateData
  
  return(result)
}
