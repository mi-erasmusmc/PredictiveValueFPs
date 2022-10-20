#' @export
filterCovariateData <- function(plpData, minimumSupport, patternLength, createSets = c("freqPatsOnly", "mix")){
  
  
  metaData <- attr(plpData$Train$covariateData, "metaData")
  covariateData <- Andromeda::copyAndromeda(plpData$Train$covariateData)
  minSup = minimumSupport
  patLen = patternLength
  
 # This fix should go to AssociationRuleMining where the vectors are created in the first place
  covariateData$covariateRef <- covariateData$covariateRef %>%
    mutate(patternLength = as.numeric(patternLength),
           support = as.numeric(support))
  
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
  result$Train$covariateData <- covariateData
  
  return(result)
}
