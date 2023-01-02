#' @export
generateFPObjects <- function(minimumSupportValues, 
                              patternLengthValues, 
                              covariateSet = c("freqPatsOnly", "mix"),
                              outputFolder, 
                              fileName){
  
  plpData_directory <- file.path(outputFolder, "data", "inputs", "plpData")
  nameMinSup = gsub(x = min(minimumSupportValues), pattern = "\\.",replacement =  "_")
  namePatternLength = max(patternLengthValues)
  
  ParallelLogger::logInfo(paste("Loading baked mother data.."))
  
  output2 <- loadBakedData(file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
  
  combs <- expand.grid(minimumSupportValues, patternLengthValues) %>%
    slice(n = 1:n() - 1)
  
  referenceNames <- output2$plpData$Train$covariateData$covariateRef %>% dplyr::collect() %>% base::colnames()

  if (!(c("support.x") %in% referenceNames)){
  if (covariateSet == "freqPatOnly"){
    covList <- list()
    for (i in seq_along(1:nrow(combs))) {
      
      result = list(
        population = output2$population
      )
      
      result$plpData <- filterPlpData(plpData = output2$plpData, 
                                      minimumSupport = combs[i, 1], 
                                      patternLength = combs[i, 2],
                                      createSets = "freqPatsOnly") 
      
      nameMinSup = gsub(x = combs[i, 1], pattern = "\\.",replacement =  "_")
      namePatternLength = combs[i, 2]
      saveBakedData(object = result, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData_fpsOnly")))
      # covList[[i]] <- result
    }
  }
  if (covariateSet == "mix"){
    covList <- list()
      for (i in seq_along(1:nrow(combs))) {
        result = list(
          population = output2$population
          )
    
        result$plpData <- filterPlpData(plpData = output2$plpData, 
                                        minimumSupport = combs[i, 1], 
                                        patternLength = combs[i, 2],
                                        createSets = "mix") 
    
  
        nameMinSup = gsub(x = combs[i, 1], pattern = "\\.",replacement =  "_")
        namePatternLength = combs[i, 2]
        saveBakedData(object = result, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
        # covList[[i]] <- result
      }
  }
  } else {
    if (covariateSet == "freqPatOnly"){
      covList <- list()
      for (i in seq_along(1:nrow(combs))) {
        
        
        
        result = list(
          population = output2$population
        )
        
        result$plpData <- filterPlpDataEmergentPatterns(plpData = output2$plpData, 
                                        minimumSupport = combs[i, 1], 
                                        patternLength = combs[i, 2],
                                        createSets = "freqPatsOnly") 
        
        nameMinSup = gsub(x = combs[i, 1], pattern = "\\.",replacement =  "_")
        namePatternLength = combs[i, 2]
        saveBakedData(object = result, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData_fpsOnly")))
        # covList[[i]] <- result
      }
    }
    if (covariateSet == "mix"){
      covList <- list()
      for (i in seq_along(1:nrow(combs))) {
        result = list(
          population = output2$population
        )
        
        result$plpData <- filterPlpDataEmergentPatterns(plpData = output2$plpData, 
                                        minimumSupport = combs[i, 1], 
                                        patternLength = combs[i, 2],
                                        createSets = "mix") 
        
        
        nameMinSup = gsub(x = combs[i, 1], pattern = "\\.",replacement =  "_")
        namePatternLength = combs[i, 2]
        saveBakedData(object = result, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
        # covList[[i]] <- result
      }
    }
  }
  ParallelLogger::logInfo(paste("Done."))
}