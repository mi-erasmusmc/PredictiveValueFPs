#' @export
extractFPs <- function(runFrequentPatternsSettings, 
                       outputFolder,
                       fileName
                       ){
  
  # Settings required for running FPM
  temporalPlpData = runFrequentPatternsSettings$temporalPlpData
  minMinimumSupport = min(runFrequentPatternsSettings$minimumSupportValues)
  maxPatternLength = max(runFrequentPatternsSettings$patternLengthValues)
  minimumSupportValues = runFrequentPatternsSettings$minimumSupportValues
  patternLengthValues = runFrequentPatternsSettings$patternLengthValues
  
  FPs_directory <- file.path(outputFolder, "data", "inputs", "minedFPs")
  plpData_directory <- file.path(outputFolder, "data", "inputs", "plpData")
  plpInput_directory <- file.path(outputFolder, "data", "inputs", "predictorSets", fileName)
  
  output1 <- loadBakedData(file.path(outputFolder, "data", "processedData"))
  temporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(plpInput_directory, paste0(fileName, "_temporal")))
  
  settings <- mineFrequentPatternsSettings(minimumSupport = minMinimumSupport, 
                                           maximumPatternLength = maxPatternLength, 
                                           maximumItemSize = 1, 
                                           removeLengthOnePatterns = TRUE,
                                           temporalPlpData = temporalPlpData,
                                           transactionsObject = NULL, 
                                           savePatterns = TRUE,
                                           classification = FALSE, 
                                           outputFolder = FPs_directory,
                                           fileName = "MOTHER")
  
  output2 = list(
    population = output1$population
  )
  output2$plpData <- PredictiveValueFPs::mineTotalFrequentPatterns(trainData = output1$plpData, featureEngineeringSettings = settings)
  
  ParallelLogger::logInfo(paste("Done extracting fps with minimum support value", minMinimumSupport, "(the lowest), and max pattern length", maxPatternLength,"(the highest)."))
  
  nameMinSup = gsub(x = minMinimumSupport, pattern = "\\.",replacement =  "_")
  namePatternLength = maxPatternLength
  saveBakedData(object = output2, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_plpData")))
  
}