#' @export
predictFPs <- function(runPlpSettings, 
                       analysisSettings,
                       covariateSet = c("freqPatsOnly", "mix"),
                       inputFolder,
                       outputFolder){
  
  # Settings required for running plp
  populationSettings = runPlpSettings$populationSettings
  splitSettings = runPlpSettings$splitSettings
  sampleSettings = runPlpSettings$sampleSettings
  preprocessSettings = runPlpSettings$preprocessSettings
  modelSettings = runPlpSettings$modelSettings
  logSettings = runPlpSettings$logSettings
  executeSettings = runPlpSettings$executeSettings
  
  # Settings for analysis
  outcomeId = analysisSettings$outcomeId
  analysisId = analysisSettings$analysisId
  analysisName = analysisSettings$analysisName 
  atemporalPlpData = analysisSettings$atemporalPlpData
  fileName = stringr::str_remove(analysisName, "predicting_")
  
  plpData_directory <- file.path(outputFolder, analysisId, "data", "inputs", "plpData")
  plpOutput_directory <- file.path(outputFolder, analysisId, "results")
  inputDirectory <- file.path(inputFolder, "data", "inputs", "predictorSets", fileName)
  
  atemporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(inputDirectory, paste0(fileName, "_atemporal")))
  
  if (covariateSet == "freqPatsOnly"){
    
    ParallelLogger::logInfo("Loading plpData objects...")
    plpDataObjects <- list.files(plpData_directory, pattern = "*_plpData_fpsOnly")
    plpDataList <- lapply(plpDataObjects, function(x) loadBakedData(file = file.path(plpData_directory, x)))
    modelsList <- vector("list", length(plpDataList))
    
    for (i in seq_along(plpDataList)) {
      minSup = attributes(plpDataList[[i]]$plpData$Train$covariateData)$minimumSupport
      patLen = attributes(plpDataList[[i]]$plpData$Train$covariateData)$patternLength
      MS = gsub(pattern = "\\.", replacement = "_", x = minSup)
      
      
      modelsList[[i]] <- executeRunPlp(plpData = atemporalPlpData, 
                                       data = plpDataList[[i]]$plpData,
                                       population = plpDataList[[1]]$population, 
                                       outcomeId = outcomeId,
                                       analysisId = paste0("FPS_lasso_MS_", MS, "_PL_", patLen, "fpsOnly"),
                                       analysisName = analysisName, 
                                       populationSettings = populationSettings, 
                                       splitSettings = splitSettings, 
                                       sampleSettings = sampleSettings, 
                                       # featureEngineeringSettings = settingsAppend, #note this here  
                                       preprocessSettings = preprocessSettings, 
                                       modelSettings = modelSettings, 
                                       logSettings = logSettings, 
                                       executeSettings = executeSettings, 
                                       saveDirectory = plpOutput_directory)
    }
  }

  if (covariateSet == "mix"){
    
    ParallelLogger::logInfo("Loading plpData objects...")
    plpDataObjects <- list.files(plpData_directory, pattern = "*_plpData$")
    plpDataList <- lapply(plpDataObjects, function(x) loadBakedData(file = file.path(plpData_directory, x)))
    modelsList <- vector("list", length(plpDataList))
    
    for (i in seq_along(plpDataList)) {
    minSup = attributes(plpDataList[[i]]$plpData$Train$covariateData)$minimumSupport
    patLen = attributes(plpDataList[[i]]$plpData$Train$covariateData)$patternLength
    MS = gsub(pattern = "\\.", replacement = "_", x = minSup)
    
    
    modelsList[[i]] <- executeRunPlp(plpData = atemporalPlpData, 
                                     data = plpDataList[[i]]$plpData,
                                     population = plpDataList[[1]]$population, 
                                     outcomeId = outcomeId,
                                     analysisId = paste0("FPS_lasso_MS_", MS, "_PL_", patLen),
                                     analysisName = analysisName, 
                                     populationSettings = populationSettings, 
                                     splitSettings = splitSettings, 
                                     sampleSettings = sampleSettings, 
                                     # featureEngineeringSettings = settingsAppend, #note this here  
                                     preprocessSettings = preprocessSettings, 
                                     modelSettings = modelSettings, 
                                     logSettings = logSettings, 
                                     executeSettings = executeSettings, 
                                     saveDirectory = plpOutput_directory)
    }
  }
}
