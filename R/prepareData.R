#' @export
prepareData <- function(runPlpSettings,
                        analysisSettings, 
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
  
  bakedData_directory <- file.path(outputFolder, analysisId, "data", "processedData")
  plpInput_directory <- file.path(outputFolder, analysisId, "data", "inputs", "predictorSets", fileName)
  atemporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(plpInput_directory, paste0(fileName, "_atemporal")))
  temporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(plpInput_directory, paste0(fileName, "_temporal")))
  
  output1 <- prepareRunPlp(plpData = atemporalPlpData,
                           outcomeId = outcomeId,
                           analysisId = analysisId,
                           analysisName = analysisName, 
                           populationSettings = populationSettings, 
                           splitSettings = splitSettings, 
                           sampleSettings = sampleSettings, 
                           preprocessSettings = preprocessSettings, 
                           modelSettings = modelSettings, 
                           logSettings = logSettings, 
                           executeSettings = executeSettings, 
                           saveDirectory = outputFolder)
  
  saveBakedData(object = output1, file = bakedData_directory)
  
}