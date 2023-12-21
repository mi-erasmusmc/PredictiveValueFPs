#' @export
recalibrateProbabilities <- function(runPlpSettings, 
                                     analysisSettings,
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
  
  # bakedPlpData_directory <- file.path(outputFolder, analysisId, "data", "processedData")
  plpOutput_directory <- file.path(outputFolder, analysisId, "results")
  recalibrationDirectory <- file.path(outputFolder, analysisId, "recalibrationResults")
  
  ParallelLogger::logInfo("Running baseline model...")
  
  ParallelLogger::logInfo("Loading plpData objects...")
  # atemporalPlpData <- PatientLevelPrediction::loadPlpData(file.path(inputDirectory, paste0(fileName, "_atemporal")))
  # bakedPlpData <- loadBakedData(file = bakedPlpData_directory)
  plpResultDir <- list.dirs(plpOutput_directory, recursive = FALSE)
  plpResultsNames <- basename(plpResultDir)
  
  for (i in seq_along(plpResultDir)) {
  
  analysisExists <- file.exists(file.path(recalibrationDirectory, plpResultsNames[i], "plpResult", "runPlp.rds"))
  
  if (!analysisExists){    
   
    plpResult <- PatientLevelPrediction::loadPlpResult(file.path(plpResultDir[i], "plpResult"))
    PatientLevelPrediction::recalibrateModel(plpResult = plpResult, 
                                             recalibrationMethod = 'internalRecalibrationInTheLarge',
                                             saveDirectory = recalibrationDirectory)
  } else {
    ParallelLogger::logInfo(paste('Analysis', analysisId, 'for outcome', analysisName, 'exists at', file.path(recalibrationDirectory)))
    }
  }
  invisible(x = NULL)
}
