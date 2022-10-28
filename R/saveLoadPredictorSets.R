#' @export
extractAtemporalData <- function(
    databaseDetails, 
    covariateSettings, 
    restrictPlpDataSettings, 
    outputFolder, 
    fileName
  ){
  
  inputDirectory <- file.path(outputFolder, "data", "inputs", "predictorSets", fileName)

  if (!dir.exists(inputDirectory)){
    dir.create(inputDirectory, recursive = TRUE)
  } 
  
  atemporalPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                         covariateSettings = covariateSettings, 
                                                         restrictPlpDataSettings = restrictPlpDataSettings
  )
  
  PatientLevelPrediction::savePlpData(atemporalPlpData, file.path(inputDirectory, paste0(fileName, "_atemporal")))
  
  ParallelLogger::logInfo("Finished extracting atemporal covariate data.")
  ParallelLogger::logInfo(paste0("Atemporal covariate data location:", inputDirectory))
}

#' @export
extractTemporalData <- function(
    databaseDetails, 
    covariateSettings,
    restrictPlpDataSettings, 
    outputFolder, 
    fileName
    ){
  
  inputDirectory <- file.path(outputFolder, "data", "inputs", "predictorSets", fileName)

  if (!dir.exists(inputDirectory)){
    dir.create(inputDirectory, recursive = TRUE)
  } 
  
  temporalPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                         covariateSettings = covariateSettings, 
                                                         restrictPlpDataSettings = restrictPlpDataSettings
  )
  
  PatientLevelPrediction::savePlpData(temporalPlpData, file.path(inputDirectory, paste0(fileName, "_temporal")))
  
  ParallelLogger::logInfo("Finished extracting temporal covariate data.")
  ParallelLogger::logInfo(paste0("Temporal covariate data location:", inputDirectory))
  
}