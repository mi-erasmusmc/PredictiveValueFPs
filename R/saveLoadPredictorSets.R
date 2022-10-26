#' @export
extractAtemporalData <- function(
    databaseDetails, 
    covariateSettings, 
    restrictPlpDataSettings, 
    outputFolder, 
    fileName
  ){
  
  plpInput_directory <- file.path(outputFolder, "data", "inputs", "predictorSets", fileName)

  if (!dir.exists(plpInput_directory)){
    dir.create(plpInput_directory, recursive = TRUE)
  } 
  
  atemporalPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                         covariateSettings = covariateSettings, 
                                                         restrictPlpDataSettings = restrictPlpDataSettings
  )
  
  PatientLevelPrediction::savePlpData(atemporalPlpData, file.path(plpInput_directory, paste0(fileName, "_atemporal")))
  
  ParallelLogger::logInfo("Finished extracting atemporal covariate data.")
  ParallelLogger::logInfo(paste0("Atemporal covariate data location:", plpInput_directory))
}

#' @export
extractTemporalData <- function(
    databaseDetails, 
    covariateSettings,
    restrictPlpDataSettings, 
    outputFolder, 
    fileName
    ){
  
  plpInput_directory <- file.path(outputFolder, "data", "inputs", "predictorSets", fileName)

  if (!dir.exists(plpInput_directory)){
    dir.create(plpInput_directory, recursive = TRUE)
  } 
  
  temporalPlpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                         covariateSettings = covariateSettings, 
                                                         restrictPlpDataSettings = restrictPlpDataSettings
  )
  
  PatientLevelPrediction::savePlpData(temporalPlpData, file.path(plpInput_directory, paste0(fileName, "_temporal")))
  
  ParallelLogger::logInfo("Finished extracting temporal covariate data.")
  ParallelLogger::logInfo(paste0("Temporal covariate data location:", plpInput_directory))
  
}