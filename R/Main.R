#' @export
execute <- function(runExtractAtemporalData = FALSE, 
                    runExtractTemporalData = FALSE, 
                    runPrepareData = FALSE, 
                    runExtractFPs = FALSE, 
                    runGenerateFPObjects = FALSE, 
                    runBaseline = FALSE,
                    runPrediction = FALSE, 
                    runPlpSettings,
                    runFrequentPatternsSettings, 
                    analysisSettings,
                    covariateSettings,
                    saveDirectory){
  
  
  # Settings required for running plp
  populationSettings = runPlpSettings$populationSettings
  splitSettings = runPlpSettings$splitSettings
  sampleSettings = runPlpSettings$sampleSettings
  preprocessSettings = runPlpSettings$preprocessSettings
  modelSettings = runPlpSettings$modelSettings
  logSettings = runPlpSettings$logSettings
  executeSettings = runPlpSettings$executeSettings
  
  # Settings required for running FPM
  temporalPlpData = runFrequentPatternsSettings$temporalPlpData
  minMinimumSupport = min(runFrequentPatternsSettings$minimumSupportValues)
  maxPatternLength = max(runFrequentPatternsSettings$patternLengthValues)
  minimumSupportValues = runFrequentPatternsSettings$minimumSupportValues
  patternLengthValues = runFrequentPatternsSettings$patternLengthValues
  
  # Settings for analysis
  outcomeId = analysisSettings$outcomeId
  analysisId = analysisSettings$analysisId
  analysisName = analysisSettings$analysisName 
  atemporalPlpData = analysisSettings$atemporalPlpData
  fileName = stringr::str_remove(analysisName, "predicting_")
  covariateSet = analysisSettings$covariateSet
  
  # Settings for extracting covariateData
  databaseDetails = covariateSettings$databaseDetails 
  atemporalCovariateSettings = covariateSettings$atemporalCovariateSettings
  temporalCovariateSettings = covariateSettings$temporalCovariateSettings
  restrictPlpDataSettings = covariateSettings$restrictPlpDataSettings
  covariateDirectory = covariateSettings$saveDirectory
  
  if (!dir.exists(saveDirectory)){
    dir.create(saveDirectory, recursive = TRUE)
  }
  
  
  ########
  # saveDirectory <-  file.path(saveDirectory, analysisId)
  baskets_directory <- file.path(saveDirectory, analysisId, "data", "inputs", "baskets")
  bakedData_directory <- file.path(saveDirectory, analysisId, "data", "processedData")
  FPs_directory <- file.path(saveDirectory, analysisId, "data", "inputs", "minedFPs")
  itemSet_directory <- file.path(saveDirectory, analysisId, "data", "inputs", "itemsets")
  # plpInput_directory <- file.path(saveDirectory, analysisId, "data", "inputs", "predictorSets", fileName)
  plpData_directory <- file.path(saveDirectory, analysisId, "data", "inputs", "plpData")
  plpOutput_directory <- file.path(saveDirectory, analysisId, "results")
  
  # Step 0: extract covariates
  if (runExtractAtemporalData){
    PredictiveValueFPs::extractAtemporalData(databaseDetails = databaseDetails, 
                                             covariateSettings = atemporalCovariateSettings, 
                                             restrictPlpDataSettings = restrictPlpDataSettings, 
                                             outputFolder = file.path(covariateDirectory, analysisId), 
                                             fileName = fileName)
  }
  
  if (runExtractTemporalData){
    PredictiveValueFPs::extractTemporalData(databaseDetails = databaseDetails, 
                                            covariateSettings = temporalCovariateSettings, 
                                            restrictPlpDataSettings = restrictPlpDataSettings, 
                                            outputFolder = file.path(covariateDirectory, analysisId), 
                                            fileName = fileName) 
  }
  
  #step1 : pepare data with folds(for train/test) + additional recipes (sample data, tidy covariates)
  if (runPrepareData){
    PredictiveValueFPs::prepareData(runPlpSettings = runPlpSettings,
                                    analysisSettings = analysisSettings, 
                                    outputFolder = saveDirectory, 
                                    inputFolder = covariateDirectory)
  }
  
  #step2: Mine frequent patterns using the minimum minSup value and the maximum pattern length
  if (runExtractFPs)  {
    PredictiveValueFPs::extractFPs(runFrequentPatternsSettings = runFrequentPatternsSettings,
                                   outputFolder = file.path(saveDirectory, analysisId), 
                                   inputFolder = file.path(covariateDirectory, analysisId),
                                   fileName = fileName)
  }
  
  #step3: Prepare all sets of frequent patterns that are going to be examined
  if (runGenerateFPObjects){
    PredictiveValueFPs::generateFPObjects(minimumSupportValues = minimumSupportValues, 
                                          patternLengthValues = patternLengthValues, 
                                          covariateSet = covariateSet,
                                          outputFolder = file.path(saveDirectory, analysisId),
                                          fileName = fileName)
  }
  
  #step4: Predict
  if (runBaseline){
    PredictiveValueFPs::predictBaseline(runPlpSettings = runPlpSettings, 
                                        analysisSettings = analysisSettings, 
                                        outputFolder = saveDirectory, 
                                        inputFolder = file.path(covariateDirectory, analysisId))
  }
  
  if (runPrediction){
    PredictiveValueFPs::predictFPs(runPlpSettings = runPlpSettings, 
                                   analysisSettings = analysisSettings, 
                                   covariateSet = covariateSet,
                                   outputFolder = saveDirectory, 
                                   inputFolder = file.path(covariateDirectory, analysisId))
  }
}