#' @export
execute <- function(runExtractAtemporalData = FALSE,
                    runExtractTemporalData = FALSE,
                    runPrepareData = FALSE,
                    runExtractFPs = FALSE,
                    runExtractEPs = FALSE,
                    runGenerateFPObjects = FALSE,
                    runFeatureSelection = FALSE, 
                    runBaseline = FALSE,
                    runPrediction = FALSE,
                    runRecalibration = FALSE,
                    runPlpSettings,
                    runFrequentPatternsSettings,
                    analysisSettings,
                    covariateSettings,
                    saveDirectory) {
  # Settings required for running plp
  populationSettings <- runPlpSettings$populationSettings
  splitSettings <- runPlpSettings$splitSettings
  sampleSettings <- runPlpSettings$sampleSettings
  preprocessSettings <- runPlpSettings$preprocessSettings
  modelSettings <- runPlpSettings$modelSettings
  logSettings <- runPlpSettings$logSettings
  executeSettings <- runPlpSettings$executeSettings

  # Settings required for running FPM
  temporalPlpData <- runFrequentPatternsSettings$temporalPlpData
  minMinimumSupport <- min(runFrequentPatternsSettings$minimumSupportValues)
  maxPatternLength <- max(runFrequentPatternsSettings$patternLengthValues)
  minimumSupportValues <- runFrequentPatternsSettings$minimumSupportValues
  patternLengthValues <- runFrequentPatternsSettings$patternLengthValues

  # Settings for analysis
  outcomeId <- analysisSettings$outcomeId
  analysisId <- analysisSettings$analysisId
  analysisName <- analysisSettings$analysisName
  atemporalPlpData <- analysisSettings$atemporalPlpData
  fileName <- stringr::str_remove(analysisName, "predicting_")
  covariateSet <- analysisSettings$covariateSet
  numberOfFeatures <- analysisSettings$numberOfFeaturesForFeatureSelection

  # Settings for extracting covariateData
  databaseDetails <- covariateSettings$databaseDetails
  atemporalCovariateSettings <- covariateSettings$atemporalCovariateSettings
  temporalCovariateSettings <- covariateSettings$temporalCovariateSettings
  restrictPlpDataSettings <- covariateSettings$restrictPlpDataSettings
  covariateDirectory <- covariateSettings$saveDirectory

  if (!dir.exists(saveDirectory)) {
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
  
  # start log 
  # ParallelLogger::addDefaultFileLogger("analysisLog")
  # on.exit(ParallelLogger::unregisterLogger("analysisLog"))
  logSettings <- PatientLevelPrediction::createLogSettings(logName = "FPMLog")
  logPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- logPath
  logSettings$logFileName <- 'fpmLog'
  logger <- do.call(PatientLevelPrediction:::createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(PatientLevelPrediction:::closeLog(logger))
  # PatientLevelPrediction:::createLog(verbosity = "DEBUG", timeStamp = TRUE, logName = "FPM Log", saveDirectory = file.path(saveDirectory, analysisId),
  #                                    logFileName =  )

  # Step 0: extract covariates
  if (runExtractAtemporalData) {
    extractAtemporalData(
      databaseDetails = databaseDetails,
      covariateSettings = atemporalCovariateSettings,
      restrictPlpDataSettings = restrictPlpDataSettings,
      outputFolder = file.path(covariateDirectory, analysisId),
      fileName = fileName
    )
  }

  if (runExtractTemporalData) {
    extractTemporalData(
      databaseDetails = databaseDetails,
      covariateSettings = temporalCovariateSettings,
      restrictPlpDataSettings = restrictPlpDataSettings,
      outputFolder = file.path(covariateDirectory, analysisId),
      fileName = fileName
    )
  }
  
  gc()
  
  # step1 : pepare data with folds(for train/test) + additional recipes (sample data, tidy covariates)
  if (runPrepareData) {
    prepareData(
      runPlpSettings = runPlpSettings,
      analysisSettings = analysisSettings,
      outputFolder = saveDirectory,
      inputFolder = covariateDirectory
    )
  }

  # step2: Mine frequent patterns using the minimum minSup value and the maximum pattern length
  if (runExtractFPs) {
    extractFPs(
      runFrequentPatternsSettings = runFrequentPatternsSettings,
      outputFolder = file.path(saveDirectory, analysisId),
      inputFolder = file.path(covariateDirectory, analysisId),
      fileName = fileName
    )
  }
  
  # step2.5: Mine frequent patterns using the minimum minSup value and the maximum pattern length
  if (runExtractEPs) {
    extractEPs(
      runFrequentPatternsSettings = runFrequentPatternsSettings,
      outputFolder = file.path(saveDirectory, analysisId),
      inputFolder = file.path(covariateDirectory, analysisId),
      fileName = fileName
    )
  }
  
  gc()

  # step3: Prepare all sets of frequent patterns that are going to be examined
  if (runGenerateFPObjects) {
    generateFPObjects(
      minimumSupportValues = minimumSupportValues,
      patternLengthValues = patternLengthValues,
      covariateSet = covariateSet,
      outputFolder = file.path(saveDirectory, analysisId),
      fileName = fileName
    )
  }
  
  gc()
  # step4: Feature Selection
  if (runFeatureSelection){
    PredictiveValueFPs::performFeatureSelection(inputDirectory = plpData_directory, 
                                                outputDirectory = saveDirectory, 
                                                analysisSettings = analysisSettings, 
                                                numberOfFeatures = numberOfFeatures)
  }

  # step5: Predict
  if (runBaseline) {
    PredictiveValueFPs::predictBaseline(
      runPlpSettings = runPlpSettings,
      analysisSettings = analysisSettings,
      outputFolder = saveDirectory,
      inputFolder = file.path(covariateDirectory, analysisId)
    )
  }

  if (runPrediction) {
    predictFPs(
      runPlpSettings = runPlpSettings,
      analysisSettings = analysisSettings,
      covariateSet = covariateSet,
      outputFolder = saveDirectory,
      inputFolder = file.path(covariateDirectory, analysisId)
    )
  }

  if (runRecalibration) {
    recalibrateProbabilities(
      runPlpSettings = runPlpSettings,
      analysisSettings = analysisSettings,
      outputFolder = saveDirectory,
      inputFolder = file.path(covariateDirectory, analysisId)
    )
  }
}
