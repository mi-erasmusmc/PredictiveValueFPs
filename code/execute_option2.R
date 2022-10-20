execute2 <- function(runPrepareData = FALSE, 
                    runExtractFPs = FALSE, 
                    runGenerateFPObjects = FALSE, 
                    runAppendAndromeda = FALSE,
                    runPrediction = FALSE, 
                    minimumSupportValues, 
                    patternLengthValues, 
                    analysisId, 
                    analysisName, 
                    runPlpSettings,
                    runFrequentPatternsSettings){
  
  
  minMinimumSupport <- min(minimumSupportValues)
  maxPatternLength <- max(patternLengthValues)
  
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
  
  #step1 : pepare data with folds(for train/test) + additional recipes (sample data, tidy covariates)
  if (runPrepareData){
    output1 <- prepareRunPlp(plpData = atemporalPlpData,
                             outcomeId = 3,
                             analysisId = "testing",
                             analysisName = "testing package", 
                             populationSettings = populationSettings, 
                             splitSettings = splitSettings, 
                             sampleSettings = sampleSettings, 
                             #featureEngineeringSettings = featureEngineeringSettings, 
                             featureEngineeringSettings = createFeatureEngineeringSettings(type="none"), 
                             preprocessSettings = preprocessSettings, 
                             modelSettings = modelSettings, 
                             logSettings = logSettings, 
                             executeSettings = executeSettings)
    
    ParallelLogger::logInfo("Done preparing plpData.")
  }
  
  #step2: Mine frequent patterns using the minimum minSup value and the maximum pattern length
  if (runExtractFPs)  {
    settings <- mineFrequentPatternsSettings(minimumSupport = minMinimumSupport, 
                                             maximumPatternLength = maxPatternLength, 
                                             maximumItemSize = 1, 
                                             removeLengthOnePatterns = TRUE,
                                             temporalPlpData = temporalPlpData,
                                             transactionsObject = NULL, 
                                             savePatterns = TRUE,
                                             classification = FALSE, 
                                             outputFolder = "testing/data/fpsOriginal", fileName = "test")
    
    output2 <- PredictiveValueFPs::mineFrequentPatterns(trainData = output1$plpData, featureEngineeringSettings = settings)
    
    ParallelLogger::logInfo(paste("Done extracting fps with minimum support value", minMinimumSupport, "(the lowest), and max pattern length", maxPatternLength,"(the highest)."))
  }
  
  #step3: Append max frequent pattern object to andromeda 
  if (runAppendAndromeda){
      
      settingsAppend <- appendFrequentPatternsSettings(frequentPatternsObject = output2$frequentPatternsObject,
                                                       inputData = output2$inputData, 
                                                       transactionsRowId = output2$transactionsRowId, 
                                                       frequentPatternMiningSettings = settings, 
                                                       outputFolder = "testing/data/plpData", 
                                                       fileName = "output")
      
      # output3 <- prepareFeatureEngineering(plpData = output1$plpData, featureEngineeringSettings = settingsAppend, executeSettings = executeSettings)
      output3 <- output1
      output3$plpData <- prepareFeatureEngineering(plpData = output1$plpData, featureEngineeringSettings = settingsAppend, executeSettings = executeSettings)
  }
  
  #step4: Prepare all sets of frequent patterns that are going to be examined
  if (runGenerateFPObjects){
    
    combs <- expand.grid(minimumSupportValues, patternLengthValues)
    freqPatsOnly <- list()
    mixCovs <- list()
    
    for (i in seq_along(1:nrow(combs))) {
      
      freqPatsOnly[[i]] <- list(output3 = output3)
      freqPatsOnly[[i]]$output3$plpData <- filterCovariateData(plpData = output3$plpData, 
                                              minimumSupport = combs[i, 1], 
                                              patternLength = combs[i, 2],
                                              createSets = "freqPatsOnly") 
      
      mixCovs[[i]] <- list(output3 = output3)
      mixCovs[[i]]$output3$plpData <- filterCovariateData(plpData = output3$plpData, 
                                              minimumSupport = combs[i, 1], 
                                              patternLength = combs[i, 2],
                                              createSets = "mix") 
    }
  }
  
  # Here is a problem with this workflow:
  # the following function, call appendFrequentPatterns() which is the featureEngineering function. 
  # However, in there, as covariateIdsInclude is save the first FPs object which has the highest number of fps, 
  # and therefore should add a fix to mine only patterns that are in the trainSet.
  # Another fox could be to add a different settingsAppend object for each run that will include the trainSet FPs. 
  
  #step5: Predict
  if (runPrediction){
    modelsFPsOnly <- list()
    modelsMix <- list()
    
    for (i in seq_along(freqPatsOnly)) {
      modelsFPsOnly[[i]] <- executeRunPlp(plpData = atemporalPlpData, 
                                   data = freqPatsOnly[[i]]$output3$plpData,
                                   population = output1$population, 
                                   outcomeId = 3,
                                   analysisId = paste0("testing/Analysis", i),
                                   analysisName = "testing package", 
                                   populationSettings = populationSettings, 
                                   splitSettings = splitSettings, 
                                   sampleSettings = sampleSettings, 
                                   #featureEngineeringSettings = featureEngineeringSettings, 
                                   featureEngineeringSettings = settingsAppend, 
                                   preprocessSettings = preprocessSettings, 
                                   modelSettings = modelSettings, 
                                   logSettings = logSettings, 
                                   executeSettings = executeSettings)
    }
    
    for (i in seq_along(modelsMix)) {
      modelsMix[[i]] <- executeRunPlp(plpData = atemporalPlpData, 
                                          data = mixCovs[[i]],
                                          population = output1$population, 
                                          outcomeId = 3,
                                          analysisId = paste0("testing/Analysis", i),
                                          analysisName = "testing package", 
                                          populationSettings = populationSettings, 
                                          splitSettings = splitSettings, 
                                          sampleSettings = sampleSettings, 
                                          #featureEngineeringSettings = featureEngineeringSettings, 
                                          featureEngineeringSettings = settingsAppend, 
                                          preprocessSettings = preprocessSettings, 
                                          modelSettings = modelSettings, 
                                          logSettings = logSettings, 
                                          executeSettings = executeSettings)
    }
  }
  
  
}
