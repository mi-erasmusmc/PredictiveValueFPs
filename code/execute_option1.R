execute <- function(runPrepareData = FALSE, 
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
  
  #step3: Prepare all sets of frequent patterns that are going to be examined
  if (runGenerateFPObjects){
    
    combs <- expand.grid(minimumSupportValues, patternLengthValues)
    settings2list <- list()
    output3list <- list()
    for (i in seq_along(1:nrow(combs))) {
    
    settings2list[[i]] <- prepareFrequentPatternsObjectSettings(minimumSupport = combs[i, 1], 
                                                     maximumPatternLength = combs[i,2], 
                                                     maximumItemSize = 1, 
                                                     removeLengthOnePatterns = TRUE,
                                                     # temporalPlpData = temporalPlpData,
                                                     transactionsObject = NULL, 
                                                     savePatterns = TRUE,
                                                     classification = FALSE, 
                                                     outputFolder = "testing/data/fpsInput", fileName = "test")
    
  
  
  output3list[[i]] <- prepareFrequentPatternsObjects(frequentPatternsObject = output2$frequentPatternsObject, featureEngineeringSettings = settings2list[[i]])
    }
  }
  
  #step4: Append all frequent pattern objects to andromeda objects
  if (runAppendAndromeda){
    
    settings4list <- list()
    output4list<- list()
    for (i in seq_along(output3list)) {
  
    settings4list[[i]] <- appendFrequentPatternsSettings(frequentPatternsObject = output3list[[i]],
                                                         inputData = output2$inputData, 
                                                         transactionsRowId = output2$transactionsRowId, 
                                                         frequentPatternMiningSettings = settings, 
                                                         outputFolder = "testing/data/plpData", 
                                                         fileName = "output")
    output4list[[i]] <- output1
    output4list[[i]] <- prepareFeatureEngineering(plpData = output1$plpData, featureEngineeringSettings = settings4list[[i]], executeSettings = executeSettings)
    }
  }
  
  #step5: Predict
  if (runPrediction){
    models <-list()
    for (i in seq_along(output4list)) {
    models[[i]] <- executeRunPlp(plpData = atemporalPlpData, 
                  data = output4list[[i]],
                  population = output1$population, 
                  outcomeId = 3,
                  analysisId = paste0("testing/Analysis", i),
                analysisName = "testing package", 
                populationSettings = populationSettings, 
                splitSettings = splitSettings, 
                sampleSettings = sampleSettings, 
                #featureEngineeringSettings = featureEngineeringSettings, 
                featureEngineeringSettings = settings4, 
                preprocessSettings = preprocessSettings, 
                modelSettings = modelSettings, 
                logSettings = logSettings, 
                executeSettings = executeSettings)
    }
  }

  
}
