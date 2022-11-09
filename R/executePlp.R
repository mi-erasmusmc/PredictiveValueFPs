#' @export
prepareRunPlp <- function(
    plpData,
    outcomeId = plpData$metaData$call$outcomeIds[1],
    analysisId = paste(Sys.Date(), plpData$metaData$call$outcomeIds[1], sep = '-'),
    analysisName = 'Study details',
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(
      type = 'stratified', 
      testFraction=0.25, 
      trainFraction = 0.75, 
      splitSeed=123, 
      nfold=3
    ),
    sampleSettings = PatientLevelPrediction::createSampleSettings(type = 'none'),
    featureEngineeringSettings = PatientLevelPrediction::createFeatureEngineeringSettings(type = 'none'),
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(
      minFraction = 0.001,
      normalize = T
    ),
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    logSettings = PatientLevelPrediction::createLogSettings(
      verbosity = 'DEBUG',
      timeStamp = T,
      logName = 'prepareRunPlp Log'
    ),
    executeSettings = PatientLevelPrediction::createDefaultExecuteSettings(),
    saveDirectory = getwd()
){
  
  # start log 
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- 'prepareRunPlpLog'
  logger <- do.call(PatientLevelPrediction:::createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(PatientLevelPrediction:::closeLog(logger))
  
  #check inputs + print 
  settingsValid <- tryCatch(
    {
      PatientLevelPrediction:::checkInputs(
        inputs = list(
          plpData = plpData, 
          outcomeId = outcomeId,
          populationSettings = populationSettings, 
          splitSettings = splitSettings,
          sampleSettings = sampleSettings,
          featureEngineeringSettings = featureEngineeringSettings, 
          preprocessSettings = preprocessSettings, 
          modelSettings = modelSettings,
          executeSettings = executeSettings
        )
      )
    },
    error = function(e){ParallelLogger::logError(e); return(NULL)}
  )
  
  if(is.null(settingsValid)){
    stop('Settings are invalid - check log for error message')
  }
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  # print the header in the log
  tryCatch({
    PatientLevelPrediction:::printHeader(
      plpData, 
      plpData$metaData$databaseDetails$targetId, 
      outcomeId, 
      analysisId, 
      analysisName,
      ExecutionDateTime
    )
  })
  
  # create the population
  population <- tryCatch(
    {
      do.call(
        PatientLevelPrediction::createStudyPopulation, 
        list(
          plpData = plpData,
          outcomeId = outcomeId,
          populationSettings = populationSettings, 
          population = plpData$population
        )
      )
    },
    error = function(e){ParallelLogger::logError(e); return(NULL)}
  )
  
  if(is.null(population)){
    stop('population NULL')
  }
  
  if(executeSettings$runSplitData){
    # split the data (test/train/cv) + summarise at the end
    data <- tryCatch(
      {
        PatientLevelPrediction::splitData(
          plpData = plpData,
          population = population,
          splitSettings = splitSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data)){
      stop('data NULL after splitting')
    }
    
    PatientLevelPrediction:::dataSummary(data)
  } 
  
  # small fix so that splitData() outputs object of calss 'plpData'
  class(data) <- 'plpData'
  
  if(executeSettings$runSampleData){
    # sampling
    data$Train <- tryCatch(
      {
        PatientLevelPrediction:::sampleData(
          trainData = data$Train, 
          sampleSettings = sampleSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after sample')
    }
    PatientLevelPrediction:::dataSummary(data)
  }
  
  # if(executeSettings$runfeatureEngineering){
  #   
  #   data$Train <- tryCatch(
  #     {
  #       PatientLevelPrediction:::featureEngineer(
  #         data = data$Train, 
  #         featureEngineeringSettings = featureEngineeringSettings
  #       )
  #     },
  #     error = function(e){ParallelLogger::logError(e); return(NULL)}
  #   )
  #   if(is.null(data$Train)){
  #     stop('train data NULL after feature engineering')
  #   }
  #   PatientLevelPrediction:::dataSummary(data)
  # }
  
 
  
  #  ExecutionSummary details:
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- difftime(endTime, ExecutionDateTime, units='mins')
  
  executionSummary <- list(
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ),
    TotalExecutionElapsedTime = TotalExecutionElapsedTime,
    ExecutionDateTime = ExecutionDateTime,
    Log = logSettings$logFileName # location for now
    #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  result = list(
    population = population, 
    plpData = data
  )
  
  return(result)
}

#' @export
prepareFeatureEngineering <- function(
    plpData,
    # data,
    outcomeId = plpData$metaData$call$outcomeIds[1],
    analysisId = paste(Sys.Date(), plpData$metaData$call$outcomeIds[1], sep = '-'),
    analysisName = 'Study details',
    # populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),
    # splitSettings = PatientLevelPrediction::createDefaultSplitSetting(
    #   type = 'stratified', 
    #   testFraction=0.25, 
    #   trainFraction = 0.75, 
    #   splitSeed=123, 
    #   nfold=3
    # ),
    # sampleSettings = PatientLevelPrediction::createSampleSettings(type = 'none'),
    featureEngineeringSettings = PatientLevelPrediction::createFeatureEngineeringSettings(type = 'none'),
    # preprocessSettings = PatientLevelPrediction::createPreprocessSettings(
    #   minFraction = 0.001,
    #   normalize = T
    # ),
    # modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    # logSettings = PatientLevelPrediction::createLogSettings(
    #   verbosity = 'DEBUG',
    #   timeStamp = T,
    #   logName = 'prepareRunPlp Log'
    # ),
    executeSettings = PatientLevelPrediction::createDefaultExecuteSettings()
    # saveDirectory = getwd()
    ){
  # small fix 
  data <- plpData
  
  if(executeSettings$runfeatureEngineering){

    data$Train <- tryCatch(
      {
        PatientLevelPrediction:::featureEngineer(
          data = data$Train,
          featureEngineeringSettings = featureEngineeringSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after feature engineering')
    }
    PatientLevelPrediction:::dataSummary(data)
  }
  
  return(data)
}


#' @export
executeRunPlp <- function(
    plpData,
    data,
    # temporalPlpData,
    population,
    outcomeId = plpData$metaData$call$outcomeIds[1],
    analysisId = paste(Sys.Date(), plpData$metaData$call$outcomeIds[1], sep = '-'),
    analysisName = 'Study details',
    populationSettings = createStudyPopulationSettings(),
    splitSettings = createDefaultSplitSetting(
      type = 'stratified', 
      testFraction=0.25, 
      trainFraction = 0.75, 
      splitSeed=123, 
      nfold=3
    ),
    sampleSettings = createSampleSettings(type = 'none'),
    featureEngineeringSettings = createFeatureEngineeringSettings(type = 'none'),
    preprocessSettings = createPreprocessSettings(
      minFraction = 0.001,
      normalize = T
    ),
    modelSettings = setLassoLogisticRegression(),
    logSettings = PatientLevelPrediction::createLogSettings(
      verbosity = 'DEBUG',
      timeStamp = T,
      logName = 'executeRunPlp Log'
    ),
    executeSettings = createDefaultExecuteSettings(),
    saveDirectory = getwd()
){
  
  # start log 
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- 'executeRunPlpLog'
  logger <- do.call(PatientLevelPrediction:::createLog, logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(PatientLevelPrediction:::closeLog(logger))
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  data <- data
  
  if(executeSettings$runPreprocessData){
    
    data$Train$covariateData <- tryCatch(
      {
        PatientLevelPrediction:::preprocessData(
          covariateData = data$Train$covariateData, 
          preprocessSettings = preprocessSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train$covariateData)){
      stop('train data NULL after preprocessing')
    }
    PatientLevelPrediction:::dataSummary(data)
  }
  
  model <- NULL
  prediction <- NULL
  performance <- NULL
  if(executeSettings$runModelDevelopment){
    # fit model
    settings <- list(
      trainData = data$Train, 
      modelSettings = modelSettings,
      analysisId = analysisId
    )
    
    ParallelLogger::logInfo(sprintf('Training %s model',settings$modelSettings$name))  
    model <- tryCatch(
      {
        do.call(PatientLevelPrediction::fitPlp, settings)
      },
      error = function(e) { ParallelLogger::logError(e); return(NULL)}
    )
    
    # Fix to read correct objectWithFPs
    model$preprocessing$featureEngineering$settings$covariateIDsInclude$trainPatterns <- featureEngineeringSettings$frequentPatternsObject
    
    if(!is.null(model)){
      prediction <- model$prediction
      # remove prediction from model
      model$prediction <- NULL
      
      #apply to test data if exists:
      if('Test' %in% names(data)){
        predictionTest <- tryCatch(
          {
            PatientLevelPrediction::predictPlp(
              plpModel = model, 
              plpData = data$Test,
              population = data$Test$labels
            )
          },
          error = function(e) { ParallelLogger::logError(e); return(NULL)}
        )
        
        predictionTest$evaluationType <- 'Test'
        
        if(!is.null(predictionTest)){
          prediction <- rbind(predictionTest, prediction[, colnames(prediction)!='index'])
        } 
        
        
      }
      
      # evaluate model
      performance <- tryCatch(
        {
          PatientLevelPrediction::evaluatePlp(prediction, typeColumn = 'evaluationType')
        },
        error = function(e) { ParallelLogger::logError(e); return(NULL)}
      )
    }
    
  }
  
  # Adding a fix to pass a message for the feature engineering
  minSup = attributes(data$Test$covariateData)$minimumSupport
  patLen = attributes(data$Test$covariateData)$patternLength
  model$modelDesign$featureEngineeringSettings<- list(paste0("MS ", minSup, " and patternLength ", patLen))
  
  # covariateSummary
  covariateSummaryResult <- NULL
  if(executeSettings$runCovariateSummary){
    
    if(!is.null(data$Test)){
      strata <- data.frame(
        rowId = c(
          data$Train$labels$rowId, 
          data$Test$labels$rowId 
        ),
        strataName = c(
          rep('Train', nrow(data$Train$labels)), 
          rep('Test', nrow(data$Test$labels))
        )
      )
    } else{
      strata <- data.frame(
        rowId = c( data$Train$labels$rowId ),
        strataName = c( rep('Train', nrow(data$Train$labels)) )
      )
    }
    
    variableImportance <- plpData$covariateData$covariateRef %>% 
      dplyr::mutate(covariateValue = 0) %>% 
      dplyr::select(.data$covariateId, .data$covariateValue) %>% 
      dplyr::collect()
    if(!is.null(model)){
      if(!is.null(model$covariateImportance)){
        variableImportance <- model$covariateImportance %>% dplyr::select(.data$covariateId, .data$covariateValue)
      }
    }
    
    covariateSummaryResult <- do.call(covariateSummary,   
                                      list(
                                        covariateData = plpData$covariateData,
                                        cohort = population %>% dplyr::select(.data$rowId),
                                        labels = population %>% dplyr::select(.data$rowId, .data$outcomeCount), 
                                        strata = strata,
                                        variableImportance = variableImportance,
                                        featureEngineering = NULL
                                      )
    )
    
  }
  
  #  ExecutionSummary details:
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- difftime(endTime, ExecutionDateTime, units='mins')
  
  executionSummary <- list(
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ),
    TotalExecutionElapsedTime = TotalExecutionElapsedTime,
    ExecutionDateTime = ExecutionDateTime,
    Log = logSettings$logFileName # location for now
    #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  # if model is NULL convert it to list for saving 
  if(is.null(model)){
    model <- list(noModel = T)
    attr(model, "predictionFunction") <- 'noModel'
    attr(model, "saveType") <- 'RtoJson'
    class(model) <- 'plpModel'
  }
  
  results <- list(
    #inputSetting = inputSetting, 
    executionSummary = executionSummary, 
    model = model,
    prediction = prediction,
    performanceEvaluation = performance,
    covariateSummary = covariateSummaryResult,
    analysisRef = list(
      analysisId = analysisId,
      analysisName = analysisName
    )
  )
  class(results) <- c('runPlp')
  
  ParallelLogger::logInfo("Run finished successfully.")
  
  # save the results
  ParallelLogger::logInfo(paste0('Saving PlpResult'))
  tryCatch(savePlpResult(results, file.path(analysisPath,'plpResult')),
           finally= ParallelLogger::logTrace('Done.'))
  ParallelLogger::logInfo(paste0('plpResult saved to ..\\', analysisPath ,'\\plpResult'))
  
  return(results)
  
}