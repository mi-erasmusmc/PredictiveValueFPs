library(PredictiveValueFPs)
library(Eunomia)
library(dplyr)
library(arules)
library(arulesSequences)
library(PatientLevelPrediction)


# Database details ---------
connectionDetails <- getEunomiaConnectionDetails()
createCohorts(connectionDetails = connectionDetails)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = 'main', 
                                                                 cdmDatabaseName = 'Eunomia', 
                                                                 cohortDatabaseSchema = 'main', 
                                                                 cohortTable = 'cohort', 
                                                                 outcomeDatabaseSchema = 'main', 
                                                                 outcomeTable = 'cohort', 
                                                                 cohortId = 1, 
                                                                 outcomeIds = 3
)


# Define minSup and pattern length values -------
minSup <- c(0.2, 0.1, 0.05)
patLength <- c(2, 3, 4, 5)


# Plp data settings -------
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  sampleSize = NULL
)

atemporalCovariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = T,
  useDemographicsGender = T,
  useConditionGroupEraLongTerm = T,
  endDays = -1,
  longTermStartDays = -365*3
)

temporalCovariateSettings <- FeatureExtraction::createTemporalSequenceCovariateSettings(useConditionOccurrence = TRUE, 
                                                                                        useDrugEraStart = TRUE, 
                                                                                        timePart = 'DAY', 
                                                                                        timeInterval = 1, 
                                                                                        sequenceStartDay = -99999, 
                                                                                        sequenceEndDay = 0)

# RunPlpSettings -------
# normalize features and remove any redundant ones
preprocessSettings <- PatientLevelPrediction::createPreprocessSettings(
  minFraction = 0.001, 
  normalize = T, 
  removeRedundancy = T
)

logSettings <- PatientLevelPrediction::createLogSettings(verbosity = 'DEBUG')

executeSettings <- PatientLevelPrediction::createExecuteSettings(
  runSplitData = T, 
  runSampleData = T, 
  runfeatureEngineering = T, 
  runPreprocessData = T, 
  runModelDevelopment = T, 
  runCovariateSummary = T
)

# split into 25% test and 75% training data, use 3-fold cross validation
# random split but keep outcome rate the same in test/train
splitSettings <- PatientLevelPrediction::createDefaultSplitSetting(
  testFraction = 0.25, 
  trainFraction = 0.75, 
  splitSeed = 123, 
  nfold = 3, 
  type = 'stratified'
)


# do no sampling
sampleSettings <- PatientLevelPrediction::createSampleSettings(
  type = 'none'  
  # type = 'underSample', 
  # sampleSeed = 123
)


# Population Settings ------
populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  binary = T, 
  firstExposureOnly = T, 
  washoutPeriod = 365, 
  removeSubjectsWithPriorOutcome = T,
  priorOutcomeLookback = 9999,
  requireTimeAtRisk = F, 
  riskWindowStart = 1, 
  riskWindowEnd = 180
)


# Model Settings -------
# use default LASSO logistic regression 
modelSettings <- PatientLevelPrediction::setLassoLogisticRegression(seed = 1234)

# PredictiveValueFPs package settings ------

runPlpSettings <- list(
  populationSettings = populationSettings, 
  splitSettings = splitSettings, 
  sampleSettings = sampleSettings, 
  preprocessSettings = preprocessSettings, 
  modelSettings = modelSettings, 
  logSettings = logSettings, 
  executeSettings = executeSettings
)

runFrequentPatternsSettings <- list(
  # temporalPlpData = temporalPlpData, 
  minimumSupportValues = minSup, 
  patternLengthValues = patLength
)

covariateSettings <- list(
  databaseDetails = databaseDetails, 
  atemporalCovariateSettings = atemporalCovariateSettings,
  temporalCovariateSettings = temporalCovariateSettings,
  restrictPlpDataSettings = restrictPlpDataSettings
)

analysisSettings <- list(
  outcomeId = 3,
  analysisId = "predicting_3",
  analysisName = "predicting_gi", 
  covariateSet = "mix"
  # atemporalPlpData = atemporalPlpData
)

# Execute study -----
PredictiveValueFPs::execute(runExtractAtemporalData = FALSE, 
                            runExtractTemporalData = FALSE, 
                            runPrepareData = FALSE, 
                            runExtractFPs = FALSE, 
                            runGenerateFPObjects = FALSE, 
                            runPrediction = TRUE,
                            runPlpSettings = runPlpSettings, 
                            runFrequentPatternsSettings = runFrequentPatternsSettings,
                            covariateSettings = covariateSettings,
                            analysisSettings = analysisSettings, 
                            saveDirectory = file.path(getwd(), "Eunomia4") )

