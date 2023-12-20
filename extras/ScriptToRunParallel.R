library(methods)
library(dplyr)
library(arules)
library(arulesSequences)
library(PatientLevelPrediction)
library(PredictiveValueFPs)

args = commandArgs(trailingOnly=TRUE)

print(args)

opt = list(
  o = as.numeric(args[[1]]), 
  c = args[[2]], 
  d = args[[3]],
  inputDir = args[[4]],
  outputDir = args[[5]], 
  cohortId = as.numeric(args[[6]])
)

print(opt)

##############################################################################
# Fill in the following  
#Database Details
cdmDatabaseSchema = Sys.getenv("sqlSchema")
cdmDatabaseName = opt$d
cohortDatabaseSchema = Sys.getenv("sqlSchema")
cohortTable = "plpframeworkcohort"
outcomeDatabaseSchema = Sys.getenv("sqlSchema")
cohortId = cohortId
outcomeId = opt$o
outcomeName = opt$c

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = your_database_dbms,
  server = your_server,
  port = your_port,
  user = your_user_name,
  password = your_password,
  extraSettings = your_extra_settings)

##############################################################################

#MinSup
minSup <- c(0.5, 0.2, 0.1, 0.05, 0.01)
patLength <- c(2, 3, 4)
maximumSize = 1
seed = 42

binary = T
firstExposureOnly = T
washoutPeriod = 365
removeSubjectsWithPriorOutcome = T
priorOutcomeLookback = 9999
requireTimeAtRisk = T 
riskWindowStart = 1
riskWindowEnd = 365

inputFilesDirectory = opt$inputDir
outputDirectory = opt$outputDir

# Plp data settings -------
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = F, # this is hardcoded on purpose: When extracting data make available all possible observations
  washoutPeriod = washoutPeriod, 
  sampleSize = NULL
)

atemporalCovariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = T,
  useDemographicsGender = T,
  useConditionOccurrenceLongTerm = T,
  endDays = -1,
  longTermStartDays = -365
)

temporalCovariateSettings <- FeatureExtraction::createTemporalSequenceCovariateSettings(useConditionOccurrence = TRUE, 
                                                                                        # useDrugEraStart = TRUE, 
                                                                                        timePart = 'DAY', 
                                                                                        timeInterval = 1, 
                                                                                        sequenceStartDay = -365, 
                                                                                        sequenceEndDay = -1)

# RunPlpSettings -------
# normalize features and remove any redundant ones
preprocessSettings <- PatientLevelPrediction::createPreprocessSettings(
  minFraction = 0.00001, 
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
  runCovariateSummary = F
)

# split into 25% test and 75% training data, use 5-fold cross validation
# random split but keep outcome rate the same in test/train
splitSettings <- PatientLevelPrediction::createDefaultSplitSetting(
  testFraction = 0.25, 
  trainFraction = 0.75, 
  splitSeed = seed, 
  nfold = 5, 
  type = 'stratified'
)

sampleSettings <- PatientLevelPrediction::createSampleSettings(
  type = 'underSample', 
  # type = "none",
  sampleSeed = seed
)

# Population Settings ------
populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  binary = binary, 
  firstExposureOnly = firstExposureOnly, 
  washoutPeriod = washoutPeriod, 
  removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
  priorOutcomeLookback = priorOutcomeLookback,
  requireTimeAtRisk = requireTimeAtRisk, 
  riskWindowStart = riskWindowStart, 
  riskWindowEnd = riskWindowEnd
)

# Model Settings -------
# use default LASSO logistic regression 
lassoSettings <- PatientLevelPrediction::setLassoLogisticRegression(seed = seed)
xgbSettings <- PatientLevelPrediction::setGradientBoostingMachine(seed = seed)

# PredictiveValueFPs package settings ------

runPlpSettings <- list(
  populationSettings = populationSettings, 
  splitSettings = splitSettings, 
  sampleSettings = sampleSettings, 
  preprocessSettings = preprocessSettings, 
  modelSettings = lassoSettings, 
  logSettings = logSettings, 
  executeSettings = executeSettings
)

runFrequentPatternsSettings <- list(
  minimumSupportValues = minSup, 
  patternLengthValues = patLength,
  absoluteDifference = 0.01, 
  keepDiscriminative = FALSE
)

covariateSettings <- list(
  databaseDetails = databaseDetails, 
  atemporalCovariateSettings = atemporalCovariateSettings,
  temporalCovariateSettings = temporalCovariateSettings,
  restrictPlpDataSettings = restrictPlpDataSettings, 
  saveDirectory = inputFilesDirectory
)

analysisSettings <- list(
  outcomeId = outcomeId,
  analysisId = paste0("predicting_", outcomeName),
  analysisName = outcomeName, 
  covariateSet = "mix",
  numberOfFeaturesForFeatureSelection = 200
)

# Execute study -----
PredictiveValueFPs::execute(runExtractAtemporalData = TRUE, 
                            runExtractTemporalData = TRUE, 
                            runPrepareData =TRUE, 
                            runExtractFPs = TRUE,
                            runGenerateFPObjects = TRUE,
                            runBaseline = TRUE,
                            runPrediction = TRUE,
                            runRecalibration = TRUE,
                            runPlpSettings = runPlpSettings, 
                            runFrequentPatternsSettings = runFrequentPatternsSettings,
                            covariateSettings = covariateSettings,
                            analysisSettings = analysisSettings, 
                            saveDirectory = file.path(outputDirectory)) 




