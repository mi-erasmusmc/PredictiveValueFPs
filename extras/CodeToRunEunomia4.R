library(Eunomia)
library(dplyr)
library(arules)
library(arulesSequences)
library(PatientLevelPrediction)
library(optparse)
library(methods)
library(PredictiveValueFPs)

args = commandArgs(trailingOnly=TRUE)

print(args)

opt = list(
  o = as.numeric(args[[1]]), 
  c = args[[2]], 
  d= args[[3]],
  inputDir = args[[4]],
  outputDir= args[[5]]
)

print(opt)

#Database Details
cdmDatabaseSchema = "main"
cdmDatabaseName = opt$d
cohortDatabaseSchema = "main"
cohortTable = 'cohort'
outcomeDatabaseSchema = 'main'
cohortId = 1
outcomeId = opt$o
outcomeName = opt$c

#MinSup
minSup <- c(0.2, 0.1, 0.05)
patLength <- c(2, 3, 4, 5)
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

print(opt)

connectionDetails = getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails = connectionDetails)

# Database details ---------
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema, 
                                                                 cdmDatabaseName = cdmDatabaseName, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema, 
                                                                 cohortTable = cohortTable, 
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema, 
                                                                 outcomeTable = cohortTable, 
                                                                 targetId = cohortId, 
                                                                 outcomeIds = outcomeId
)

# Plp data settings -------
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  sampleSize = NULL
)

atemporalCovariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = T,
  useDemographicsGender = T,
  useConditionGroupEraLongTerm = T,
  endDays = 0,
  longTermStartDays = -365
)

temporalCovariateSettings <- FeatureExtraction::createTemporalSequenceCovariateSettings(useConditionOccurrence = TRUE, 
                                                                                        useDrugEraStart = TRUE, 
                                                                                        timePart = 'DAY', 
                                                                                        timeInterval = 1, 
                                                                                        sequenceStartDay = -365, 
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
  splitSeed = seed, 
  nfold = 5, 
  type = 'stratified'
)


# do no sampling
sampleSettings <- PatientLevelPrediction::createSampleSettings(
  type = 'none'  
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
  patternLengthValues = patLength
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
  covariateSet = "mix"
)

# Execute study -----
PredictiveValueFPs::execute(runExtractAtemporalData = FALSE, 
                            runExtractTemporalData = FALSE, 
                            runPrepareData = TRUE,
                            runExtractFPs = TRUE, 
                            runGenerateFPObjects = TRUE, 
                            runPrediction = FALSE,
                            runPlpSettings = runPlpSettings, 
                            runFrequentPatternsSettings = runFrequentPatternsSettings,
                            covariateSettings = covariateSettings,
                            analysisSettings = analysisSettings, 
                            saveDirectory = file.path(outputDirectory)) 

