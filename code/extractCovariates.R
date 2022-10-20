#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(AssociationRuleMining)
library(PatientLevelPrediction)
library(arulesSequences)
library(FeatureExtraction)
library(tryCatchLog)
library(DatabaseConnector)
library(dplyr)
library(vroom)


## Here I need to source a document that specifies all parameters below in a list called params. 

# Parameters
covariateDataName <- params$covariateDataName
covariateDataLocation <- params$covariateDataLocation
outputFolder <- params$outputFolder
outcomeId <- params$outcomeId
minimumSupport <- as.numeric(params$minimumSupport)
minimumConfidence <- as.numeric(params$minimumConfidence)
maximumLength <- as.numeric(params$maximumLength)
maximumItems <- as.numeric(params$maximumItems)
sampleSize <- as.numeric(params$sampleSize)
connectionDetails <- params$connectionDetails
cdmDatabaseSchema <- params$cdmDatabaseSchema
resultsDatabaseSchema <- params$resultsDatabaseSchema
cohortDatabaseSchema <- params$cohortDatabaseSchema
outcomeDatabaseSchema <- params$outcomeDatabaseSchema
cohortTable <- params$outcomeTable
cohortId <- params$cohortId
outcomeTable <- params$outcomeTable
firstExposureOnly <- params$firstExposureOnly
sequenceEndDay <- params$sequenceEndDay
sequenceStartDay <- params$sequenceStartDay
priorOutcomeLookback = params$priorOutcomeLookback
requireTimeAtRisk = params$requireTimeAtRisk  
riskWindowStart = params$riskWindowStart
riskWindowEnd = params$riskWindowEnd
removeSubjectsWithPriorOutcome <- params$removeSubjectsWithPriorOutcome
washoutPeriod <- params$washoutPeriod
fileName <- params$fileName
databaseName <- params$databaseName

#Set up connection details
connectionDetailsv <- DatabaseConnector::createConnectionDetails()

# Specify directories if they are not there
baskets_directory <- file.path(outputFolder, "inputs", "baskets")
plpInput_directory <- file.path(outputFolder, "inputs", "predictorSets", fileName)
results_directory <- file.path(outputFolder)
FPs_directory <- file.path(outputFolder, "inputs", "FPsExtracted", fileName)

# Create directories 

if (!file.exists(outputFolder)) {
  warning("Folder '", outputFolder, "' not found. Attempting to create")
  dir.create(outputFolder)
}
if (!file.exists(file.path(outputFolder, "inputs"))) {
  dir.create(file.path(outputFolder, "inputs"))
}
if (!file.exists(file.path(outputFolder, "inputs", "predictorSets"))) {
  dir.create(file.path(outputFolder, "inputs", "predictorSets"))
}
# if (!file.exists(file.path(outputFolder, "inputs", "predictorSets", paste0("minSup_", fileName)))) {
#   dir.create(file.path(outputFolder, "inputs", "predictorSets", paste0("minSup_", fileName)))
# }
if (!file.exists(file.path(outputFolder, "inputs", "FPsExtracted"))) {
  dir.create(file.path(outputFolder, "inputs", "FPsExtracted"))
}
# if (!file.exists(file.path(outputFolder, "inputs", "FPsExtracted", paste0("minSup_", fileName)))) {
#   dir.create(file.path(outputFolder, "inputs", "FPsExtracted", paste0("minSup_", fileName)))
# }
if (!file.exists(file.path(outputFolder, "inputs", "baskets"))) {
  dir.create(file.path(outputFolder, "inputs", "baskets"))
}
if (!file.exists(file.path(outputFolder, "results"))) {
  dir.create(file.path(outputFolder, "results"))
}

# Parameters to extract data using getPlpData()
## specify database details
databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cdmDatabaseName = databaseName,
  cohortDatabaseSchema = resultsDatabaseSchema,
  cohortTable = cohortTable,
  cohortId = cohortId,
  outcomeDatabaseSchema = resultsDatabaseSchema,
  outcomeTable = outcomeTable,
  outcomeIds = outcomeId,
  cdmVersion = 5
)

## Specify 
restrictPlpDataSettings <- createRestrictPlpDataSettings(sampleSize = sampleSize)

## Specify covariate settings
covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T, 
  useDemographicsAgeGroup = T, 
  useConditionOccurrenceMediumTerm = T, 
  mediumTermStartDays = -365
)

## Extract Data

### Atemporal
plpData <- getPlpData(databaseDetails = databaseDetails, covariateSettings = covariateSettings, restrictPlpDataSettings = restrictPlpDataSettings)

savePlpData(plpData, file.path(plpInput_directory, paste0(fileName, "_", gsub(pattern = "-", replacement = "", x = sequenceStartDay), "_conditions_atemporal")))

### Temporal
temporalPlpDataSettings <- createTemporalSequenceCovariateSettings(useConditionOccurrence = TRUE,
                                                                   sequenceStartDay = sequenceStartDay, 
                                                                   sequenceEndDay = sequenceEndDay, 
                                                                   timePart = 'DAY', 
                                                                   timeInterval = 1)
temporalPlpData <- getPlpData(databaseDetails = databaseDetails, 
                              covariateSettings = temporalPlpDataSettings, 
                              restrictPlpDataSettings = restrictPlpDataSettings)
savePlpData(temporalPlpData, file.path(plpInput_directory, paste0(fileName, gsub(pattern = "-", replacement = "", x = sequenceStartDay), "_conditions_temporal")))



