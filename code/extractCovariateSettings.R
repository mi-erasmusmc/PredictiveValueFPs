# Define parameters

cdmDatabaseSchema = cdmDatabaseSchema_mdcd
vocabDatabaseSchema = cdmDatabaseSchema_mdcd
resultsDatabaseSchema = Sys.getenv("sqlSchema")
cohortDatabaseSchema = Sys.getenv("sqlSchema")
outcomeDatabaseSchema = Sys.getenv("sqlSchema")
cohortTable = "plpframeworkcohort"
outcomeTable = "plpframeworkcohort"
firstExposureOnly = TRUE
sequenceEndDay = 0
sequenceStartDay = -365
washoutPeriod = 365
outputDirectory = file.path("E:/sioannou", "results", "depression", "MDCD", "conditions_sensitivity")
minimumSupport = 0.8
minimumConfidence = 0.8
maximumLength = 2
maximumItems = 1
washoutPeriod = 365
priorOutcomeLookback = 9999
requireTimeAtRisk = T 
riskWindowStart = 1
riskWindowEnd = 365
removeSubjectsWithPriorOutcome = TRUE
databaseName = "MDCD"
cohortId = 5430

cohorts <- read.csv("../inst/settings/CohortsToCreate.csv")
head(cohorts)

new = cohorts
# new <- cohorts %>%
#   mutate(fileName = gsub(pattern = "plp_task_", replacement = "", x = .$name), 
#          outcomePrevalence = outcomeRate_CCAE/100, 
#          trainSampleSize_Small = 2500/outcomePrevalence, 
#          trainSampleSize_Large = 5000/outcomePrevalence, 
#          totalSampleSize_Small = trainSampleSize_Small/0.75, 
#          totalSampleSize_Large = trainSampleSize_Large/0.75)
new <- new %>% filter(cohortId != 5430)
new$fileName[21] <- "ventricular_arrythmia"
new$fileName[21]

settingsList <- list(
  outcomeIdVec = new$cohortId, 
  sampleSizes =  ceiling(new$totalSampleSize_Small),
  fileNames = new$fileName
)
parallelList <- vector("list", length(settingsList[[1]]))

for (i in 1:length(settingsList[[1]])){
  parallelList[[i]] <- lapply(settingsList, '[', i)
}

params = list(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    resultsDatabaseSchema = resultsDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                    cohortTable = cohortTable,
                    cohortId = cohortId,
                    outcomeTable = outcomeTable,
                    firstExposureOnly = firstExposureOnly,
                    sequenceEndDay = sequenceEndDay,
                    sequenceStartDay = sequenceStartDay,
                    #outputFolder = analysisDirectory, 
                    #outcomeId = outcomeIdVec,
                    minimumSupport = minimumSupport,
                    minimumConfidence = minimumConfidence,
                    maximumLength = maximumLength, 
                    maximumItems = maximumItems, 
                    #sampleSize = sampleSize, 
                    washoutPeriod = 365,
                    priorOutcomeLookback = priorOutcomeLookback,
                    requireTimeAtRisk = requireTimeAtRisk,  
                    riskWindowStart = riskWindowStart, 
                    riskWindowEnd = riskWindowEnd, 
                    removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                    databaseName = databaseName
                    #fileName = fileName
)

# outcomeId <- parallelList[[1]][[1]]
# outcomeidsampleSize <- parallelList[[1]]$sampleSizes
# fileName <- parallelList[[1]]$fileNames
# analysisDirectory <-file.path(outputDirectory, paste0("predicting_", fileName))
# outputFolder <- analysisDirectory
# params <- append(renderParams, list(outcomeId =outcomeId, sampleSize = sampleSize, 
#                                     fileName = fileName, outputFolder = analysisDirectory))