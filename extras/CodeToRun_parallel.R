library(dplyr)
library(ParallelLogger)

###########################
# Fill in the following
databaseName = your_database_name
# inputDir: Where to save the inputs (can be the same as the outputDir)
inputDir = your_input_directory
# outputDir  Where to save the results
outputDir = your_output_directory
# change the number of cores if you want the analysis to run in parallel
numberOfCores = 1
###########################

cohorts <- read.csv("inst/settings/CohortsToCreate.csv") %>%
  dplyr::rename(outcomeId = cohortId) %>%
  mutate(fileName = gsub(pattern = "plp_task_", replacement = "", x = .$name), 
         fileName = dplyr::recode(fileName, ventricular_arrhythmia_and_sudden_cardiac_death_inpatient = "ventricular_arrythmia")) %>%
  filter(outcomeId != 5430) %>%
  mutate(cohortId = 5430) %>% 
  arrange(fileName)

parallelList <- vector("list", length(nrow(cohorts)))

for(i in 1:nrow(cohorts)){
  parallelList[[i]] <- list(
    outcomeIdVec = cohorts$outcomeId[i], 
    fileNames = cohorts$fileName[i],
    inputDir = paste0(inputDir, cohorts$fileName[i]),
    outputDir = paste0(outputDir, cohorts$fileName[i]), 
    cohortId = cohorts$cohortId[i]
  )
}

renderAnalysis <- function(x, databaseName){
  outcomeId <- x$outcomeIdVec
  outcomeName <- x$fileNames
  databaseName <- databaseName
  inputDir <- x$inputDir
  outputDir <- x$outputDir
  cohortId <- x$cohortId
  rscriptLoc <- file.path(getwd(), "extras", "ScriptToRunParallel.R")
  system(paste("Rscript", rscriptLoc, outcomeId,outcomeName, databaseName ,inputDir, outputDir, cohortId))
  
}

doBigJob <- function(outcomeIdsVec) {
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = numberOfCores)
  clusterApply(cluster, outcomeIdsVec, renderAnalysis, databaseName = databaseName)
  stopCluster(cluster)
  
  tt <- Sys.time() - t1
  print(tt)
  return(tt)
}

doBigJob(parallelList)

