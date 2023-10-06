##' @export
#applyFeatureSelection <- function(bakedPlpData, numberOfFeatures = 100, analysisSettings, outputFolder){
#  
#  outcomeId = analysisSettings$outcomeId
#  analysisId = analysisSettings$analysisId
#  analysisName = analysisSettings$analysisName 
#  atemporalPlpData = analysisSettings$atemporalPlpData
#  fileName = stringr::str_remove(analysisName, "predicting_")
#  
#  plpData_directory <- file.path(outputFolder, analysisId, "data", "inputs", "plpData")
#  minSup = attributes(bakedPlpData$plpData$Train$covariateData)$minimumSupport
#  namePatternLength = attributes(bakedPlpData$plpData$Train$covariateData)$patternLength
#  nameMinSup = gsub(pattern = "\\.", replacement = "_", x = minSup)
#  
#  
#  ParallelLogger::logInfo("Starting Feature Selection...")
#  t1 <- Sys.time() 
#  df <- bakedPlpData$plpData$Train$covariateData$covariates %>% 
#    dplyr::collect() %>%
#    tidyr::pivot_wider(id_cols = rowId, names_from = covariateId, values_from = covariateValue, values_fill = 0) %>%
#    dplyr::inner_join(., bakedPlpData$population %>% select(rowId, outcomeCount), by = "rowId") %>%
#    dplyr::select(- rowId) %>%
#    dplyr::select(outcomeCount, everything())%>% 
#    dplyr::mutate(outcomeCount = base::ordered(base::factor(outcomeCount), levels = c("0", "1")))
#  
#  dd <- mRMRe::mRMR.data(data = df[,-1], 
#                         strata = df[,1] %>% pull())
#  
#  classic <- mRMRe::mRMR.classic(data = dd, 
#                                 target_indices = c(1), # Position of label variables
#                                 feature_count = as.numeric(numberOfFeatures) # how many features to select
#  )
#  
#  selectedFeatures <- mRMRe::solutions(classic)
#  featureNames <- classic@feature_names[unlist(selectedFeatures)]
#  tt <- t1 - Sys.time()
#  ParallelLogger::logInfo("Done Feature Selection.")
#  ParallelLogger::logInfo(paste("Total run time for Feature Selection:", tt[[1]], attr(tt, "units")))
#  
#  ParallelLogger::logInfo("Keeping selected Features in Andromeda...")
#  newBakedPlpData <- bakedPlpData
#  newBakedPlpData$plpData$Train$covariateData$covariates <- newBakedPlpData$plpData$Train$covariateData$covariates %>%
#    filter(covariateId %in% featureNames)
#  newBakedPlpData$plpData$Train$covariateData$covariateRef <- newBakedPlpData$plpData$Train$covariateData$covariateRef %>%
#    filter(covariateId %in% featureNames) 
#  attr(newBakedPlpData$plpData$Train$covariateData, "featuresSelected") <- paste0("FeatureSelected_", numberOfFeatures)
#  ParallelLogger::logInfo("Done.")
#  
#  saveBakedData(object = newBakedPlpData, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_FeatureSelected_", numberOfFeatures, "_plpData")))
#  
#  invisible(TRUE)
#}
#
##' @export
#performFeatureSelection <- function(inputDirectory, outputDirectory, analysisSettings, numberOfFeatures){
#  # inputDirectory = file.path("Arrow3", "predicting_gi", "data", "inputs", "plpData")
#  # numberOfFeatures = 25
#  directories <- list.dirs(inputDirectory, recursive = FALSE)
#  nonFeatureSelection <- stringr::str_detect(string = basename(directories), pattern = "_FeatureSelected_", negate = TRUE)
#  directories <- directories[nonFeatureSelection]
#  newFileNames <- basename(directories) %>%
#    stringr::str_remove(., pattern = "_plpData") %>%
#    paste0(., "_FeatureSelected_", numberOfFeatures, "_plpData")
#  
#  for (i in seq_along(directories)) {
#    analysisExists <- file.exists(file.path(inputDirectory, newFileNames[i]))
#    if(!analysisExists){
#      bakedPlpData <- loadBakedData(file.path(directories[i]))
#      applyFeatureSelection(bakedPlpData = bakedPlpData, 
#                            numberOfFeatures = numberOfFeatures, 
#                            analysisSettings = analysisSettings, 
#                            outputFolder = outputDirectory)
#    } else {
#      ParallelLogger::logInfo(paste0("Object ", newFileNames[i], " exists."))
#                              }
#  }
#  invisible(TRUE)
#}

#' @export
applyFeatureSelection <- function(bakedPlpData, numberOfFeatures = 20, analysisSettings, outputFolder){
  
  outcomeId = analysisSettings$outcomeId
  analysisId = analysisSettings$analysisId
  analysisName = analysisSettings$analysisName 
  atemporalPlpData = analysisSettings$atemporalPlpData
  fileName = stringr::str_remove(analysisName, "predicting_")
  
  plpData_directory <- file.path(outputFolder, analysisId, "data", "inputs", "plpData")
  minSup = attributes(bakedPlpData$plpData$Train$covariateData)$minimumSupport
  namePatternLength = attributes(bakedPlpData$plpData$Train$covariateData)$patternLength
  nameMinSup = gsub(pattern = "\\.", replacement = "_", x = minSup)
  
  
  ParallelLogger::logInfo("Starting Feature Selection...")
  t1 <- Sys.time() 
  sparseData <- bakedPlpData$plpData$Train %>% 
    PatientLevelPrediction::toSparseM(., .$labels)
    
  denseMatrix <- as.matrix(sparseData$dataMatrix)
  dataFrame <- as.data.frame(denseMatrix)
  y <- sparseData$labels$outcomeCount
  
  selection <- praznik::NJMIM(X = dataFrame, 
                              Y = y,
                              k = numberOfFeatures)
  
  featureNames <- sparseData$covariateMap %>%
    dplyr::filter(columnId %in% selection$selection) %>%
    dplyr::pull(covariateId)

  tt <- t1 - Sys.time()
  ParallelLogger::logInfo("Done Feature Selection.")
  ParallelLogger::logInfo(paste("Total run time for Feature Selection:", tt[[1]], attr(tt, "units")))
  
  ParallelLogger::logInfo("Keeping selected Features in Andromeda...")
  newBakedPlpData <- bakedPlpData
  newBakedPlpData$plpData$Train$covariateData$covariates <- newBakedPlpData$plpData$Train$covariateData$covariates %>%
    filter(covariateId %in% featureNames)
  newBakedPlpData$plpData$Train$covariateData$covariateRef <- newBakedPlpData$plpData$Train$covariateData$covariateRef %>%
    filter(covariateId %in% featureNames) 
  attr(newBakedPlpData$plpData$Train$covariateData, "featuresSelected") <- paste0("FeatureSelected_", numberOfFeatures)
  ParallelLogger::logInfo("Done.")
  
  saveBakedData(object = newBakedPlpData, file = file.path(plpData_directory, paste0(fileName, "_MS_", nameMinSup, "_PL_", namePatternLength, "_FeatureSelected_", numberOfFeatures, "_plpData")))
  
  invisible(TRUE)
}

#' @export
performFeatureSelection <- function(inputDirectory, outputDirectory, analysisSettings, numberOfFeatures){
  # inputDirectory = file.path("Arrow3", "predicting_gi", "data", "inputs", "plpData")
  # numberOfFeatures = 25
  directories <- list.dirs(inputDirectory, recursive = FALSE)
  nonFeatureSelection <- stringr::str_detect(string = basename(directories), pattern = "_FeatureSelected_", negate = TRUE)
  directories <- directories[nonFeatureSelection]
  newFileNames <- basename(directories) %>%
    stringr::str_remove(., pattern = "_plpData") %>%
    paste0(., "_FeatureSelected_", numberOfFeatures, "_plpData")
  
  for (i in seq_along(directories)) {
    analysisExists <- file.exists(file.path(inputDirectory, newFileNames[i]))
    if(!analysisExists){
      bakedPlpData <- loadBakedData(file.path(directories[i]))
      applyFeatureSelection(bakedPlpData = bakedPlpData, 
                            numberOfFeatures = numberOfFeatures, 
                            analysisSettings = analysisSettings, 
                            outputFolder = outputDirectory)
    } else {
      ParallelLogger::logInfo(paste0("Object ", newFileNames[i], " exists."))
    }
  }
  invisible(TRUE)
}

