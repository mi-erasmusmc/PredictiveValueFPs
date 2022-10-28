#' @export
saveBakedData <- function(object, 
                          file, 
                          envir, 
                          overwrite){
  if (missing(object)){
    stop("Must specify plpData")
  }
  if (missing(file)){
    stop("Must specify file")
  }
  # if (!inherits(x = plpData, what =  c("plpData"))){
  #   stop("Data not of class plpData")
  # }
  if(dir.exists(file.path(file, "covariates"))){
    stop('Folder to save covariates already exists...')
  }
  
  trainPath <- file.path(file, "Train")
  testPath <- file.path(file, "Test")
  
  if(!dir.exists(file)){
    dir.create(file, recursive = T)
  }
  
  if(!dir.exists(trainPath)){
    dir.create(trainPath, recursive = T)
  }
  
  if(!dir.exists(testPath)){
    dir.create(testPath, recursive = T)
  }
  
  if(is.null(object$plpData$metaData$call$sampleSize)){  # fixed a bug when sampleSize is NULL
    object$plpData$metaData$call$sampleSize <- 'NULL'
  }
  # Population
  saveRDS(object = object$population, file = file.path(file, "population.rds"))
  # Train
  Andromeda::saveAndromeda(object$plpData$Train$covariateData, file = file.path(file, "Train", "covariates"), maintainConnection = T)
  # saveRDS(plpData$timeRef, file = file.path(file, file.path("Train", "timeRef.rds")))
  # saveRDS(plpData$cohorts, file = file.path(file, file.path("Train", "cohorts.rds")))
  # saveRDS(plpData$outcomes, file = file.path(file, file.path("Train","outcomes.rds")))
  # saveRDS(object$plpData$metaData, file = file.path(file, file.path("Train","metaData.rds")))
  saveRDS(object$plpData$Train$folds, file = file.path(file, "Train","folds.rds"))
  saveRDS(object$plpData$Train$labels, file = file.path(file, "Train","labels.rds"))
  # Train metaData
  trainMetaData <- attr(object$plpData$Train, "metaData")
  saveRDS(trainMetaData, file.path(file, "metaData.rds"))
  # Test
  Andromeda::saveAndromeda(object$plpData$Test$covariateData, file = file.path(file, "Test", "covariates"), maintainConnection = T)
  saveRDS(object$plpData$Test$labels, file = file.path(file, "Test","labels.rds"))

}

#' @export
loadBakedData <- function(file, readOnly = TRUE){
  # if (!file.exists(file))
  #   stop(paste("Cannot find folder", file))
  # if (!file.info(file)$isdir)
  #   stop(paste("Not a folder", file))
  
  population <- readRDS(file = file.path(file, "population.rds"))
  metaData <- readRDS(file.path(file, "metaData.rds"))
  
  Train <- list(covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "Train", "covariates")),
                 # timeRef = readRDS(file.path(file, "timeRef.rds")),
                 # cohorts = readRDS(file.path(file, "cohorts.rds")),
                 # outcomes = readRDS(file.path(file, "outcomes.rds")),
                # metaData = readRDS(file.path(file, "Train", "metaData.rds")),
                folds = readRDS(file.path(file, "Train", "folds.rds")), 
                labels = readRDS(file.path(file, "Train", "labels.rds"))
                )
  
  attr(Train, "metaData") <- metaData
  Test <- list(covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "Test", "covariates")), 
               labels = readRDS(file.path(file, "Test", "labels.rds"))
               )
  
  plpData <- list(Train = Train, 
                  Test = Test)
  
  
  class(plpData$Train) <- "plpData"
  class(plpData$Test) <- "plpData"
  class(plpData) <- "plpData"
  
  result <- list(population = population, 
                 plpData = plpData)
  
  return(result)
}

