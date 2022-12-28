#' @export
createResultsDatabase <- function(databaseDetails, resultsDirectory){
  
  PatientLevelPrediction::insertResultsToSqlite(
    resultLocation = resultsDirectory, 
    cohortDefinitions = NULL,
    databaseList = PatientLevelPrediction::createDatabaseList(
      cdmDatabaseSchemas = databaseDetails$cohortDatabaseSchema,
      cdmDatabaseNames = databaseDetails$cdmDatabaseName,
      databaseRefIds = databaseDetails$cdmDatabaseId
    ),
    sqliteLocation = file.path(resultsDirectory, 'sqlite')
  )
  
  invisible(x = NULL)
}