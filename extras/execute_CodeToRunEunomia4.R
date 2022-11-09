outcomeId = 3
outcomeName = "gi"
databaseName = "Eunomia"
rscriptLoc =file.path(getwd(), "extras/CodeToRunEunomia4.R")
inputDir = file.path(getwd(), "data", "Eunomia8")
outputDir = file.path(getwd(), "Eunomia11")

# rscriptLoc = file.path(getwd(), "experimentation/testing_optparse.R")

# system(paste("Rscript", rscriptLoc, "-o", outcomeId, "-c", outcomeName,"-d", databaseName ,"-i", inputDir ))
system(paste("Rscript", rscriptLoc, outcomeId, outcomeName, databaseName ,inputDir, outputDir))


