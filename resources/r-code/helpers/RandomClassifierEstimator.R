#
# Script to generate random labels to estimate indicators of random classifier.
#

#
# Clear workspace
#
rm(list = ls())


#
# Set language to En
#
Sys.setlocale(category = "LC_ALL", locale = "english")

#
# Libraries loading
#
libraries <- c("yardstick", "performanceEstimation", "qs")

if (length(setdiff(libraries, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = T)  
}

sapply(libraries, function(libName) { 
  library(libName, character.only = TRUE)
})


#
#
#

if (!file.exists("RandomClassDistrIndicators.qs") || !file.exists("RandomClassDistrIndicators.RData")) {
  
  # Distribution of classes taken from: http://ana.cachopo.org/datasets-for-single-label-text-categorization
  samplNo <- c(319, 389, 394, 392, 385, 392, 390, 395, 398, 397, 399, 396, 393, 396, 394, 398, 364, 376, 310, 251)
  classNames <- letters[1:length(samplNo)]
  trueLabels <- c()
  
  for (i in 1:length(samplNo)) {
    trueLabels <- append(trueLabels, rep(classNames[i], samplNo[i]))
  }
  
  trueLabels <- as.factor(trueLabels)
  
  results <- c()
  
  for (sampNo in 1:100) {
    clust <- parallel::makeCluster(parallel::detectCores() - 2)
    sampledTrueLabels  <- sample(trueLabels)
    parallel::clusterExport(clust, "sampledTrueLabels")
    parallel::clusterEvalQ(clust, "library(performanceEstimation)")
    parallel::clusterEvalQ(clust, "library(qs)")
    
    for (i in 1:100) {
      message(paste0("Iteration: ", sampNo, " ", i))
      res <- parallel::parSapply(clust, c(1:10000), function(x) {
        tSamples <- sample(sampledTrueLabels)
        indiVal <- performanceEstimation::classificationMetrics(tSamples, sampledTrueLabels)
        indiValSer <- qs::base91_encode(qs::qserialize(indiVal, preset = "custom", compress_level = 22))
        return(indiValSer)
      })
      
      results <- append(results, res)
    }
    
    parallel::stopCluster(clust)  
  }
  
  save(results, file = "RandomClassDistrIndicators.RData")
  qs::qsave(results, "RandomClassDistrIndicators.qs")
  
} else {
  if (!exists("results")) {
    message("Load data")
  } 
  
  len <- length(results)
  accs <- rep(NA, len)
  macroFs <- rep(NA, len)
  microFs <- rep(NA, len)
  
  for (i in 1:len) {
    indicators <- qdeserialize(base91_decode(results[i]))
    accs[i] <- indicators["acc"]
    macroFs[i] <- indicators["macroF"]
    microFs[i] <- indicators["microF"]
  }
  
}
