prismDoseResponse <- function(tab, nTreatments = 2, errorBars = "std.errors", lwd = 3, ylab = NULL, xlab = NULL, main = "", col = NULL) {
  
  if(is.null(col)) {
    col <- brewer.pal(max(nTreatments, 3), "Set1")
  }
  
  ##remove empty rows and columns
  tab <- tab[rowSums(is.na(tab)) != ncol(tab)]
  tab <- tab[colSums(is.na(tab)) != nrow(tab)]
  
  tab.2 <- melt(tab, id.vars = colnames(tab)[1])
  
  treatmentVector <- kmeans(sapply(colnames(tab[,-1]), agrepl, colnames(tab[,-1]), max.distance = 0.2), centers = nTreatments)$cluster
  treatmentVector.2 <- kmeans(sapply(tab.2[,"variable"], agrepl, tab.2[,"variable"], max.distance = 0.2), centers = nTreatments)$cluster
  
  treatmentList <- list()
  for(i in unique(treatmentVector)) {
    treatment <- subset(tab[,-1], select = which(treatmentVector == i))
    treatmentName <- colnames(treatment)[1]
    treatmentList[[ treatmentName ]] <- treatment
  }
  meanPoints <- do.call("rbind", lapply(treatmentList, rowMeans, na.rm = TRUE))
  errorLimits <- switch(errorBars,
                        std.dev = lapply(treatmentList, std.dev),
                        std.errors = lapply(treatmentList, std.errors),
                        conf.intervals = lapply(treatmentList, conf.intervals),
                        none = lapply(treatmentList, function(x) { rep(0, nrow(x)) }))
  errorLimits <- do.call("rbind", errorLimits)
  
  doseModel <- drm(value~X, names(treatmentList)[treatmentVector.2], fct=L.4(), data = tab.2)
  plot(doseModel, col = col, lwd = 3, bty = "l", xlab = xlab, ylab = ylab, main = main)
  
  ## we cant plot error bars with no length
  for(i in seq_along(unique(treatmentVector))) {
    keepIdx <- which(errorLimits[i,] > 0)
    arrows(tab[keepIdx,1], meanPoints[i,keepIdx] - errorLimits[i,keepIdx], tab[keepIdx,1], meanPoints[i,keepIdx] + errorLimits[i,keepIdx], angle = 90, code = 3, length = 0.1, lwd = lwd, col = col[i])
  }
}
