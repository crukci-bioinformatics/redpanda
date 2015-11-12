prismTimeSeries <- function(tab, nTreatments = 3, errorBars = "std.errors", lwd = 3, ylab = NULL, xlab = NULL, main = "", col = NULL) {
  
  ## define the plotting colours if needed
  if(is.null(col)) {
    col <- brewer.pal(max(nTreatments, 3), "Set1")
  }
  
  ##remove empty rows and columns
  tab <- tab[rowSums(is.na(tab)) != ncol(tab)]
  tab <- tab[colSums(is.na(tab)) != nrow(tab)]
  ## assume time is the first column
  treatmentList <- list()
  time <- tab[,1]
  tab <- tab[-1]
  ## try to find the replicate samples based on similar column names
  treatmentVector <- kmeans(sapply(colnames(tab), agrepl, colnames(tab), max.distance = 0.2), centers = nTreatments)$cluster
  
  for(i in unique(treatmentVector)) {
    treatment <- subset(tab, select = which(treatmentVector == i))
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
  
  ## define the plotting limits
  upper <- max(meanPoints + errorLimits, na.rm = TRUE)
  
  for(i in seq_along(unique(treatmentVector))) {
    if(i == 1) {
      plot(time, meanPoints[i,], col = col[i], ylim = c(0, upper+0.04*upper), xlim = c(0, max(time)+2), axes = FALSE, xaxs = "i", yaxs = "i", pch = 14 + i, cex = 1.5, xlab = xlab, ylab = ylab, main = main)
      axis(side = 1, lwd = 3)
      axis(side = 2, lwd = 3, las = 1)
    } else {
      points(time, meanPoints[i,], col = col[i], pch = 14 + i, cex = 1.5)
    }
    lines(x = time, y = meanPoints[i,], lwd = lwd, col = col[i])
    arrows(time, meanPoints[i,] - errorLimits[i,], time, meanPoints[i,] + errorLimits[i,], angle = 90, code = 3, length = 0.1, lwd = lwd, col = col[i])
  }
  
  legend(x = "topright", legend = names(treatmentList), pch = 14+seq_along(unique(treatmentVector)), col = col, box.lwd = 0, pt.cex = 1.5)
}
