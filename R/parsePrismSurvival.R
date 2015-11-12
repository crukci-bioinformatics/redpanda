prismSurvivalCurve <- function(tab, main = NULL, xlab = NULL, ylab = NULL, col = NULL, lwd = 3) {
  
  
  time <- tab[,1]
  groups <- extractSurvivalGroups(tab[,-1])
  events <- extractSurvivalEvent(tab[,-1]) 
  
  if(is.null(col)) {
    if(is.null(col)) {
      col <- brewer.pal(max(length(unique(groups)), 3), "Set1")
    }
  }  
  
  survData <- Surv(time, events)
  
  plot(survfit(survData ~ groups), yaxs = "i", las = 1, bty = "l", lwd = lwd, col = col, main = main, ylab = ylab, xlab = xlab)
  legend(x = "topright", legend = unique(groups), fill = col, box.lwd = 0)
}


extractSurvivalGroups <- function(matrix){
  
  grpNames <- colnames(matrix)
  Group <- NULL
  for(i in 1:ncol(matrix)){
    Group[[i]] <- rep(grpNames[i], sum(!is.na(matrix[,i])))
  }
  unlist(Group)
}

extractSurvivalEvent <- function(matrix){
  
  Event <- NULL
  for(i in 1:ncol(matrix)){
    Event[[i]] <- matrix[!is.na(matrix[,i]),i]
  }
  unlist(Event)
}


