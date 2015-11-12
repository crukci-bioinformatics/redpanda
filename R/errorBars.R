## Functions for computing standard deviations, standard errors and 95% confidence intervals
## Each function expects to be passed a data.frame in a form where the summary function will be 
## applied to each row


std.dev <- function(data) {
  apply(data, 1, sd, na.rm = TRUE)
}

std.errors <- function(data) {
  stdDev <- apply(data, 1, sd, na.rm = TRUE)
  sqRoot <- sqrt(rowSums( !is.na(data) ))
  stdDev / sqRoot
}

conf.intervals <- function(data) {
  std.errors(data) * 1.96
}