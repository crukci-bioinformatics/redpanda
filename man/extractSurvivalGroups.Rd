\name{extractSurvivalGroups}
\alias{extractSurvivalGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract a group vector for a matrix comprising both group and event data
}
\description{
A simple function designed to extract group information from a matrix that encodes both Event and Group data. The output vector can then be used in a survival analysis. 
}
\usage{
extractSurvivalGroups(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
  matrix where each column is a max of 0s and 1, or NA values. The non-NA values are taken to be the event variable for a particular group
}
}
\details{

Each column is assumed to be a different group of interest in the analysis. The observations for each group are taken to be the rows where non-NA values are observed.

}
\value{
A vector with group information for survival analysis
}

\author{
Mark Dunning
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{link{extractSurvivalEvent}}
}
\examples{

svdata <- structure(list(X = structure(c(4L, 9L, 16L, 7L, 14L, 2L, 8L, 
9L, 19L, 15L, 13L, 1L, 5L, 11L, 14L, 2L, 8L, 9L, 20L, 21L, 3L, 
12L, 6L, 17L, 22L, 18L, 10L), .Label = c("AC", "AT", "BO", "CO", 
"EB", "HO", "LT", "ME", "NT", "OO", "OT", "OW", "PL", "RE", "RK", 
"RO", "TO", "UT", "WO", "XX", "XY", "YO"), class = "factor"), 
    Days = c(78, 34, 123, 45, 234, 345, 123, 211, 356, 378, 88, 
    321, 211, 111, 156, 178, 236, 198, 211, 234, 257, 322, 344, 
    365, 245, 299, 351), Control = c(1L, 1L, 0L, 1L, 1L, 1L, 
    1L, 1L, 1L, 0L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA), Treatment.A = c(NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 
    1L, NA, NA, NA, NA, NA, NA, NA), Treatment.B = c(NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, 1L, 1L, 1L, 1L, 0L, 1L, 1L)), .Names = c("X", 
"Days", "Control", "Treatment.A", "Treatment.B"), class = "data.frame", row.names = c(NA, 
-27L))

Event <- extractSurvivalGroups(svdata[,2:3])

}