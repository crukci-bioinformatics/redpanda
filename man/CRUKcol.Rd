\name{CRUKcol}
\alias{CRUKcol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to return CRUK brand colours }
\description{

This function can return the 2 primary CRUK colours and the 8 secondary CRUK colours, either as they are or as 10% or 20% tints

}
\usage{
CRUKcol(col = "Blue", tint = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{ should be one of "Blue", "Magenta","Teal","Green","Orange","Yellow","Purple","Pink","LightBlue" or "Turquoise". }
  \item{tint}{ should be 0 (default) for saturated colour, 1 for 10\% tint, 2 for 20\% tint}
}
\references{ www.cancerresearchuk.org/branding/images/pdfs/guidelines }
\author{ Andy Lynch }
\seealso{ \code{\link{Camcol}}}
\examples{
plot(rnorm(500),rnorm(500),pch=15,col=CRUKcol("Blue"))}
\keyword{color}
