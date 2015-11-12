\name{Camcol}
\alias{Camcol}
\title{The Cambridge University Brand colours}
\description{
Offering access to the six colours that provide a strong and distinctive means of identity for the University}
\usage{
Camcol(col = "LightBlue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{ should be one of "LightBlue", "DeepBlue", "Red", "Yellow", "Green" or "Burgundy" }
}
\value{
Returns the value associated with the colour for use with col=XXXX statements.
}
\references{ http://www.cam.ac.uk/cambuniv/style/five.html }
\author{ Andy Lynch }

\seealso{ \code{\link{CRUKcol}}}
\examples{
plot(rnorm(500),rnorm(500),pch=15,col=Camcol("LightBlue"))}
\keyword{color}
