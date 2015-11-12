\name{prismDoseResponse}
\alias{prismDoseResponse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a dose response plot similar to those produced by GraphPad Prism.
}
\description{
 Our attempt to create a dose response plot that looks similar to those produced by 
 GraphPad Prism. The intention is to reproduce plots as closely as possible, without
 overwhelming the user with possible plotting options. If you find fuctionality you'd like
 to be added please contact the package authors. 
}
\usage{
  prismDoseResponse(tab, nTreatments = 2, errorBars = "std.errors", lwd = 3, 
                    ylab = NULL, xlab = NULL, main = "", col = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
    A data frame object.  Expected to have one row per time point, with a column for each sample in the study.  The first column is expected to be the time and all subsequent columns are expected to be measurments.
  }
  \item{nTreatments}{
    Integer value specifying the number of treatments.
  }
  \item{errorBars}{
    Character string specifying the calculation to be used for drawing error bars.  Can take one of four values: "std.dev", "std.errors", "conf.intervals", or "none".  Defaults to "std.errors".
  }
  \item{lwd}{
    Integer specifying the thickness of the lines.  Increasing this value will generate thicker lines.
  }
  \item{ylab}{
    Label for the y axis.
  }
  \item{xlab}{
    Label for the x axis.
  }
  \item{main}{
    Title for the plot.
  }
  \item{col}{
    Specify colours for the curves and points.
  }
}
\details{
This plot is based heavily on the drc package.  If it doesn't look how you'd hope and the options to alter the plot in the way you'd like are not available, then consider using that package directly as it has many more options than you can currently access here.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Mike L. Smith
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plotting }
