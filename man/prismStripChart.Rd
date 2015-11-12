\name{prismStripChart}
\alias{prismStripChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a strip chart similar to those produced by GraphPad Prism. 
  The intention is to reproduce plots as closely as possible, without
  overwhelming the user with possible plotting options. If you find fuctionality 
  you'd like to be added please contact the package authors. 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  prismStripChart(tab, errorBars = "std.dev")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
    A data frame object.  Expected to have one column per treatment group with replicates in each row.
  }
  \item{errorBars}{
    Character string specifying the calculation to be used for drawing error bars.  Can take one of four values: "std.dev", "std.errors", "conf.intervals", or "none".  Defaults to "std.dev".
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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

