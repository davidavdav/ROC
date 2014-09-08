\name{summary.det}
\alias{summary.det}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ This function prints summary information about a DET analysis }
\description{
  This function simply returns the first 10 elements of an object of
  class \code{det}, but as a data.frame. 
}
\usage{
summary.det(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{det}}
}
\value{
  A data frame with the first 10 items in \code{x}. 
}
\author{ David A. van Leeuwen }
\seealso{ \code{\link{summary}}, \code{\link{det.sre}}}
\examples{
data(ru.2009)
summary(det(ru.2009))
}
\keyword{ print }% __ONLY ONE__ keyword per line
