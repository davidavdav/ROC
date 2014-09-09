\name{llr.plot}
\alias{llr.plot}
\title{Score-to-Log-Likelihood-Ratio plot}
\description{
  This function shows the optimal score-to-log-likelihood-ratio function as is determined by computing 
  the ROC convex hull.
}
\usage{
llr.plot(x)
}
\arguments{
 \item{x}{An object of class \code{roc}, or for convenience a data frame with columns \code{target} and \code{score}}
}
\details{
 \code{llr.plot} plots the transformation from scores to log-likelihood-ratios such that the log-likelihood-ratios are optimally calibrated for the data \code{x}.  It effectively creates a \code{roc} object, and draws the relation between \code{score} and \code{opt.llr}, which is found after isotonic regression of the scores.  
}
\author{ David A. van Leeuwen}
\examples{
data(ru.2009)
llr.plot(ru.2009)
}
\keyword{ LLR }
