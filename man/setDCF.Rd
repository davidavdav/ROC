\name{setDCF}
\alias{setDCF}
\alias{getDCF}
\alias{actDCF}
\alias{minDCF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ These functions allows to specify the Decision Cost Function and compute the result }
\description{
  Decision cost function related functions.  A decision cost function weights the costs of false alarms and misses, and uses a prior over the target an non-target classes to compute a total expected cost. 

  The cost parameters can be set using \code{setDCF}, retrieved by \code{getDCF}.  The decision cost otself can be evalated using \code{actDCF} (for actual DCF based on Bayes decisions given the log likelihood ratio scores) and \code{minDCF} for the minimum ataainable DCF after optimal calibration of the scores. 
}
\usage{
setDCF(evaluation, prior=0.5, cfa=1, cmiss=1)
getDCF()
actDCT(x, lpo=prior.log.odds(), norm=TRUE)
minDCF(x, lpo=prior.log.odds(), norm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{evaluation}{ A stirng indicating the evaluation, ``nist'' or ``evalita'', or empty to set parameters to prior.log.odds=0}
  \item{prior}{The prior for targets in the DCF}
  \item{cfa, cmiss}{The costs of false alarms and misses in the DCF}
  \item{x}{A data frame of class \code{cst} or that can be coerced to one or a \code{roc} object}
  \item{lpo}{The log prior odds to use as the operating point}
  \item{norm}{Should the results be normalized to 1 for a classifier based on the prior only?}
}
\details{
  \code{setDCF} knows a few Speaker Recognition Evaluation DCF parameter sets. By specifiying \code{evaluation} the whole cost function can be set immediately.   Valid values are "nist" for pre-2010 cost parameters, "nist10" for SRE 2010, or "evalita" vor EVALITA cost parameters. 

  Alternatively one or more parameters \code{prior}, \code{cfa} or
  \code{cmiss} can be specified.  By giving no arguments, the DCF is reset to \code{cfa=cmiss=1} and \code{prior=0.5}, corresponding to log prior odds of 0. 

  The functions \code{actDCF} and \code{minDCF} compute the actual and minimum DCF for the currently set DCF parameters.  Alternatively, a specific operating point can be set using \code{lpo}.  
}
\author{ David A. van Leeuwen }
\seealso{ \code{\link{roc}}}
\examples{
data(ru.2009)
actDCF(ru.2009)
}
\keyword{ internal }% __ONLY ONE__ keyword per line
