\name{DCF}
\alias{DCF}
\alias{setDCF}
\alias{getDCF}
\alias{actDCF}
\alias{minDCF}
\alias{log.prior.odds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ These functions allows to specify the Decision Cost Function and compute the result }
\description{
  Decision cost function related functions.  A decision cost function weights the costs of false alarms and misses, and uses a prior over the target an non-target classes to compute a total expected cost: 

  DCF = P_tar * C_miss * P_miss + (1-P_tar) * C_fa * P_fa

  The cost parameters can be set using \code{setDCF}, and retrieved by \code{getDCF}.  The effect of the cost function to minimum Bayes' decisions can be summarized in an `effective prior', or equivalently in a `prior log odds'

  prior.log.odds() = C_miss * P_tar / (C_fa * (1-P_tar))

  The decision cost otself can be evalated using \code{actDCF} (for actual DCF based on Bayes decisions given the log likelihood ratio scores) and \code{minDCF} for the minimum ataainable DCF after optimal calibration of the scores. 
}
\usage{
setDCF(evaluation, prior=0.5, cfa=1, cmiss=1)
getDCF()
prior.log.odds(dcf.p = get("DCF.p", envir = .sretoolsEnv), prior = dcf.p$prior, 
    cfa = dcf.p$cfa, cmiss = dcf.p$cmiss)
actDCT(x, plo=prior.log.odds(), norm=TRUE)
minDCF(x, plo=prior.log.odds(), norm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{evaluation}{ A stirng indicating the evaluation campaign, ``nist,'' ``nist10', ``nist12'' or ``evalita'', defining the cost parameters, or empty to set parameters to \code{prior.log.odds=0}}
  \item{prior}{The prior P_tar for targets in the DCF}
  \item{cfa, cmiss}{The costs of false alarms and misses in the DCF}
  \item{dcf.p}{A list containing a DCF parameter set}
  \item{x}{A \code{coc} object, or a data frame of class \code{cst} or that can be coerced to one}
  \item{plo}{Thel prior log odds to use as the operating point}
  \item{norm}{Should the results be normalized to 1 for a classifier based on the prior only?}
}
\details{
  \code{setDCF} knows a few Speaker Recognition Evaluation DCF parameter sets. By specifiying \code{evaluation} the whole cost function can be set immediately.   Valid values are "nist" for pre-2010 cost parameters, "nist10" for SRE 2010, or "evalita" for EVALITA cost parameters.  Please note that the evaluation "nist12" sets two operating points.  

  Alternatively one or more parameters \code{prior}, \code{cfa} or
  \code{cmiss} can be specified.  By giving no arguments, the DCF is reset to \code{cfa=cmiss=1} and \code{prior=0.5}, corresponding to log prior odds of 0. 

  The functions \code{actDCF} and \code{minDCF} compute the actual and minimum DCF for the currently set DCF parameters.  Alternatively, a specific operating point can be set using \code{lpo}, this may be a vector of values.  Please note that \code{minDCF} computes the ROC, so it is more efficient to pass a \code{roc} structure if that has already been computed. 

  \code{prior.log.odds} returns the prior log odds corresponding to the currently set DCF parameters. 
}
\author{ David A. van Leeuwen }
\seealso{ \code{\link{roc}}}
\examples{
data(ru.2009)
actDCF(ru.2009)
}
\keyword{ internal }% __ONLY ONE__ keyword per line
