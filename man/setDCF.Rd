\name{setDCF}
\alias{setDCF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ This function allows to specify the Decision Cost Function }
\description{
  The decision cost function is typically specified for a particular
  evaluation, and does not change all the time.  Hence we keep the
  parameters as globals in an environment \code{.sretoolsEnv}.  This
  function allows to change the parameters. 
}
\usage{
setDCF(evaluation, prior=0.5, cfa=1, cmiss=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{evaluation}{ A stirng indicating the evaluation, ``nist'' or ``evalita''
 }
 \item{prior}{ the target prior }
 \item{cfa}{the cost of a false alarm }
 \item{cmiss}{ the cost of a miss}
}
\details{
  By specifiying \code{evaluation} the whole cost function can be set
  immediately.

  Alternatively one or more parameters \code{prior}, \code{cfa} or
  \code{cmiss} can be specified. 
}
\author{ David A. van Leeuwen }
\seealso{ \code{\link{det.sre}}}
\examples{
data(ru.2009)
summary(det(ru.2009))
}
\keyword{ internal }% __ONLY ONE__ keyword per line
