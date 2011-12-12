\name{srekey}
\alias{srekey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Data known about existing SREs }
\description{
  This data structure contains some information from the key files of
  some evaluations in the past.  Currently this is NIST SRE 2008 and
  EVALITA 2009. 
}
\usage{
data(srekey)
}
%- maybe also 'usage' for other objects documented here.
\value{
  A list with three items.
  \item{nist.2008}{ The key information from NIST 2008 }
  \item{evalita.2009}{ The key information from EVALITA 2009 }
  \item{sre}{ A key finding the right evaluation from the model name }
}
\author{ David A. van Leeuwen }
\seealso{\code{\link{read.sre}}}
\examples{
data(srekey)
names(srekey$nist.2008)		## The meta data available for NIST 2008
levels(srekey$nist.2008$mlang)	## The languages of model speakers
}
\keyword{ print }% __ONLY ONE__ keyword per line
\keyword{ data }
