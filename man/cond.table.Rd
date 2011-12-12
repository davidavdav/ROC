\name{cond.table}
\alias{cond.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute a contingency table from an sre object }
\description{
  For averaging detection trial lists that were obtained from different
  experimental conditions, the amounts of trials in these conditions
  should be weighted.  This function computes a contingency table from
  an \code{sre} object.
}
\usage{
cond.table(x, cond, target = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{sre}}
  \item{cond}{ A list of factors making up the various conditions }
  \item{target}{ Is column \code{target} to be seen as a factor? }
}
\details{
  This function produces a table of the number of trials for each
  condition in a trial list.  Conditions are specified by all
  combinations of all levels of the factors specified in \code{cond}.
  Conditions with no trials are left out of the table.

  This table can be used in equalizing the influence of different
  implicit conditions in a larger set of trials, such as NIST SRE-2008.
  In order to do this properly, the analysis needs to be carried out
  separately for target and non-target trials, this can be specified by
  \code{target=TRUE}.

  This function is used by the functions \code{det.sre()}  and
  \code{plot.cond(), but can be used separately as well.}
}
\value{
  A data.frame with a columns for each factor, specifying the condition,
  and the column \code{Freq} that indicated the trial count. 
}
\references{ David A. van Leeuwen, ``Overal performance metrics for
  multi-condition Speaker Recognition Evaluations,'' Proc. Interspeech,
  Brighton, September 2009, pp 908--911.}
\author{ David van Leeuwen }
\seealso{ \code{det.sre()}, \code{plot.cond}}
\examples{
## a NIST 2008 SRE data set
data(tno.2008)
## Analyse by gender
cond.table(tno.2008, gender)
#  gender  Freq
#f      f 59343
#m      m 39433
## Analyse by acoustic condition
cond.table(tno.2008, list(mtype, mmic, ttype, tmic))
#       mtype mmic     ttype tmic  Freq
#1  interview  mic interview  mic 34181
#4  phonecall  phn interview  mic  7350
#8  phonecall  phn phonecall  mic  8454
#13 interview  mic phonecall  phn 11741
#16 phonecall  phn phonecall  phn 37050
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

