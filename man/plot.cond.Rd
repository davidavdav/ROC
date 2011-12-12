\name{plot.cond}
\alias{plot.cond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot several DET plots from a single trial list, analyzed per condition }
\description{
  This function plots DET curves for all conditions in a multi-condition
  trial set, such as NIST SRE-2008.  An overall DET curve, equalizing
  trial counts, is also plotted. 
}
\usage{
plot.cond(x, cond, nr = 1, equalize = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data.frame of class \code{sre}/ }
  \item{cond}{ A list of factors specifying the conditions }
  \item{nr}{ The number (color) of the first line. }
  \item{equalize}{ Do we equalize the overall DET curve? }
  \item{\dots}{ further parameters for \code{plot.det()} }
}
\details{
  For each condition determined by the list of factors, a separate DET
  will be plotted in a new color.  The overall condition is plotterd in
  black, as a last curve so that it will be ``on top of'' the other
  curves.  You may find yourself soon to be
  running out of sensible colors.

  It is possible to specify \code{equalize=FALSE} purely for
  demonstration purposes, we don't believe this makes very much sense. 
}
\value{
  A table with a summary of the main detection performance metrics for
  each condition, and averaged over all conditions (but equalized for trial
  counts). 
}
\references{ David A. van Leeuwen, ``Overal performance metrics for
    multi-condition Speaker Recognition Evaluations,''
    Proc. Interspeech,
    Brighton, September 2009, pp 908--911.}
\author{ David van Leeuwen }
\seealso{ \code{cond.table}, \code{plot.det} }
\examples{
## load data
data(tno.2008)
## plot for all sensible conditions
plot.cond(tno.2008, list(mtype, ttype, tmic))
## another eval
data(ru.2009)
setDCF("evalita")
## we can plot fol all train/test conditions in one go...
plot.cond(ru.2009, list(mcond, tcond))
## or look at gender effect, or channel
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
plot.cond(x, gender)
plot.cond(x, channel)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
