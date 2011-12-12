\name{plot.det}
\alias{plot.det}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Make a Detection Error Trade-off plot }
\description{
  This function makes DET plot using a data structure of class
  \code{det}.  A DET plot is a form of Receiver Operating
  Characteristic, but with axes warped by the quantile function of the
  normal distribution.  DET plots are standard in Speaker Recognition
  detection analysis, but have application in other detection problems. 
}
\usage{
plot.det(x, nr = 1, lty = 1, col = nr, optimize = T, xmin = 0.1, xmax = 50, ymin = 0.1, ymax = 50, xlab = "false alarm probability (\%)", ylab = "miss probability (\%)", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a data structure of class \code{det}.  This can be obtained
    by the function \code{det.sre}.}
  \item{nr}{ The number of the line in the DET plot.  \code{nr=1}
    initiates a new plot, calls with \code{nr>1} cause the DET curve to
    be added to an existing plot. }
  \item{lty}{ Line type for this plot }
  \item{col}{ Color for this plot }
  \item{optimize}{ Remove points in the curve that do not change the
    direction of the line for efficiency.  }
  \item{xmin}{ Minimum of the False Alarm rate, in percent }
  \item{xmax}{ Maximum of the False Alarm rate, in percent }
  \item{ymin}{ Minimum of the Miss rate, in percent }
  \item{ymax}{ Maximum of the Miss rate, in percent }
  \item{xlab}{ Label for the x-axis }
  \item{ylab}{ Label for the y-axis }
  \item{\dots}{ Additional arguments for \code{plot.default} }
}
\details{
  A DET plot is a standard way of showing the discrimination performance
  of a two-class classifier.  In default axes range (0.1--50\,\%) it
  caters for a wide range in detection performance.  The axes are warped
  by the quantile function of the normal distribution, a.k.a. the probit
  function.  If the scores of target an non-target trials are
  distributed normally, this leads to a straight line in a DET plot.
  (The reverse, however, does not hold).

  The information in a DET plot needs to be prepared by the function
  \code{\link{det.sre}}, which takes a trial list and sorts scores and
  integrates the error rares, and determines various other performance
  measures.

  The DET plot contains two special points, the so-called `actual
  decision costs' and the `minimum decision costs'.  The former depends
  on the decisions taken for each trial, and the cost function (see
  \code{\link{setDCF}}.  It is indicated by a rectangle, where the width
  of the rectangle indicates the 95\,\% confidence interval for the
  decision point.  The minimum decision costs is a the operating point on
  the curve that indicates the minimum costs that could have been
  obtained using this DET and the cost function.  It is indicated by a
  circle.  

  A special value of \code{x=NULL} causes an empty DET frame to be
  plotted, with just a grid and labeled axes.

  Multiple DET curves can be made in the same plot by setting either
  \code{nr>1} or \code{lty>1}.
  
  DET plots are traditionally plotted with False Alarms (or False
  Positives) along the X-axis and Misses (of False Negatives) along the
  y-axis.  Although it is possible to change the range of the axes this
  is discouraged; the general position of the curve within the plot
  gives the experienced DET plot reader an immediate feeling for the
  discriminability of the detector.

  In psychology the measurement of miss and false alarm rates can be
  converted to a quantity \eqn{d'} (d-prime), which in a DET plot can be
  found by moving from the measure miss and false alarm rate point in
  the graph along a line of angle \eqn{-45^\circ} until the diagonal is
  crossed.  The values of \eqn{d'} vary from 0 in the upper-right to
  about 6 in the lowerl left of the graph.  
}
\value{
  The function returns a list containing the important DET statistics
  \item{Cllr}{Cost of LLR}
  \item{Cllr.min}{Minimum Cllr, computed using (weighted) isotonic regression}
  \item{EER}{The equal Error Rate, computed using the Convex Hull method}
  \item{Cdet}{The detection error costs, given the decisions in the
    trials}
  \item{Cdet.min}{The minimum dectection costs.}
  \item{mt}{The mean value of target scores}
  \item{mn}{The mean value of non-target scores}
  \item{nt}{The number of target trials}
  \item{nn}{The number of non-target trials}
  \item{n}{The number of trials}
}
\references{Alvin Martin et al, ``The {DET} Curve in Assessment of
  Detection Task Performance,'' Proc. Interspeech, 1895--1898 (1997). }
\author{ David A. van Leeuwen }
\seealso{ \code{\link{det.sre}} for generating a det object,
  \code{\link{setDCF}} for specifying the cost function,
  \code{\link{palette}} for controlling the colors of the curves. }
\examples{
## Load example SRE data:
## RU submission to EVALITA speaker recognition applications track
data(ru.2009)
setDCF("evalita")
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
plot(det(x), main="RU TC6 TS1 primary submission EVALITA 2009")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

