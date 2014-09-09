\name{roc.plot}
\alias{roc.plot}
\alias{plot.roc}
\alias{det.plot}
\title{Make a Receiver Operating Characteristic plot}
\description{These are basic plotting routine of objecs of type \code{roc}, ROC and DET plots.}
\usage{
 plot(r, nr=1, chull=TRUE, type = ifelse(nrow(r) > 15, "l", "b"), 
 	 traditional = F, xlim = c(0, 1), ylim = c(0, 1), 
	 xlab = NULL, ylab = NULL, lwd = 2, ...)
 roc.plot(x, ...)
 det.plot(x, nr=1, lty=1, col=nr,  xmin=0.1, xmax=50, ymin=0.1, ymax=50,
         xlab="false alarm probability (\%)", ylab="miss probability (\%)",
         chull=F, Ninterp=10, ...)
}
\arguments{
 \item{r}{An object of class \code{roc}}
 \item{nr}{The number (color) for this plot.  \code{nr=1} starts a new plot.}
 \item{chull}{Plots the ROC convex hull instead of the ROC.}
 \item{type}{The plotting symbol/line type to use.}
 \item{traditional}{If \code{TRUE}, the true positive rate rather than it complement, the false negative rate, is drawn. }
 \item{xlim, ylim}{Plotting limits}
 \item{xlab, ylab}{x and y labels}
 \item{lwd}{plotting line width}
 \item{x}{An object of class \code{roc}, or an object that can be coerced into that.}
  \item{lty}{ Line type for this plot }
  \item{col}{ Color for this plot }
  \item{xmin}{ Minimum of the False Alarm rate, in percent }
  \item{xmax}{ Maximum of the False Alarm rate, in percent }
  \item{ymin}{ Minimum of the Miss rate, in percent }
  \item{ymax}{ Maximum of the Miss rate, in percent }
 \item{...}{Additional arguments passed to \code{plot}.}
}
\details{
 \code{plot.roc} draws a curve showing the trade-of betwee false positives (x-axis) and false negatives (y-axis) by varying an implicit threshold for the score.   Better discrimination between tatrgets and non-targets leads to a curve more to the lower left, i.e., fewer errors in general. 

 The default is to draw the trade-off between errors.  Others prefer to see the trade-off between true and false positives, for this the argument \code{traditional=TRUE} can be used. 

 A heuristic is made about the plotting as a continuous or discrete curve with points, if the number of points on the ROC (or convex hull) is more than 15 lines are used, otherwise points and lines. 

 \code{det.plot} shows the same information as a ROC plot, but on a warped probability scale.  The worping function is \code{qnorm}, the inverse of the cumulative normal distribution.  If the underlying target and non-targets score distributions are Gaussian, this results in a straight line in the DET plot.  Often, straight lines are observed in the DET plot, suggesting that the scores can be transformed such that the scores are more-or-less normally distributed.  

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
  found by moving from the measured point (false alarm rat, miss rate) in
  the graph along a line of angle \eqn{-45^\circ} until the diagonal is
  crossed.  The values of \eqn{d'} vary from 0 in the upper-right to
  about 6 in the lower left of the graph.  

}
\value{
 A data frame with the summary statistics of the ROC. 
}
\references{Alvin Martin et al, ``The {DET} Curve in Assessment of
  Detection Task Performance,'' Proc. Interspeech, 1895--1898 (1997). }
\author{David A. van Leeuwen}
\examples{
z <- cst.tnt(rnorm(100, 1), rnorm(1000, 0))
plot(roc(z))
det.plot(z)
}
