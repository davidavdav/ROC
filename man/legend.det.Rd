\name{legend.det}
\alias{legend.det}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add a legnd to a DET plot }
\description{
  This function puts a legend in a DET plot.  According to what your
  typical performance is, you can place the legen either in the upper
  right corner or lower left corner. 
}
\usage{
legend.det(legend, where = "ur", order = 1:length(legend), col, lty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{legend}{ An array of legend texts}
  \item{where}{ Position to place legend in plot, "ur" or "ll" }
  \item{order}{ Order in which the legend text and lines are to be
    plotted, so allow legend order to match lines in the DET plot }
  \item{col}{ The colors of the lines, as reference to the palette}
  \item{lty}{ The line types of the lines }
}
\details{
  The main task of this function is to place the box in an easy way. 
}
\value{
  Nothing interesting (the value returned by \code{legend()}.
}
\author{ David van Leeuwen }
\seealso{ \code{plot.det()} }
\examples{
## make an empty DET frame
plot.det(NULL)
## Add a legend
legend.det(c("one system", "another system"), order=c(2,1), lty=1:2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
