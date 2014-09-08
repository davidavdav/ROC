\name{roc.plot}
\alias{roc.plot}
\alias{plot.roc}
\title{Make a Receiver Operating Characteristic plot}
\description{This is a basic plotting routine of objecs of type \code{roc}}
\usage{
 plot(r, n=1, chull=TRUE, type = ifelse(nrow(r) > 15, "l", "b"), traditional = F, xlim = c(0, 1), ylim = c(0, 1), xlab = NULL, ylab = NULL, lwd = 2, ...)
 roc.plot(x, traditional=FALSE, ...)
}
\arguments{
 \item{r}{An object of class \code{roc}}
 \item{n}{The number (color) for this plot.  \code{n=1} starts a new plot.}
 \item{x}{An object of class \code{roc}, or an object that can be coerced into that.}
 \item{traditional}{If \code{TRUE}, the true positive rate rather than it complement, the false negative rate, is drawn. }
 \item{...}{Further arguments passed to \code{plot}.}
}
\details{
 This function draws a curve showing the trade-of betwee false positives (x-axis) and false negatives (y-axis) by varying an implicit threshold for the score.   Better discrimination betwen tatrgets and non-targets leads to a curve more to the lower left, i.e., fewer errors in general. 

 The default is to draw the trade-off between errors.  Others prefer to see the trade-off between true and false positives, for this the argument \code{traditional=FALSE} can be used. 

 A heuristic is made about the plotting as a continuous or discrete curve with points, if the number of points on the ROC after 
}
\value{
 A data frame with the summary statistics of the ROC. 
}
\author{David A. van Leeuwen}
\examples{
z <- cst.tnt(rnorm(100, 1), rnorm(1000, 0))
plot(roc(z))
}
