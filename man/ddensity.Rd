\name{ddensity}
\alias{ddensity}
\alias{plot.ddensity}
\title{Double probability density computation and plotting}
\description{
 \code{ddensity} computes the density of the scores in a \code{cst} data frame simultaneously for target and non-target scores.  The main purpose is for plotting the densities of the scores.
}
\usage{
 ddensity(x, ...)
 plot(ddensity(x))
}
\arguments{
 \item{x}{An object of class \code{cst}, or a data frame that can be coerced into that}
 \item{...}{Additional arguments for \code{density}}
}
\details{
 \code{plot.ddensity} draws two probability density curves based on the data in the `collection of supervised trials' (\code{cst}) data frame, scaling the x- and y-axes to cover the ranges of both the target and non-target distributions.  The two distributions are drawn in blue and red, with their label near the mode of the distribution.  

 This gives a quick impression on how the scores of the ROC analysis are distributed with respect to each other. 
}
\author{David A. van Leeuwen}
\examples{
## data from TNO's submission to NIST Speaker Recogntion Evaluation 2008
data(tno.2008)
plot(ddensity(tno.2008))
}
