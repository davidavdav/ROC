\name{tippet.plot}
\alias{tippet.plot}
\title{Draw a Tippet plot for a forensic comparison system}
\description{
  This function draws a Tippet plot for a forensic comparison system, given data in a \code{cst} type data frame.  Tippet plots are used in the forensic science literature, although the present author does not believe they are very insightful.}
\usage{
tippet.plot(x, ...)
}
\arguments{
 \item{x}{An object of class \code{roc}, or a data frame that can be coerced to that by using \code{roc(x}}
 \item{...}{Further arguments to \code{plot} and \code{lines}}
}
\details{
 A tippet plot shows both the true positive and the true negative rate as a function of score threshold.  The x-axis is shown on a logarithmic scale, with the title "LR bigger than" indicating the cumulative nature of the y-axis.  

 The Tippet plot shows the same basic information as a ROC or DET plot, but explicitly shows the score axis, which is deemed a Likelihood Ratio.  However, it is very hard to evaluate if the LR distribution has the correct properties.  For a Tippet plot, for instance, the slopes at log LR = 0 for the two curves should be the same for well calibrated LRs.  
}
\author{ David A. van Leeuwen}
\examples{
## data from TNO's submission to NIST Speaker Recogntion Evaluation 2008
data(tno.2008)
tippet.plot(tno.2008)
}
