\name{ape.plot}
\alias{ape.plot}
\alias{nbe.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to make a Applied Probability of Error plot }
\description{
  These function plots one or more APE plots, given a list of \code{det}
  objects, or a single NBE plot.  In an APE plot one can observe the loss of information as a
  function of effective prior in a detection experiment.  Both the
  detection and calibration information loss are shown. 

  In an NBE plot, the error rates are normalized such that a non-informative trivial detector that outputs LR=1 for all input, has error 1.  This way of plotting zooms in on low-prior odds, allowing inspection of the calibration performance at low priors.   
}
\usage{
ape.plot(data, legend = "Ape plot", bar = T, bw = F)

nbe.plot(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ An object (or list) of class \code{det}}
  \item{legend}{ An array of strings to be used as legend }
  \item{bar}{ Do we need to make a bar plot as well }
  \item{bw}{ Use gray shades instead of colours }
}
\details{
  The data is either a list of objects or a single object of class
  \code{det}.  For each det, an APE plot is drawn.  In the APE plot one
  can observe the total error as a function of the `log prior odds'
  a.k.a. `effective prior odds'---which are determined by the cost
  function.  The red curve shows the probability of error given the
  supplied scores, interpreted as log-likelihood-ratios.  The green
  curve shows the same values after optimal calibration using isotonic
  regression to convert (uncalibrated) scores into log-likelihood
  ratios.  (For a fixed operation point cost function such as Cdet, this
  is the same as computing minCdet.)  The black dashed curve indicated
  the performance of a system based on effective prior alone, i.e.,
  ignoring information in the observed data. 

  If bar==TRUE, bars indication the area under the curves is plotted
  beneath the APE plots.  This can readily be interpreted as Cllr (red
  bar) and minCllr (green), measured in bits. 
}
\value{
  The value returned are the graphical parameters \code{par()} used.
  This may not be useful in any way.
}
\references{ Niko Br\"ummer and Johan du Preez,
  ``Application-independent evaluation of speaker detection,'' Computer,
    Speech and Language 20, 230--275, (2006).  David van Leeuwen and
    Niko Br\"ummer, ``An Introduction to Application-Independent Evaluation
    of Speaker Recognition System,'' LNCS 4343 (2007). }
\author{David A. van Leeuwen}
\seealso{\code{plot.det}}
\examples{
## example SRE 2008 data set for short2-short3 condition
data(tno.2008)
## select NIST condition 6
x <- subset(tno.2008, mtype=="phonecall" & ttype=="phonecall" &
  tmic=="phn")
## select low-error-favourite NIST condition 7
y <- subset(x, mlang=="eng" & tlang=="eng")
## compare the two
ape.plot(list(det(x), det(y)), legend=c("all phn", "english only"))
## look at the NBE plot for the English trials
nbe.plot(det(y))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

