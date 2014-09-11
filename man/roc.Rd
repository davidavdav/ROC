\name{roc}
\alias{roc}
\alias{as.roc}
\alias{ROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Receiver Operating Characteristic statistics from a dataframe or CST object}
\description{
  This function computes the relevant statistics for carrying out ROC
  error analysis for two-class classifiers that produce a score.  The
  input dataframe should contain at least two columns, named "target" and
  "score", respectively.  The classifier should produce scores that
  are higher for \code{target==TRUE} and lower for \code{target==FALSE}.
  \code{roc()} computes the false alarm probability and miss probability as
  a function of an implicitly swept threshold.  It further computes
  summary statistics, such as the equal error rate (EER), the cost of
  the log likelihood ratio (if the score is a log-likelihood-ratio),
  and the convex hull of the ROC, which reveals the optimal
  score-to-log-likelihood ratio transform.  The structure can be used for 
  subsequent analysis and plotting. 
}
\usage{
roc(x, laplace=TRUE)
as.roc(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame, optionally of class \code{cst}}
  \item{laplace}{A logical: should Laplace's rule of succesion be applied?  This option effectively adds scores of plus and minus infinity to both target and non-target scores, smoothing the false alarm and miss probabilities to account for unseen observations.}
  \item{y}{An object of class \code{roc}, or a data frame that can be coerced to that by using \code{roc(y)}}
}
\details{
  The data \code{x} is a data frame with at least two fields: a logical \code{target}, indicating
  whether the trial is a target or a non-target trial, and a numeric \code{score}, a value that increases with increasing likelihood that \code{target==TRUE}.  An object of class \code{cst} (collection of supervised trials) ensures that these fields are present. 
  Other columns in the
  data frame can be used to specify factors for conditioning, or other
  meta data.  Typically, a \code{cst} object is created by \code{as.cst} or
  \code{cst.tnt}.

  \code{as.roc} is used internally in places where an object of type \code{roc} is expected, ans is used as a convenience function to functions like \code{det.plot}, so that the data can be passed directly as an argument to that function.  In other words, \code{det.plot(roc(x))} and \code{det.plot(x)} can be used both.  Computing the ROC statistics is relatively computationally intensive, so it can be more efficient to use \code{roc} explicitly and store the result. 
}
\value{
  The function returns a data frame containing the important ROC statistics
  \item{pfa}{The probability of false alarm (fraction of non-target trials with a score below the threshold)}
  \item{pmiss}{The probability of a miss (fraction of target trials with a score above the threshold)}
  \item{thres}{The threshold at which \code{pfa} and \code{pmiss} are determined}
  \item{chull}{A logical indicating whether this point is on the Convex Hull of the ROC or not}
  \item{opt.llr}{The optimum log-lilelihood-ratio corresponding to scores between this threshold and the next.  It is equal to the log of the negative slope of line segments on the convex hull.}
  
  The data frame further has two additional attributes, \code{data}, the original data augmented with a column \code{opt.llr}, and \code{stats}, which contains some basic summary statistics of the ROC analysis.  
  \item{Cllr}{Cost of LLR (see [2,3])}
  \item{Cllr.min}{Minimum Cllr, computed using isotonic regression (see [2])}
  \item{EER}{The equal Error Rate, computed using the Convex Hull method}
  \item{mt}{The mean value of target scores}
  \item{mn}{The mean value of non-target scores}
  \item{nt}{The number of target trials}
  \item{nn}{The number of non-target trials}
  \item{n}{The number of trials}
  \item{discrete}{A heuristic indicating if the scores are discrete or continuous.}  
}
\references{\enumerate{
  \item Alvin Martin et al, ``The {DET} Curve in Assessment of
  Detection Task Performance,'' Proc. Interspeech, 1895--1898 (1997).
  \item Niko Br\"ummer and Johan du Preez, ``Application-independent
  evaluation of speaker detection,'' Computer,
  Speech and Language 20, 230--275, (2006).
  \item David van Leeuwen and Niko
  Br\"ummer, ``An Introduction to Application-Independent Evaluation of
  Speaker Recognition System,'' LNCS 4343 (2007).
  \item 4 Foster Provost and
  Tom Fawcett, ``Analysis and Visualization of Classifier Performance:
  Comparison under Imprecise Class and Cost Distributions,'' Third
  International Conference on Knowledge Discovery and Data Mining
  (1997).}  
}
\author{ David A. van Leeuwen}
%%\note{ ~~further notes~~ %
%%
%% ~Make other sections like Warning with \section{Warning }{....} ~
%%}
\seealso{\code{read.cst}, \code{read.tnt}, \link{\code{plor.roc}},\link{\code{det.plot}}}
\examples{
## Load example SRE data: 
## RU submission to EVALITA speaker recognition applications track
data(ru.2009)
## inspect details of data frame
head(ru.2009)
## look at TC6 train condition and TS2 test condition (easiest task:-)
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
## compute det statistics
r <- roc(x)
r
summary(r)
## and plot results
plot(r, main="RU TC6 TS1 primary submission EVALITA 2009")
det.plot(r, main="RU TC6 TS1 primary submission EVALITA 2009")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ROC }

