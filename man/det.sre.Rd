\name{det.sre}
\alias{det}
\alias{det.sre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Detection Error Tradeoff statistics from SRE opbject}
\description{
  This function computes the relevant statistics of a NIST-style
  Spreaker Recognition Evaluation trial set.  The scores in the trial
  set are sorted, and Detection Error Tradeoff statistics are computed,
  including Cdet, minimum Cdet, equal error rate, Cllr and minimum
  Cllr.  The resulting class can be plotted using \code{plot.det()}. 
}
\usage{
det(x, cond)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data frame of class \code{sre} }
  \item{cond}{ An optional list of factors for which to compensate
    unballanced trial counts}
}
\details{
  The trial set \code{x} must be an SRE
  object, which is a data.frame of class ``sre'' containing at least the
  columns ``score'', `dec'' (decision) and ``target'' (whether
  this trial was a target or non-target trial).   Other columns in the
  data frame can be used to specify factors for conditioning, or other
  meta data.  Typically, an SRE object is created by \code{read.sre} or
  \code{read.tnt}.
  
  If \code{cond} is specified, the trials are split in subset according
  to the factors listed in \code{cond}.  Then the weights of the subsets
  are equalized by weighting the trials in the subset inversely
  proportional to their frequency, see \code{\link{cond.table}} and the
  refererence therein. 
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
  \item{...}{... more DET statistics ...}  
}
\references{ Alvin Martin et al, ``The {DET} Curve in Assessment of
  Detection Task Performance,'' Proc. Interspeech, 1895--1898 (1997).
  Niko Br\"ummer and Johan du Preez, ``Application-independent
  evaluation of speaker detection,'' Computer,
  Speech and Language 20, 230--275, (2006).  David van Leeuwen and Niko
  Br\"ummer, ``An Introduction to Application-Independent Evaluation of
  Speaker Recognition System,'' LNCS 4343 (2007).   Foster Provost and
  Tom Fawcett, ``Analysis and Visualization of Classifier Performance:
  Comparison under Imprecise Class and Cost Distributions,'' Third
  International Conference on Knowledge Discovery and Data Mining
  (1997).  }
\author{ David A. van Leeuwen}
%%\note{ ~~further notes~~ %
%%
%% ~Make other sections like Warning with \section{Warning }{....} ~
%%}
\seealso{\code{read.sre}, \code{read.tnt}, \code{\link{setDCF}}, \code{plot.det}}
\examples{
## Load example SRE data:
## RU submission to EVALITA speaker recognition applications track
data(ru.2009)
## inspect details of data frame
ru.2009[1,]
setDCF("evalita")
## look at TC6 train condition and TS2 test condition (easiest task:-)
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
## compute det statistics
d <- det(x)
summary(d)
## and plot results
plot(d, main="RU TC6 TS1 primary submission EVALITA 2009")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

