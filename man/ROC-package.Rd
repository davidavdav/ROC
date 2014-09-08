\name{ROC-package}
\alias{ROCs-package}
\alias{ROC}
\docType{package}
\title{
  Compute performance measures for two-class classifiers for Receiver Operating Characteristics
  analysis. 
}
\description{
  This can compute various performance measures for two-class classifiers.
  The formost important is the Receiver Operating Characteristc (ROC) curve, which shows the trade-off between false positives and false negatives when an implicit threshold is changed.  Further, it computes the ROC convex hull, which has relevance to minimum-cost detection thresholds and optimal score-to-likelihood ratio transformations.  Standard plotting routines include ROC, DetectionErorr Trade-off (DET), Applied Proability of Error (APE), Normalized Bayes Error rate (NBE), ideal score-to-Log-Likelihood-ratio (LLR), double-density and Tippet plots.  The data can be stord in a data frame with records for indicidual trials, so that additional conditions for each of the trials can represented easily.  This make per-condition analysis very easy.  Summary statistics include the Equal Error Rate (convex hull), the Cost of the Log Likelihood Ratio (Cllr), and minimum Cllr.  Score data can be numeric continuous of discrete, or in ordered factors.
}
\details{
\tabular{ll}{
Package: \tab ROC\cr
Type: \tab Package\cr
Version: \tab 0.10\cr
Date: \tab 2014-9-1\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The package provides tools for computing evaluation metrics on standard
trial sets in speaker recognition.  With \code{\link{read.tnt}} target and non-taregt scores 
can be read into an \code{cst} structure (collection of supervised trials).
Then, with \code{\link{roc}} the performance measures are computed
and a structure is prepared for plotting, and displays
basic performance measures.  \code{\link{det.plot}} will make a
DET plot.  
}
\author{
David A. van Leeuwen. 

Maintainer: <david.vanleeuwen@gmail.com>
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
\keyword{ package }
\seealso{
\code{\link{cst.tnt}}, \code{\link{roc}}, \code{\link{roc.plot}}, \code{\link{det.plot}}.   
}
\examples{
## RU submission to EVALITA speaker recognition applications track
data(ru.2009)
## inspect details of data frame
ru.2009[1,]
## look at TC6 train condition and TS2 test condition (easiest task:-)
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
## compute det statistics
r <- roc(x)
summary(r)
## and plot results
plot(r, main="RU TC6 TS1 primary submission EVALITA 2009")
det.plot(r, main="RU TC6 TS1 primary submission EVALITA 2009")
}
