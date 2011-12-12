\name{sretools-package}
\alias{sretools-package}
\alias{sretools}
\docType{package}
\title{
  Compute performance measures for Speaker Recognition Evaluations and
  make DET plots.
}
\description{
  This can compute various performance measures for an automatic speaker
  recognition system.  It knows target truth values about NIST SRE-2008 and
  EVALITA 2009 evaluations, and will read submission files and augment
  the data with target and aother meta information.  The package can
  equalized for unbalanced conditions within an evaluation, and computes
  modern evaluation measures such as \eqn{C_{llr}}.  
}
\details{
\tabular{ll}{
Package: \tab sretools\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2011-10-31\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The package provides tools for computing evaluation metrics on standard
trial sets in speaker recognition.  With \code{\link{read.sre}} a
standard submission file can be read into an \code{sre} structure.
Then, with \code{\link{det.sre}} the performance measures are computed
and a structure is prepared for plotting.  \code{\link{summary.det}}
shows basic performance measures, and \code{\link{plot.det}} will make a
DET plot.  Additionaly, APE plots can be generated using
\code{\link{ape.plot}}. 

}
\author{
David A. van Leeuwen. 

Maintainer: <david.vanleeuwen@tno.nl>
}
\references{
Alvin Martin et al, ``The {DET} Curve in Assessment of
  Detection Task Performance,'' Proc. Interspeech, 1895--1898 (1997).
  Niko Br\"ummer and Johan du Preez, ``Application-independent
  evaluation of speaker detection,'' Computer,
  Speech and Language 20, 230--275, (2006).  David van Leeuwen and Niko
  Br\"ummer, ``An Introduction to Application-Independent Evaluation of
  Speaker Recognition System,'' LNCS 4343 (2007).   Foster Provost and
  Tom Fawcett, ``Analysis and Visualization of Classifier Performance:
  Comparison under Imprecise Class and Cost Distributions,'' Third
  International Conference on Knowledge Discovery and Data Mining
  (1997).  
}
\keyword{ package }
\seealso{
\code{\link{read.sre}}, \code{\link{det.sre}}, \code{\link{plot.det}}.   
}
\examples{
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
