\name{as.cst}
\alias{as.cst}
\alias{cst.tnt}
\title{Create a data frame from a Collection of Supervised Trials for ROC analysis}
\description{
  These functions produce a \code{cst} object, which is a data frame blessed to be used for various 
  ROC analysis tools.  A \code{cst} object is an arbitrary data frame with at least two columns: one named \code{target}, a logical indicating to which class the trial belongs (in detection theory named a "target" vs "non-target" trial, but other fields have different names such as "same-source" vs "difference source").  The second required column is \code{score}, which is either an ordered factor or a numeric, where higer numbers indicate a higher likelihood that the trial is of the kind \code{target==TRUE}.
 
  \code{as.cst} takes as input a data frame, checks for the columns and their types, and blesses it to be of class \code{cst}.  \code{cst.tnt} takes two arguments: one for target scores and one for non-targets scores, and it produces a data frame of class \code{cst}
}
\usage{
as.cst(df)
cst.tnt(tar, non)
}
\arguments{
 \item{df}{A data frame, containing columns \code{target} and \code{score}}
 \item{tar, non}{Target and non-target scores.  Either a vector of values, or a file name that is scanned for a vector of values.}
}
\details{
  The data frame \code{df} may contain additionmal columns, which are maintained in further processing.  They can be used for conditioning the ROC analysis, e.g., in \code{plot.cond}.  

  The arguments \code{tar} and \code{non} can either refer to a vector of values or to a file name.  In the latter case, the file is opened and an attempt is made to read numeric values from the file using \code{scan}.  
}
\value{
 A data frame of class \code{cst} with at least the columns \code{target} and \code{score} of the right type. 
}
\author{David A. van Leeuwen}
\examples{
## create normally disributed target and nontarget scores around 0 and 1.
df <- data.frame(target=rep(c(F,T),each=100), score=rnorm(200, mean=rep(c(0,1), each=100)))
x <- as.cst(df)
roc(x)
## read in data from vectors
y <- cst.tnt(rnorm(100, 1), rnorm(1000, 0))
## read in data from files
z <- cst.tnt("targets.txt", "nontargets.txt")
}
