\name{EER}
\alias{eer}
\title{
 Compute the equal error rate given false positive and false negative rates, using the convex hull method. 
}
\description{
 This routine interpolates the false positive and false negative rates on the convex hull of the ROC curve, to find the point where they are equal, and returns this value.  This is a convenience function for people who are only interesed in a (consistent) EER value without going through the hassle of obtaining a \code{roc} object.  
}
\usage{
  eer(pfa, pmissm index=NULL)
}
\arguments{
 \item{pfa}{probability of false alarm, or false positive rate}
 \item{pmiss}{probability of miss, or false negarive rate.  Also known as the complement of the hit rate, or one minus the true positive rate.}
 \item{index}{a vector of indices indicating which points ly on the convex hull, or \code{NULL} to indicate the convex hull is to be computed by \code{eer()}.}
}
\value{
  This value returns the interpolated value where the \code{pfa==pmiss}, for points lying on the convex hull of the ROC. 
}
\seealso{\code{\link{Cllr.tnt}}, \code{\link{pAUC}}}
\examples{
data(ru.2009)
x <- subset(ru.2009, mcond=="TC6" & tcond=="TS2")
r <- roc(x)
summary(r)
eer(r$pfa, r$pmiss, which(r$chull))
## re-compute the EER without the convex-hull intermpretation
eer(r$pfa, r$pmiss, 1:nrow(r))
}