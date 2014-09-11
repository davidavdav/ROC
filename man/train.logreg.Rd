\name{train.logreg}
\alias{train.logreg}
\alias{train.cmlg}
\title{Train calibration of probabilistic classifier}
\description{
 These functions train a transform of scores such that these can be interpreted better as a log-likelihood-ratio.  The resulting scores should have a lower Cllr.  
}
\usage{
 cal <- train.logreg(x, dep="score")
 cal <- train.cmlg(x)
}
\arguments{
 \item{x}{A data frame of class \code{cst} or something that can be coerced into that}
 \item{dep}{A string indiceting the formula the \code{target} depends on for model fitting}
}
\details{
 \code{train.logreg} performs logistic regression by fitting the generalized linear model \code{target ~ score}.  The weights of the trials are chosen inversely proportional to the class priors, so that effectively the model produces a log likelihood ratio instead of log posterior odds.  

 \code{train.cmlg} is a light-weight linear calibration function that assumes equal-variance Gaussian distribution of the target and non-target scores.
}
\value{
 A model, or a function of class \code{linearcalibration}, such that the function \code{predict} can be applied to the calibration. 
}
\author{David A. van Leeuwen}
\examples{
## These data were already calibrated
data(tno.2008)
summary(roc(tno.2008))
## now re-calibrate with a fairly complicated model
train <- subset(tno.2008, as.character(model)<65357)
tst <- subset(tno.2008, as.character(model)>=65357)
cal <- train.logreg(train, "score + mmic + tmic")
cal 
x <- transform(tst, score=predict(cal, tst))
det.plot(x)
## re-calibrate with a simple equal-variance Gaussian model
cal <- train.cmlg(train)
x <- transform(tst, score=predict(cal, tst))
summary(roc(x))
}
