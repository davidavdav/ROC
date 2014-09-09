\name{logit}
\alias{logit}
\alias{sigmoid}
\title{Logistic function and sigmoid function}
\description{
 These functions compute logit(p) = log(p/(1-p)) and sigmoid(lo) = 1/(1+exp(-lo))
}
\usage{
 logit(p)
 sigmoid(lo)
}
\arguments{
 \item{p}{a probability in the range 0..1, to be converted to log-adds}
 \item{lo}{log-odds in the range -Inf..Inf, to be converted to a probability}
}
\value{
 \code{logit(p)} computes the logit of the probability \code{p}.  \code{sigmoid(lo)} computes the sigmoid of the log-odds \code{lo}
}
\author{ David A. van Leeuwen}
