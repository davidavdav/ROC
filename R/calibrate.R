calibrate <- function(x, method="logreg", prior=0.5, add.terms=NULL) {
  if (method=="logreg") {
    w <- 1/table(x$target)
    if (is.null(add.terms)) {
      lm <- glm(target ~ score, data=x, family="binomial", weight=w[as.character(target)])
      coef <- as.numeric(lm$coefficients)  # strip name as well...
      cal <- function(x) coef[1] + coef[2]*x
    } else {
      form <- eval(substitute(target ~ score + add.terms))
      lm <- glm(form, data=x, family="binomial", weight=w[as.character(target)])
      cal <- function(x, ...) predict(lm, newdata=data.frame(score=x, ...))
    }
  } else if(method=="cmlg") {
    m <- tapply(x$score, x$target, mean)
    s2 <- tapply(x$score, x$target, var)
    v <- s2 %*% c((1-prior), prior)
    a <- diff(m) / v
    b <- -a * sum(m)/2
    cal <- function(x) a*x + b
  } else {
    cal <- function(x) x
  }
  cal
}
