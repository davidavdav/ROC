ece.plot <- function(data) {
  if ("sre" %in% class(data)) data <- det(data) # "sre" is OK
  stopifnot("det" %in% class(data))       # otherwise must be class "det"
  x <- data$data
  plo <- seq(-10, 10, by=0.1)
  plot(plo, ece.tnt(x$score[x$target], x$score[!x$target], plo=plo), type="l", main="ECE plot", xlab="prior log odds", ylab="empirical cross entropy (bits)", col="red", lwd=2, panel.first=grid())
  lines(plo, ece.tnt(x$opt.llr[x$target], x$opt.llr[!x$target], plo=plo), col="green", lwd=2)
  lines(plo, ece.tnt(0, 0, plo=plo), col="black", lty=2, lwd=2)
}
