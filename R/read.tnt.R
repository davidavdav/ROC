`read.tnt` <- function(tar, non, thres=0, ...) {
  if (is.character(tar)) tar <- scan(tar, ...)
  if (is.character(non)) non <- scan(non.file, ...)
  stopifnot(is.numeric(tar), is.numeric(non))
  x <- data.frame(score=c(tar, non), target=c(rep(T, length(tar)), rep(F, length(non))))
  x <- transform(x, dec=target>=thres)
  class(x) <- c("sre", class(x))
  x
}
