## function similar to density() but for data frames with scores & target
ddensity <- function(x, ...) {
  stopifnot(all(c("score", "target") %in% names(x)))
  d <- density(x$score, ...)
  rx <- range(d$x)
  non.d <- density(x$score[!x$target], from=rx[1], to=rx[2], ...)
  tar.d <- density(x$score[x$target], from=rx[1], to=rx[2], ...)
  res <- list(tar.d=tar.d, non.d=non.d)
  class(res) <- "ddensity"
  res
}
