## bless object with class "sre"
sre <- function(x) {
  stopifnot("data.frame" %in% class(x), c("score", "target") %in% names(x))
  if (! "dec" %in% names(x)) x$dec <- x$score>0
  class(x) <- c("sre", class(x))
  x
}
