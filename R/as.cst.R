## bless object with class "cst", collection of supervised trials
as.cst <- function(x) {
  stopifnot("data.frame" %in% class(x), c("score", "target") %in% names(x), is.logical(x$target), is.numeric(x$score))
  class(x) <- c("cst", class(x))
  x
}
