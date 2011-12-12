sre <- function(x) {
  if ("data.frame" %in% class(x))
    class(x) <- c("sre", class(x))
  x
}
