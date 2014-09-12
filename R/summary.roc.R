summary.roc <- function(x) {
  data.frame(attr(x, "stats")[1:8])
}
