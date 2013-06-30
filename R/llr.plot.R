llr.plot <- function(roc) {
  stopifnot("roc" %in% class(roc))
  ptype <- ifelse(nrow(r) > 15, "l", "b")
  with(roc, plot(thres, opt.llr, xlab="score", ylab="LLR", main="Optimal score to LLR mapping",
       type=ptype))
}
