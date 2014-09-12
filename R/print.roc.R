print.roc <- function(x, full=FALSE, ...) {
  cat("Receiver Operating Characteristic, call = ", paste(deparse(attr(x, "call")), sep="\n", collapse="\n"), "\n")
  cat("ROC table:\n")
  if (nrow(x)<100 || full)
    print.data.frame(x, ...)
  else {
    cat(sprintf("  %d entries, head():\n", nrow(x)))
    print.data.frame(head(x))
  }    
  cat("\nBasic performance characteristics\n")
  print(data.frame(attr(x, "stats")), ...)
}

