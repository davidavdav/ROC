bayes.fa.rate <- function(non, lpo) {
  n <- length(lpo)
  pfa <- vector(mode="numeric", length=n)
  for (i in 1:n) {
#    p <- sigmoid(non+lpo[i])
#    pfa[i] <- mean(1-sign(0.5-p))/2
    pfa[i] <- mean(non+lpo[i] > 0)
  }
  pfa*sigmoid(-lpo)
}
