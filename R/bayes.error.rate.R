`bayes.error.rate` <-
function(tar, non, lpo) {
  n <- length(lpo)
  pmiss <- pfa <- vector(mode="numeric", length=n)
  for (i in 1:n) {
    p <- sigmoid(tar+lpo[i])
    pmiss[i] <- mean(1-sign(p-0.5))/2
    p <- sigmoid(non+lpo[i])
    pfa[i] <- mean(1-sign(0.5-p))/2
  }
  pmiss*sigmoid(lpo) + pfa*sigmoid(-lpo)
}

