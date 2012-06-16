bayes.miss.rate <- function(tar, lpo) {
  n <- length(lpo)
  pmiss <- vector(mode="numeric", length=n)
  for (i in 1:n) {
#    p <- sigmoid(tar+lpo[i])
#    pmiss[i] <- mean(1-sign(p-0.5))/2
    pmiss[i] <- mean(tar+lpo[i] < 0)
  }
  pmiss*sigmoid(lpo)
}
