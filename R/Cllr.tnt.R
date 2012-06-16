Cllr.tnt <- function(tar, non) {
  c.miss <- mean(log(1+exp(-tar))) / log(2)
  c.fa <- mean(log(1+exp(non))) / log(2)
  (c.miss+c.fa)/2
}
