sre.tnt <- function(tar, non, thres=0) {
  score <- c(tar,non)
  target <- rep(c(T,F),c(length(tar),length(non)))
  dec <- score > thres
  o <- order(score)
  sre(data.frame(score=score[o], target=target[o], dec=dec[o]))
}
