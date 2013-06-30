cst.tnt <- function(tar, non, thres=NULL) {
  if (is.character(tar)) tar <- scan(tar)
  if (is.character(non)) non <- scan(non)
  stopifnot(is.numeric(tar), is.numeric(non))
  score <- c(tar,non)
  target <- rep(c(T,F),c(length(tar),length(non)))
  o <- order(score)
  x <- as.cst(data.frame(score=score[o], target=target[o]))
  if (!is.null(thres))
    x$dec <- x$score > thres
  x           
}
