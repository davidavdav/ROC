`read.tnt` <- function(tar.file, non.file, thres=0) {
  tar <- scan(tar.file)
  non <- scan(non.file)
  x <- data.frame(score=c(tar, non), target=c(rep(T, length(tar)), rep(F, length(non))))
  x <- transform(x, dec=target>=thres)
  class(x) <- c("sre", class(x))
  x
}
                  
