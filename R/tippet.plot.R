tippet.plot <- function(d, ...) {
  if ("sre" %in% class(d)) d <- det(d)
  else stopifnot("det" %in% class(d))
  LLR <- rep(d$thres/log(10), each=2)
  r <- rep(c(1,2,1), c(1,length(d$fa)-2,1))
  fa <- rep(d$fa, r)
  miss <- rep(d$miss, r)
  plot(LLR, fa, xlab="10log LR larger than", ylab="fraction of trials", main="Tippet plot",
       type="l", ...)
  lines(LLR, 1-miss, ...)
  grid()
  r <- range(LLR, finite=T)
  text(r[1], 0, adj=c(0,0), labels="different source")
  text(r[2], 1, adj=c(1,1), labels="same source")
}

  
