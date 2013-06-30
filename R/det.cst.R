## This is a simplified version of det.sre, just doing the necessary stuff for
## making a ROC or a DET plot. 
det.cst <- function(x) {
  if (! any(c("cst","sre") %in% class(x))) {
    if (sum(names(x) %in% c("score", "dec")) == 2) 
      x <- as.cst(x)
    else stop("`x' must be of class cst or sre or contain `score' and `target'")
  }
  t <- as.numeric(x$target)
  nt <- sum(t*w)                        # number of target trials
  nn <- sum((1-t)*w)                    # number of non-target trials
  miss <- c(0,rangecheck(cumsum(t*w)/nt)) # the cummlative (weighted) probs
  fa <- c(1,rangecheck(1 - cumsum((1-t)*w)/nn))
  thres <- x$score
  cllr <- Cllr(x)
  x <- opt.llr(x, laplace=F)
  cllr.min <- Cllr(x, opt=T)
  ## EER, though convex hull optimization---this is the same as PAV!
  ch <- chull(c(fa,1.1), c(miss,1.1))
  ch <- sort(ch[ch<=length(fa)])                 # remove outer hull point
  EER=eer(fa, miss, ch)
  ## means of target and non-target scores
  mt <- mean(x$score[x$target])
  mn <- mean(x$score[!x$target])
  res <- list(Cllr=cllr, Cllr.min=cllr.min, eer=100*EER,
              mt=mt, mn=mn, 
              nt=nt, nn=nn, n=nt+nn,
              fa=fa, miss=miss, data=x)
  class(res) <- "det"
  invisible(res)
}

             
  
