## this should cover most cases of input data now
roc <- function(x, cond, laplace=T) {
  x <- as.cst(x)
  call <- match.call()
  ordered <- is.ordered(x$score)        #ordered factor
  if (!missing(cond)) {
      ct <- eval(substitute(cond.table(x, cond, target=TRUE)), x, parent.frame())
      nvar <- ncol(ct)-1
      mf <- tapply(ct$Freq, ct$target, mean)
      ct$weight <- as.vector(mf[ct$target]/ct$Freq)
      x <- merge(x, ct, by=names(ct)[1:nvar])
  } else {
      x$weight <- 1
  }
  o <- order(x$score, !x$target)        # order targets before nontargets
  xo <- x[o,]
  score <- xo$score
  w <- xo$weight
  discrete <- length(unique(score)) < 0.5 * length(score)
  t <- as.numeric(xo$target)             # targets, delta pmiss
  if (laplace & !ordered) {
      t <- c(1, 0, t, 1, 0)
      if (ordered) {                    # this shouldn't happen
          ls <- length(score)
          score <- score[c(1,1,1:ls,1,1)]
          score[1:2] <- min(score)
          score[ls+(1:2)] <- max(score)
      } else {
          score <- c(-Inf, -Inf, score, Inf, Inf)
      }
      w <- c(1, 1, w, 1, 1)
  }
  n <- 1-t                              # nontargtes, delta pfa
  nt <- sum(t*w)                        # number of target trials
  nn <- sum((1-t)*w)                    # number of non-target trials
  ## in the following, we group targets and non-targets with identical score
  ## we need to step carfully through them, as their cumulative counts need
  ## to be used to compute delta PFA and delta pmiss. 
  dt <- c(1,diff(as.numeric(score))!=0,1)         # points where threshold changes
  changes <- which(dt!=0)
  if (ordered) {                        # can't append to a factor
    thres <- score[c(1:length(score),1)]
    thres[length(score)+1] <- NA
  } 
  else 
      thres <- c(score,NA)
  remove <- numeric(0)
  for (i in 1:(length(changes)-1)) {
    start <- changes[i]
    end <- changes[i+1]-1
    if (end>start) {
      t[start] <- sum(t[start:end])
      n[start] <- sum(n[start:end])
      remove <- c(remove, (start+1):end)           # mark range for removal
  }
}
  if (length(remove)>0) {               # why 1 and not 0?  Seemed to be a major bug!
    t <- t[-remove]
    n <- n[-remove]
    thres <- thres[-remove]
    w <- w[-remove]
}
  pmiss <- c(0,rangecheck(cumsum(t*w)/nt)) # the cummlative (weighted) probs
  pfa <- c(1,rangecheck(1 - cumsum(n*w)/nn))
  ## next we want to optimize the entries in the (PFA, pmiss, thres) table.
  ## entries where one of (PFA, pmiss) does not change, but the other one does. 
  noch.pfa <- diff(pfa) == 0
  noch.pmiss <- diff(pmiss) == 0
  nch <- length(noch.pfa)
  if (nch>1) {
    changes <- c(T, ! (noch.pfa[1:(nch-1)] & noch.pfa[2:nch] | noch.pmiss[1:(nch-1)] & noch.pmiss[2:nch]), T)
    pfa <- pfa[changes]
    pmiss <- pmiss[changes]
    thres <- thres[changes]
  }
  roc <- data.frame(pfa, pmiss, thres)
  ## finally, compute the convex hull
  ## first, add a "corner" (1.1,1.1)
  n <- length(pfa)                      # number of points
  pfa <- c(pfa, 1.1)
  pmiss <- c(pmiss, 1.1)
  index <- chull(pfa, pmiss)               # find convex hull data points
  index <- sort(index[index<=n])          # remove corner
  roc$chull <- F
  roc$chull[index] <- T
  ## mind the abs( ) in the following expression, just to take care of 1/0=Inf (not -Inf)
  roc$opt.llr <- with(subset(roc, chull), log(abs(diff(pmiss) / diff(pfa))))[cumsum(roc$chull)]
  ## then, through isotonic regression---this should give identical answers
  x.opt <- attr(roc, "data") <- opt.llr(x, laplace)
  pauc <- pAUC(pfa[index], pmiss[index])
  ##eer
  if (ordered)
    stats <- list(Cllr=NA, Cllr.min=Cllr(x.opt, T), eer=100*eer(pfa, pmiss, index), pAUC=pauc, 
                  mt=NA, mn=NA, nt=nt, nn=nn, n=nt+nn, discrete=T)
  else
    stats <- list(Cllr=Cllr(x), Cllr.min=Cllr(x.opt, T), eer=100*eer(pfa, pmiss, index), pAUC=pauc,
                  mt=mean(x$score[x$target]), mn=mean(x$score[!x$target]), nt=nt, nn=nn, n=nt+nn,
                  discrete=discrete)
  attr(roc, "call") <- call
  attr(roc, "stats") <- stats
  class(roc) <- c("roc", class(roc))
  roc
}

## this should cover roc.sre and roc.data.frame
roc.default <- function(x) roc(as.cst(x))
