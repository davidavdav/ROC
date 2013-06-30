roc <- function(x) {
  UseMethod("roc")
}

roc.cst <- function(x) {
  stopifnot(is(x, "cst"))
  call <- match.call()
  o <- order(x$score, !x$target)
  xo <- x[o,]
  w <- 1                                # weight, for future change
  t <- as.numeric(xo$target)             # targets, delta pmiss
  n <- 1-t                              # nontargtes, delta pfa
  nt <- sum(t*w)                        # number of target trials
  nn <- sum((1-t)*w)                    # number of non-target trials
  ## in the following, we group targets and non-targets with identical score
  ## we need to step carfully through them, as their cumulative counts need
  ## to be used to comput delta PFA and delta pmiss. 
  dt <- c(1,diff(xo$score)!=0,1)         # points where threshold changes
  changes <- which(dt!=0)
  thres <- c(xo$score,NA)
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
  if (length(remove)>1) {
    t <- t[-remove]
    n <- n[-remove]
    thres <- thres[-remove]
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
  ## then, through isotonic regression
  x.opt <- attr(roc, "data") <- opt.llr(x, FALSE)
  ##eer
  stats <- list(Cllr=Cllr(x), Cllr.min=Cllr(x.opt, T), eer=100*eer(pfa, pmiss, index),
                mt=mean(x$score[x$target]), mn=mean(x$score[!x$target]), nt=nt, nn=nn, n=nt+nn)
  attr(roc, "call") <- call
  attr(roc, "stats") <- stats
  class(roc) <- c("roc", class(roc))
  roc
}

## this should cover roc.sre and roc.data.frame
roc.default <- function(x) roc(as.cst(x))
