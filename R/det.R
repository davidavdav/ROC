## This function takes a NIST result data frame, and computes relevant
## statistics (Cllr, Cdet, eer) and prepares for plotting.
## You may specify a condition that will weight trials according to their
## inverse frequency, i.e., compensating for their proportion difference.
## Examples:
## Plot a det of all trials of x:
## > plot(det(x))
## Compute DET statistics for x, equalizing trial counts over gender:
## > xd <- det(x, list(gender))
## > xd$eer
## Just give me the equal error rate for female
## > det(subset(x,gender=="f"))$eer
## Give some basic performance statistics
## > data.frame(det(x)[1:9])
det.sre <- function(x, cond, ct=NULL) {
  if (! "sre" %in% class(x) && sum(names(x) %in% c("score", "dec", "target")) != 3)
      stop("`x' must be of class `sre' or contain `score', `dec' and `target'")
  ## the very first we must do is eval(substitute(cond))...
  if (!missing(cond)) 
    ct <- eval(substitute(cond.table(x, cond, target=TRUE)), x, parent.frame())
  if (!is.null(ct)) {
    if (! "cond.table" %in% class(ct))
      stop("`ct' must be of class cond.table")
    if (! "target" %in% names(ct))
      stop("`ct' must be made using `target=TRUE'")
    nvar <- ncol(ct)-1
    mf <- tapply(ct$Freq, ct$target, mean)
    w <- as.vector(mf[ct$target]/ct$Freq)
    ct <- transform(ct, weight=w)
    x <- merge(x, ct, by=names(ct)[1:nvar]) # add weight = 1/Freq
    x <- x[order(x$score),]             #sort by score
  } else {
    x <- transform(x, weight=1)
    x <- x[order(x$score),]             #sort by score
    ct <- NULL                          #cond.table
  }
  t <- as.numeric(x$target)
  w <- x$weight
  nt <- sum(t*w)                        # number of target trials
  nn <- sum((1-t)*w)                    # number of non-target trials
  miss <- c(0,rangecheck(cumsum(t*w)/nt)) # the cummlative (weighted) probs
  fa <- c(1,rangecheck(1 - cumsum((1-t)*w)/nn))
  thres <- x$score
  nmiss <- sum((x$target & !x$dec)*w) # weighted counts...
  nfa <- sum((!x$target & x$dec)*w)
  ## adcf confidence interval
  if (!is.null(ct)) {                   # estimated through normal
    afa <- nfa/nn                       # approximation and weighted counts
    ci <- qnorm(0.975)*sqrt(afa*(1-afa)/nn)
    afa.lci <- afa-ci
    afa.uci <- afa+ci
    amiss <- nmiss/nt
    ci <- qnorm(0.975)*sqrt(amiss*(1-amiss)/nt)
    amiss.lci <- amiss-ci
    amiss.uci <- amiss+ci
  } else {                              # "true" binomial confidence intervals
    fa.ci <- data.frame(ci.binomial(nfa, nn))
    afa <- fa.ci$p.x.n                  # actual false alarms
    afa.lci <- fa.ci$p.lci
    afa.uci <- fa.ci$p.uci
    miss.ci <- data.frame(ci.binomial(nmiss, nt))
    amiss <- miss.ci$p.x.n              # actual misses
    amiss.lci <- miss.ci$p.lci
    amiss.uci <- miss.ci$p.uci
  }
  ## actual, minimum DCF
  adcf <- DCF(afa, amiss)
  dcf <- DCF(fa, miss)
  mi <- which.min(dcf)
  min.score <- x$score[mi]
  mfa <- fa[mi]
  mmiss <- miss[mi]
  ## Cllr
  cllr <- Cllr(x)
  x <- opt.llr(x, laplace=F)
  cllr.min <- Cllr(x, opt=T)
  ## EER, though convex hull optimization
  ch <- chull(c(fa,1.1), c(miss,1.1))
  ch <- sort(ch[ch<=length(fa)])                 # remove outer hull point
  EER=eer(fa, miss, ch)
  ## means of target and non-target scores
  mt <- mean(x$score[x$target])
  mn <- mean(x$score[!x$target])
  res <- list(Cllr=cllr, Cllr.min=cllr.min, eer=100*EER,
              Cdet=adcf, Cdet.min=dcf[mi],
              mt=mt, mn=mn, 
              nt=nt, nn=nn, n=nt+nn,
              afa=afa, amiss=amiss, afa.lci=afa.lci, afa.uci=afa.uci,
              amiss.lci=amiss.lci, amiss.uci=amiss.uci,
              mfa=mfa, mmiss=mmiss, atscore=min.score, dcf.p=DCF(), 
              fa=fa, miss=miss, thres=thres, data=x, cond.table=ct, ch=ch)
  class(res) <- "det"
  invisible(res)
}

## keep old definition of det()
det.matrix <- det
det <- function(x, ...) UseMethod("det")
det.default <- base::det
