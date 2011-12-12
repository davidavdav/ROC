`Cllr` <-
function(x, opt=F) {
  if (opt) {                            # do we want optimum?
    if (is.null(x$opt.llr)) x <- opt.llr(x, F) # did we have optimum?
    x$score <- x$opt.llr
  }
  if (is.null(x$weight)) x <- transform(x, weight=1) # did we have weights?
  attach(x)
  c.miss <- 1/log(2) * mean(log(1+exp(-score[target]))*weight[target])
  c.fa <- 1/log(2) * mean(log(1+exp(score[!target]))*weight[!target])
  detach(x)
  (c.miss+c.fa)/2
}

