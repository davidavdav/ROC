minDCF <- function(x, plo=prior.log.odds(), norm=T) {
    x <- as.cst(as.roc(x))
    mDCF <- bayes.error.rate(x$opt.llr[x$target], x$opt.llr[!x$target], plo) 
  if (norm) mDCF <- mDCF / pmin(sigmoid(plo), sigmoid(-plo))
  mDCF
}
