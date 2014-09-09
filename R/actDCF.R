actDCF <- function(x, plo=prior.log.odds(), norm=T) {
    x <- as.cst(x)
    mDCF <- bayes.error.rate(x$score[x$target], x$score[!x$target], plo) 
    if (norm) mDCF <- mDCF / pmin(sigmoid(plo), sigmoid(-plo))
    mDCF
}
