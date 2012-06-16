`ece.tnt` <- function(tar, non, prior=0.5, plo=logit(prior)) {
  if (missing(prior) && !missing(plo)) prior <- sigmoid(plo)
  ece.miss <- Vectorize(function(plo) -mean(log(sigmoid(tar+plo))))
  ece.fa <- Vectorize(function(plo) -mean(log(sigmoid(-non-plo))))
  (ece.miss(plo)*prior + ece.fa(plo)*(1-prior))/log(2)
}
