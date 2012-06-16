prior.log.odds <- function(dcf.p=get("DCF.p", envir = .sretoolsEnv), prior=dcf.p$prior, cfa=dcf.p$cfa, cmiss=dcf.p$cmiss) {
  log(cmiss*prior / (cfa * (1-prior)))
}
