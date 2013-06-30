`DCF` <-
function (pfa, pmiss) {
  DCF.p <- get("DCF.p", envir=.sretoolsEnv)
  if (missing(pfa) && missing(pmiss))
    DCF.p
  else {
    stopifnot(!missing(pfa) & !missing(pmiss))
    pmiss %o% (DCF.p$cmiss*DCF.p$prior) + pfa %o% (DCF.p$cfa*(1-DCF.p$prior))
  }
}
