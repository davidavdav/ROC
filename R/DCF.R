`DCF` <-
function (pfa, pmiss) {
  DCF.p <- get("DCF.p", envir=.sretoolsEnv)
  if (missing(pfa) && missing(pmiss))
    DCF.p
  else {
    stopifnot(!missing(pfa) & !missing(pmiss))
    DCF.p$cmiss*pmiss*DCF.p$prior + DCF.p$cfa*pfa*(1-DCF.p$prior)
  }
}
