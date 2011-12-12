`DCF` <-
function (fa, miss) {
  DCF.p <- get("DCF.p", envir=.sretoolsEnv)
  DCF.p$cmiss*miss*DCF.p$prior + DCF.p$cfa*fa*(1-DCF.p$prior)
}
