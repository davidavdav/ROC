`merge.meta` <-
function(x,m=meta.2008) {
  row.names(x) <- paste(x$model, x$test, x$chan, sep="-")
  merge(x,m,by=0)
}

