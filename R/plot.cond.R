`plot.cond` <-
function(x, cond, nr=1, equalize=T, where="ur", ...) {
  ## make an empty canvas
  r <- plot.det(NULL, ...)
  if (!missing(cond)) {
    ct <- eval(substitute(cond.table(x, cond, target=FALSE)),x, parent.frame())
    nf <- ncol(ct)-1                   # number of factors
    for (i in 1:nrow(ct)) {
      s <- merge(x,ct[i,], by=names(ct)[1:nf]) # poor man's subset
      nr <- nr+1
      p <- plot(det(sre(s)),nr, ...)
      if (nf>1)
        row.names(p) <- apply(ct[i,1:nf], 1, paste, collapse=" ")
      else
        row.names(p) <- ct[i,1]
      r <- rbind(r, p)
    }
    ## plot overall DET last, in order to paint it "on top"
    nr <- nr+1
    if (equalize)
      p <- eval(substitute(plot(det(x, cond), nr, col=1, ...)),x, parent.frame())
    else
      p <- plot(det(x), nr, col=1, ...)
  } else p <- plot(det(x), 2, col=1, ...)
  row.names(p) <- "all"
  r <- rbind(p,r)                       # but put all on top...
  legend.det(legend=row.names(r), order=order(r$eer, decreasing=T), where=where)
  r
}

