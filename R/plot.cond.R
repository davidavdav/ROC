`plot.cond` <-
function(x, cond, nr=1, equalize=TRUE, where="ur", ...) {
  ## make an empty canvas
  r <- det.plot(NULL, ...)
  if (!missing(cond)) {
    ct <- eval(substitute(cond.table(x, cond, target=FALSE)),x, parent.frame())
    nf <- ncol(ct)-1                   # number of factors
    title <- paste(names(ct)[1:nf], collapse=" ")
    for (i in 1:nrow(ct)) {
      s <- merge(x,ct[i,], by=names(ct)[1:nf]) # poor man's subset
      nr <- nr+1
      p <- det.plot(s, nr, ...)
      if (nf>1)
        row.names(p) <- apply(ct[i,1:nf], 1, paste, collapse=" ")
      else
        row.names(p) <- ct[i,1]
      r <- rbind(r, p)
    }
    ## plot overall DET last, in order to paint it "on top"
    nr <- nr+1
    if (equalize)
      p <- eval(substitute(det.plot(roc(x, cond), nr, col=1, ...)),x, parent.frame())
    else
      p <- det.plot(x, nr, col=1, ...)
  } else {
      p <- det.plot(x, 2, col=1, ...)
      title = ""
  }
  row.names(p) <- "all"
  r <- rbind(p,r)                       # but put all on top...
  legend.det(legend=row.names(r), order=order(r$eer, decreasing=T), where=where, title=title)
}

