`cond.table` <-
function(x, cond, target=F) {
  if (!missing(cond)) {
    l <- eval(substitute(cond),x, parent.frame())
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    if (target) {                       # I am sure this can be done better
      t <- as.data.frame(table(data.frame(l,x$target)))
      names(t)[ncol(t)-1] <- "target"
      nf <- ncol(t)-2
    } else {
      t <- as.data.frame(table(l))
      nf <- ncol(t)-1
    }
    names(t)[1:nf] <- c(names(nl)[unlist(eval(substitute(cond), nl))])
  }
  t <- t[t$Freq>0,]
  class(t) <- c("cond.table", class(t))
  t
}

