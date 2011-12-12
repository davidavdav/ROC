`opt.llr` <-
function(x, laplace=T) {
  o <- order(x$score)
  p.ideal <- as.numeric(x$target[o]) ## ideal posterior
  if (is.null(x$weight)) x <- transform(x, weight=1) # did we have weights?  
  w.ideal <- x$weight[o]
  nt <- sum(p.ideal)
  nn <- sum(1-p.ideal)
  if (laplace) {
    p.ideal <- c(1,0,p.ideal,1,0) ## lapace's rule of succession
    w.ideal <- c(1,1,w.ideal,1,1)
  }
  p.opt <- monoreg(p.ideal,w=w.ideal)$yf
  if (laplace) 
    p.opt <- p.opt[3:(length(p.opt)-2)]
  post.log.odds <- log(p.opt)-log(1-p.opt)
  log.prior.odds <- log(nt/nn)
  llrs <- post.log.odds - log.prior.odds
  llrs[o] <- llrs
  transform(x, opt.llr=llrs)
}

