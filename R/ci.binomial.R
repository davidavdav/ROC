`ci.binomial` <-
function(x,n,alpha=0.05){
  ## Finds the exact 95% confidence interval for binomial count
  ## R v0.90.1, Tomas Aragon, created 01/01/2000, edited
  ## x = observed count (non-negative integer)
  ## number of Bernoulli trials
  ## alpha = .05 (default), for 95% confidence interval
  ## Output includes x and CIs, and p=x/n and CIs
  ## Uses lci.binomial() and uci.binomial() for bisection method
  xn <- cbind(x,n)
  ci.pois <- function(xx,aa=alpha){
    lci <- lci.binomial(xx[1],xx[2],alpha=aa)
    uci <- uci.binomial(xx[1],xx[2],alpha=aa)
    c(lci=lci,uci=uci)
  }
  prob <- t(apply(xn,1,ci.pois))
  ans <- cbind(as.vector(x),n,prob*n,as.vector(x/n),prob)
  dimnames(ans)[[2]] <- c("x","n","x.lci","x.uci","p=x/n","p.lci","p.uci")
  ans
}

