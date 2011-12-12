`read.sre` <- function(file, method="trials") {
  data(srekey)                          # is this what we want?
  pipere <- "\\|\\s*$"                  # detect this is a pipe cmdline
  if (length(grep(pipere, file))) {
    cmd <- sub(pipere, "", file)
    x <- read.table(pipe(cmd))
  }
  else
    x <- read.table(file)               # for non-unix environments
  names(x) <- c("mcond", "adapt", "tcond", "gender",
                "model", "test", "chan", "dec", "score")
  if (class(x$dec)=="factor")
    x$dec <- x$dec=='t'                 # in case this was "t|f"
  if (method == "trials") {
    ## now we must guess the trial set
    x$model <- as.character(x$model)
    mm <- paste("m", x$model, sep="")
    sre <- unique(srekey$sre[mm,])         # what SREs are the models from?
    sre <- sre[!is.na(sre)]
    if (length(sre) != 1) 
      warning(paste("Not a unique evaluation, it appears", sre))
    ## merge in the key
    x <- merge(x, srekey[[sre]])
  } else if (method == "pin") {
    mpin <- srekey$pin[as.character(x$model),"pin"]
    testchan <- paste(x$test, x$chan, sep="-")
    tpin <- srekey$pin[testchan, "pin"]
    x <- transform(x, target=mpin==tpin)
  }
  class(x) <- list("sre", class(x))
  invisible(x)
}

