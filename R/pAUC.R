## pAUC: probability that a random non-target score is higher than a random target score

pAUC <- function(pfa, pmiss) {
    nt <- length(pfa)
    -sum((pmiss[-nt]+pmiss[-1]) * diff(pfa)) / 2
}
    
