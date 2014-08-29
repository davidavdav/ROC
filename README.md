ROC
===

An R package for computing and displaying ROC curves, DET curves, and computing detection classifier statistcs such as EER(CH), Cllr, minimum Cllr, and doing calibration.  This package was formerly known as `sretools`.  It can be used for the analysis of any two-class classifier that output a score, for instance biometric comparison systems.  

Synopsys
--------

```R
library(ROC)
data(tno.2008)
x <- tno.2008 ## get ome example data
head(x)
```

This loads the package and makes an example data.frame available, which are the results TNO subitted to the NIST 2008 Speaker Recognition Evaluation.  The last line shows the structure of the data frame.  

The most important columns are `score` and `target`.  `score` is numer that is produced by a classifier that tries to discriminated beween same-source trials `target==TRUE` and different-source trials for which `target==FALSE`.  For any data.frame with these colums, an ROC curve can be drawn:

```R
plot(roc(x))
```

The default is to plot the miss-rate, an error rate, instead of the hit-rate which some people prefer. 

However, a nicer way of plotting 2-class classifier scores is in a so-called Detection Error Tradeoff plot, DET, which is the same as a ROC, but has warped axes. 

```R
det.plot(roc(x))
## or
det.plot(x)
```

Computing ROC statistics or plotting a ROC or DET will result in a table with basic statistics
 - `Cllr` the cost of the log-likelihood ratio, if the score is interpred as a calibrated log-likelihood-ratio
 - `Cllr.min` the cost of the log-likelihood ratio, after transforming warping the scores using a monotonic function that minimized `Cllr`
 - `eer` the equal error rate, in the ROC convex hull interpretation
 - `mt` the mean of the target scores
 - `mn` the mean of the non-target scores
 - `nt`, `nn`, `n`, the number of target and non-target trials, and the total number of trials

