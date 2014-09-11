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

The most important columns are `score` and `target`.  `score` is numer that is produced by a classifier that tries to discriminate beween same-source trials `target==TRUE` and different-source trials for which `target==FALSE`.  For any data.frame with these colums, a ROC curve can be drawn:

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

Package Overview
----------------

The most important two classes are `cst` (collection of supervised trials) and `roc` (receiver operating characteristic).
Most functions have been desined to handle the input data pretty flexibly, most functions will directly take `data.frame`s, `cst` or `roc` object.  Most of the complex computation goes into creating a `roc` object.  This involves the Pool Adjacent Violators algorithm and/or convex hull computation, so for large data frames (millions of trials) it becomes more efficient to calculate the `roc` object first by calling `roc()`.  

In summary, the analysis tools are layed out as follows:
 - `cst.tnt()` and `as.cst()` create `cst` objects, which contain both the classifiers scores `score` and the class labels `target`. 
 - `roc()` computes the basic ROC statistics: probability of miss vs probability of false alarm, with the threshold values.  Furter, some overall characteristics are computed, like the equal error rate.  Together witht the original data everything incompactly stored in a `roc` object.  
 - `roc.plot()` (the default plotting routine for an object of class `roc`) and `det.plot` will plot the ROC curve, either on linear scales or `probability paper`, i.e., probit waped axes (R's `qnorm()`).  
 - `llr.plot()` shows the optimal score-to-log-likelihood-ratio function.  If scores were log-likelihood-ratios, a minimum Bayes error decision could always be made by just giving the prior and cost parameters. 
 - `train.logreg()` and `train.cmlg()` are two calibration functions, that compute calibration parameters for simple models of score-to-log-likelihood-ratio functions.  The models can be applied uing `predict()`. 
 - `ape.plot` shows an applied probability of error (APE): the Bayes optimal decision error rate versus the logit of the (effective) prior. 
 - `nbe.plot` shows a normalized version of the APE: the error rates are scaled to the error rates of a minimum error Bayes decisions based on the prior.  
 - `tippet.plot()` draws a Tippet plot, a way of analysing a 2-class classifier in Forensic literature.
 - `setDCF()`, `actDCF()`, `minDCF()` and `prior.log.odds()` are functions that set Decision Cost Functions and query the actual and minimum decision costs based on a log-likelihood-ratio interpretation of the scores.  A number of pre-defined DCF operating points are pre-defined, including the two-point NIST SRE 2012 cost function.  

Trial count equalization
------------------------

This R package was first conceived to support delaing with unbalanced conditions in classifier evaluation.  If the performance of a classifier depends on some factor, e.g., the image resolution in a face recognition system, or the number of minutiae in a fingermark comparison system, it can occur that the trial counts over the various conditions are not equally balanced.  If the conditions are analysed separately, there is no problem, but if all trials are mixed in order to give an overall performance characteristic, then the unbalanced trial counts give a biased performance behaviour that may vary along the ROC curve. 

In order to compensate for such effects, `roc()` has an option to specify a list of factors for which unbalanced trial counts can be equalized.  Additionally, the main performance metrics are computed using the equalized counts.  The factors can be included in the original data frame as extra columns.  Thus it is easy to make subsets of the data and analyse these separately, or combine conditions and computed equal-weighted averages of performance metrics. 

`plot.cond()` is a function that draws multiple DET curves in a single plot, according to a list of factors, and finally draws the overall curve using trial-count equalized statistics.  
 
