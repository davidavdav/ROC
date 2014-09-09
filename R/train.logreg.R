## logistic regression for a CST data frame
train.logreg <- function(x, dep="score") {
    x <- as.cst(x)
    weights <- 1/table(x$target)
    f <- formula(paste("target ~", dep))
    model <- glm(f, x, family=binomial, weight=weights[1+x$target])
}

