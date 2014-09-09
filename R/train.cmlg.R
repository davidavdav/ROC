`train.cmlg` <- function(x) {
    tar <- x$score[x$target]
    non <- x$score[!x$target]
    mt <- mean(tar)
    mn <- mean(non)
    v <- (var(tar) + var(non)) / 2
    a <- (mt-mn) / v
    b <- -a * (mt+mn) / 2
    f <- function(x) a*x + b
    class(f) <- c("linearcalibration", class(f))
    f
}

`predict.linearcalibration` <- function(model, x)
    model(x$score)

