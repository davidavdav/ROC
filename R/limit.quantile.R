limit.quantile <- function(data, limits) {
  data[data == -Inf] <- 2*limits[1] - limits[2]
  data[data == Inf] <- 2*limits[2] - limits[1]
  data
}
