#' Creates histogram, boxplot, and numerical summary of a numeric vector.
#'
#' @param x A numeric vector.
#' @return A data frame with mean, min, max, and median.
#' @export

ds <- function(x){
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector.")
  }

  # Set up plotting area
  par(mfrow = c(1,2))
  # histogram
  hist(x, col = rainbow(30), main = "Histogram", xlab = "Values", ylab = "Frequency")
  boxplot(x, col = 'green', main = "Boxplot")

  # Reset plot area
  par(mfrow = c(1,1))
  return(data.frame(
    Mean = mean(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE)
  ))
}
