#' Simple Linear Regression
#'
#' @param x numeric vector
#' @param y numeric vector
#' @export
#' @examples
#' x <- 1:100
#' y <- 2 * x + rnorm(100, mean = 0, sd = 20)
#' SimpLinR(x, y)
SimpLinR <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    stop("Length of x and y must be the same.")
  }
  
  # Call the C++ function
  result <- SimpLinCpp(x, y)
  return(result)
}