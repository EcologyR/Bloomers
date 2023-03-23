#' Multiply by a give value (default is ten)
#'
#' @param x Numeric
#' @param y Numeric (10 by default)
#' @param warning.inf Logical warning if the result is infinite
#'
#' @return
#' @export
#'
#' @examples
#' multipy_by(2)
#' multiply_by(2,100)

multiyply_by <- function(x = Null, y = 10){

  ## Check arguments (defending programming)

  stopifnot(is.numeric(x), is.numeric(y))

  if (length(x) != length(y)) {
    stop("x and y must have same length")
  }

  ## Calculate value
  valor <- x * y


  return(value)
}
