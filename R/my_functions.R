
#' Report p values
#'
#' This function takes a p-value and to a specified number of digits reports as text.
#'
#' @param x numeric value of p
#' @param y numeric value of sig digits
#' @return reported text
#' @export
#' @examples
#' report_p(0.001, 3)
#'



report_p <- function(p, digits = 3) {

  if (!is.numeric(p)) stop("p must be a number")
  if (p <= 0) warning("p-values cannot less 0")
  if (p >= 1) warning("p-values cannot be greater than 1")

  reported <- if_else(p < 0.001,
                     "p < 0.001",
                     paste("p =", round(p, digits)))
  return(reported)
}
