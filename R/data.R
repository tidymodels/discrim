#' Parabolic class boundary data
#'
#' @details These data were simulated. There are two correlated predictors and
#' two classes in the factor outcome.
#'
#' @name parabolic
#' @aliases parabolic
#' @docType data
#' @return \item{parabolic}{a data frame}
#'
#' @keywords datasets
#' @examples
#' data(parabolic)
#'
#' library(ggplot2)
#' ggplot(parabolic, aes(x = X1, y = X2, col = class)) +
#'   geom_point(alpha = .5) +
#'   theme_bw()
#'
NULL
