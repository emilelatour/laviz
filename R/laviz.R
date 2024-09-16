#' @title
#' laviz: an opinionated package of Emile's visualizations
#'
#' @description
#' The objective of this package is to perform statistical inference using a
#' grammar that illustrates the underlying concepts and a format that coheres
#' with the tidyverse.
#'
#' @examples
#' # Example usage:
#' library(laviz)
#'
#' @keywords internal
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("."))
