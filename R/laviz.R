

#' laviz: an opinionated package of Emile's visualizations
#'
#' The objective of this package is to perform statistical inference
#' using a grammar that illustrates the underlying
#' concepts and a format that coheres with the tidyverse.
#'
#' @docType package
#' @name laviz
#' @examples
#' # Example usage:
#' library(laviz)
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("."))
