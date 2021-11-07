#' @title
#' Axis scales and formatting for percentages
#'
#' @description
#' Wrappers around `ggplot2::scale_x_continuous` and
#' `ggplot2::scale_y_continuous` to make it easy to format percentages.
#'
#' @param ... Arguments to `ggplot2::scale_x_continuous` and `ggplot2::scale_x_continuous`
#' @param limits default is `c(0, 1)` See `?ggplot2::scale_x_continuous` for additional details
#' @param labels default is `scales::percent` See `?ggplot2::scale_x_continuous` for additional details
#'
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales percent
#'
#' @references
#' https://github.com/thomas-neitmann/scalesextra
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins %>%
#'   dplyr::count(species) %>%
#'   mutate(pct = n / sum(n)) %>%
#'   ggplot(data = .,
#'          aes(x = species,
#'              y = pct)) +
#'   geom_bar(stat = "identity")
#'
#' penguins %>%
#'   dplyr::count(species) %>%
#'   mutate(pct = n / sum(n)) %>%
#'   ggplot(data = .,
#'          aes(x = species,
#'              y = pct)) +
#'   geom_bar(stat = "identity") +
#'   scale_y_pct()
#'
#' @name scale_continuous
#' @aliases scale_x_pct
#' @aliases scale_y_pct

#' @rdname scale_pct
#'
#' @export
scale_x_pct <- function(...,
                        limits = c(0, 1),
                        labels = scales::percent) {
  ggplot2::scale_x_continuous(...,
                              limits = limits,
                              labels = labels)
}

#' @rdname scale_pct
#'
#' @export
scale_y_pct <- function(...,
                        limits = c(0, 1),
                        labels = scales::percent) {
  ggplot2::scale_y_continuous(...,
                              limits = limits,
                              labels = labels)
}



