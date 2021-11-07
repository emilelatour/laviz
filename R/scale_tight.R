#' @title
#' Axis formatting tight to the plot
#'
#' @description
#' Wrappers around `ggplot2::scale_x_continuous` and
#' `ggplot2::scale_y_continuous` to make it easy to tighten up the axis
#'
#' @param ... Arguments to `ggplot2::scale_x_continuous` and `ggplot2::scale_x_continuous`
#'
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales comma_format
#'
#' @references
#' https://github.com/thomas-neitmann/scalesextra
#' https://stackoverflow.com/questions/44170871/how-does-ggplot-scale-continuous-expand-argument-work
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' penguins %>%
#'   ggplot(data = .,
#'          aes(x = flipper_length_mm,
#'              y = body_mass_g,
#'              colour = species,
#'              shape = species)) +
#'   geom_point()
#'
#' penguins %>%
#'   ggplot(data = .,
#'          aes(x = flipper_length_mm,
#'              y = body_mass_g,
#'              colour = species,
#'              shape = species)) +
#'   geom_point() +
#'   scale_x_tight()
#'
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
#'   scale_y_tight()
#'
#' @name scale_tight
#' @aliases scale_x_tight
#' @aliases scale_y_tight

#' @rdname scale_tight
#'
#' @export
scale_x_tight <- function(...) {
  ggplot2::scale_x_continuous(..., expand = tight())
}

#' @rdname scale_tight
#'
#' @export
scale_y_tight <- function(...) {
  ggplot2::scale_y_continuous(..., expand = tight())
}

tight <- function() {
  ggplot2::expansion(mult = c(0, 0.05),
                     add = c(0, 0))
}



