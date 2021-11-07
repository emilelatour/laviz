#' @title
#' Axis formatting with commas
#'
#' @description
#' Wrappers around `ggplot2::scale_x_continuous` and
#' `ggplot2::scale_y_continuous` to make it easy to format with commas
#'
#' @param ... Arguments to `ggplot2::scale_x_continuous` and `ggplot2::scale_x_continuous`
#' @param labels default is `scales::percent` See `?ggplot2::scale_x_continuous` for additional details
#'
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales comma_format
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
#'   scale_y_comma()
#'
#' @name scale_comma
#' @aliases scale_x_comma
#' @aliases scale_y_comma

#' @rdname scale_comma
#'
#' @export
scale_x_comma <- function(...,
                        labels = scales::comma_format(
                          accuracy = 1,
                          scale = 1,
                          prefix = "",
                          suffix = "",
                          big.mark = ",",
                          decimal.mark = ".",
                          trim = TRUE)) {
  ggplot2::scale_x_continuous(...,
                              labels = labels)
}

#' @rdname scale_comma
#'
#' @export
scale_y_comma <- function(...,
                        labels = scales::comma_format(
                          accuracy = 1,
                          scale = 1,
                          prefix = "",
                          suffix = "",
                          big.mark = ",",
                          decimal.mark = ".",
                          trim = TRUE)) {
  ggplot2::scale_y_continuous(...,
                              labels = labels)
}




library(dplyr)
library(ggplot2)
library(palmerpenguins)

penguins %>%
  ggplot(data = .,
         aes(x = flipper_length_mm,
             y = body_mass_g,
             colour = species,
             shape = species)) +
  geom_point()

penguins %>%
  ggplot(data = .,
         aes(x = flipper_length_mm,
             y = body_mass_g,
             colour = species,
             shape = species)) +
  geom_point() +
  scale_y_comma()
