#' Generic color scale from any palette collection
#'
#' @param collection Name of palette collection (e.g. "sb", "ohsu")
#' @param palette Name of palette in that collection
#' @param discrete Logical: use discrete scale?
#' @param ... Other arguments passed to `scale_*_manual` or `scale_*_gradientn`
#'
#' @export
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' ggplot(data = penguins,
#'        aes(x = flipper_length_mm,
#'            y = body_mass_g)) +
#'   geom_point(aes(color = species,
#'                  shape = species),
#'              size = 2) +
#'   theme_minimal() +
#'   laviz::scale_color_collection(collection = "personal",
#'                                 palette = "sb_colorblind")
#'
#' ggplot(data = penguins,
#'        aes(x = flipper_length_mm,
#'            y = body_mass_g)) +
#'   geom_point(aes(color = species,
#'                  shape = species),
#'              size = 2) +
#'   theme_minimal() +
#'   laviz::scale_color_collection(collection = "personal",
#'                                 palette = "carter3a")
#'
#' ggplot(penguins,
#'        aes(x = flipper_length_mm,
#'            y = body_mass_g)) +
#'   geom_point(aes(color = sex)) +
#'   theme_minimal() +
#'   laviz::scale_color_collection(collection = "personal",
#'                                 palette = "binary") +
#'   facet_wrap(~ species)
scale_color_collection <- function(collection, palette, discrete = TRUE, ...) {
  pal <- get_palette(collection, palette, type = if (discrete) "discrete" else "continuous")
  if (discrete) {
    ggplot2::scale_color_manual(values = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }
}

#' @rdname scale_color_collection
#' @export
#' @examples
#' library(ggplot2)
#' library(palmerpenguins)
#'
#' ggplot(penguins,
#'        aes(x = island,
#'            fill = species)) +
#'   geom_bar(alpha = 0.8) +
#'   theme_minimal() +
#'   facet_wrap(~species, ncol = 1) +
#'   coord_flip() +
#'   laviz::scale_fill_collection(collection = "sb",
#'                                palette = "deep")
#'
#'
#' ggplot(data = penguins,
#'        aes(x = flipper_length_mm)) +
#'   geom_histogram(aes(fill = species),
#'                  alpha = 0.5,
#'                  position = "identity",
#'                  color = "white",
#'                  binwidth = 2) +
#'   theme_minimal() +
#'   laviz::scale_fill_collection(collection = "personal",
#'                                palette = "morosco3")
#'

scale_fill_collection <- function(collection, palette, discrete = TRUE, ...) {
  pal <- get_palette(collection, palette, type = if (discrete) "discrete" else "continuous")
  if (discrete) {
    ggplot2::scale_fill_manual(values = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  }
}


# Add Aliases for Favorite Collections

#' Shortcut for SB ggplot2 color and fill scales
#'
#' Uses `scale_color_collection()` and `scale_fill_collection()` with `"sb"` as the collection.
#'
#' @inheritParams scale_color_collection
#' @export
scale_color_sb <- function(palette, discrete = TRUE, ...) {
  scale_color_collection("sb", palette, discrete, ...)
}

#' @rdname scale_color_sb
#' @export
scale_fill_sb <- function(palette, discrete = TRUE, ...) {
  scale_fill_collection("sb", palette, discrete, ...)
}
