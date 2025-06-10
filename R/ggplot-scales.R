#' Generic color scale from any palette collection
#'
#' @param collection Name of palette collection (e.g. "sb", "ohsu")
#' @param palette Name of palette in that collection
#' @param discrete Logical: use discrete scale?
#' @param ... Other arguments passed to `scale_*_manual` or `scale_*_gradientn`
#'
#' @export
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
