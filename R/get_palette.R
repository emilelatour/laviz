#' Retrieve a color palette from a collection
#'
#' Access a named palette from a named collection. Supports discrete or continuous palettes.
#'
#' @param collection Character name of the collection (e.g., "sb", "ohsu", "personal")
#' @param palette Character name of the palette within the collection (e.g., "deep", "muted")
#' @param n Number of colors to return. If NULL, returns all available colors.
#' @param type One of "discrete" or "continuous".
#' @return A character vector of hex color codes.
#' @export
#'
#' @examples
#' get_palette("sb", "deep")
#' get_palette("sb", "muted6", n = 3)
#' get_palette("sb", "deep", n = 100, type = "continuous")
get_palette <- function(collection, palette, n = NULL, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  if (!collection %in% names(color_palettes)) {
    stop(sprintf("Collection '%s' not found in color_palettes.", collection))
  }

  palettes <- color_palettes[[collection]]

  if (!palette %in% names(palettes)) {
    stop(sprintf("Palette '%s' not found in collection '%s'.", palette, collection))
  }

  pal <- palettes[[palette]]

  if (is.null(n)) n <- length(pal)

  if (type == "discrete") {
    if (n > length(pal)) {
      stop(sprintf("Requested %d colors, but palette '%s' only has %d.", n, palette, length(pal)))
    }
    return(pal[1:n])
  }

  grDevices::colorRampPalette(pal)(n)
}
