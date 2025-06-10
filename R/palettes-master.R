# At the top of palettes-master.R
#' @include palettes-sb.R
#' @include palettes-ohsu.R
#' @include palettes-wom.R
#' @include palettes-drsimonj.R
#' @include palettes-personal.R
#'
#'
#'
#' Master list of all palette collections in laviz
#'
#' This object is a nested list of named palettes grouped by collection (e.g., "sb", "ohsu").
#' It is used internally by functions like `get_palette()`, `scale_color_collection()`,
#' and `view_palette_collection()`.
#'
#' @title Master color palette list
#' @format A named list of lists of character vectors (hex color codes)
#' @name color_palettes
#' @docType data
#' @keywords internal
#' @export
color_palettes <- list(
  sb = sb_palettes,
  ohsu = ohsu_palettes,
  wom = wom_palettes,
  drsimonj = drsimonj_palettes,
  personal = personal_palettes
)


#' List available palette names from a collection
#'
#' @param collection Name of the collection (e.g., "sb", "ohsu")
#' @return A character vector of palette names
#' @export
#'
#' @examples
#' palette_names("sb")
#' # palette_names("personal")
palette_names <- function(collection) {
  if (!collection %in% names(color_palettes)) {
    stop(sprintf("Collection '%s' not found in color_palettes.", collection))
  }

  names(color_palettes[[collection]])
}

#' List all available palette collections
#'
#' @return A character vector of collection names
#' @export
#'
#' @examples
#' collection_names()
collection_names <- function() {
  names(color_palettes)
}
