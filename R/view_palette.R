#' View a palette with color labels
#'
#' Displays a pie chart with labeled hex codes for visualizing and identifying
#' individual colors in a palette.
#'
#' @param palette A character vector of hex color codes.
#' @export
#'
#' @examples
#' view_palette(get_palette("sb", "deep"))
view_palette <- function(palette) {
  graphics::pie(rep(1, length(palette)),
                labels = sprintf("%d (%s)", seq_along(palette), palette),
                col = palette)
}

#' Show a palette as a color bar
#'
#' Displays a horizontal strip of colors using `scales::show_col()`, useful for
#' continuous palettes or clean comparisons.
#'
#' @param palette A character vector of hex color codes.
#' @param border Optional border color for color boxes.
#' @export
#'
#' @examples
#' show_palette(get_palette("sb", "deep", type = "continuous", n = 20))
show_palette <- function(palette, border = "grey80") {
  scales::show_col(palette, labels = FALSE, borders = border)
}

#' View all palettes in a collection
#'
#' Visualizes each palette in a collection using `scales::show_col()`.
#'
#' @param collection Name of the collection (e.g., "sb", "ohsu")
#' @param type One of "discrete" or "continuous"
#' @param n If type = "continuous", number of interpolated colors to show
#' @param border Border color (passed to `scales::show_col()`)
#'
#' @importFrom graphics par title
#'
#' @export
#'
#' @examples
#' view_palette_collection("sb")                             # discrete
#' view_palette_collection("sb", type = "continuous", n = 40)  # continuous
view_palette_collection <- function(collection,
                                    type = "discrete",
                                    n = 20,
                                    border = "grey80") {
  if (!collection %in% names(color_palettes)) {
    stop(sprintf("Collection '%s' not found.", collection))
  }

  palettes <- color_palettes[[collection]]
  op <- par(mfrow = c(ceiling(length(palettes) / 2), 2),
            mar = c(1, 1, 2, 1))
  on.exit(par(op), add = TRUE)

  for (name in names(palettes)) {
    pal <- if (type == "continuous") {
      get_palette(collection, name, n = n, type = "continuous")
    } else {
      get_palette(collection, name, type = "discrete")
    }

    scales::show_col(pal, borders = border)
    title(name, line = -1)
  }
}


