#' Minimal Theme with White Background and Custom Axis/Grid Options
#'
#' This function extends \code{ggplot2::theme_minimal()} by ensuring a white background
#' (preventing gray backgrounds when exporting to PNG) and providing options for customizing
#' axis lines, grid visibility, axis ticks, and borders.
#'
#' @param base_size Numeric. Base font size (default: 11).
#' @param base_family Character. Base font family (default: "").
#' @param base_line_size Numeric. Base line size relative to `base_size` (default: `base_size / 22`).
#' @param base_rect_size Numeric. Base rectangle size relative to `base_size` (default: `base_size / 22`).
#' @param base_col Character. Default color for axes, ticks, and borders (default: "#323232").
#' @param axis_col Character. Color of axis lines and ticks. Defaults to `base_col`.
#' @param grid Logical or Character. Controls the visibility of grid lines:
#'   \itemize{
#'     \item \code{TRUE} (default): Shows major and minor grid lines.
#'     \item \code{FALSE}: Hides all grid lines.
#'     \item Character string: Allows selective control using "X", "Y", "x", "y":
#'       \itemize{
#'         \item \code{"X"}: Shows major grid lines on the x-axis.
#'         \item \code{"Y"}: Shows major grid lines on the y-axis.
#'         \item \code{"x"}: Shows minor grid lines on the x-axis.
#'         \item \code{"y"}: Shows minor grid lines on the y-axis.
#'       }
#'   }
#' @param axis Logical or Character. Controls axis lines:
#'   \itemize{
#'     \item \code{TRUE}: Shows both x and y axis lines.
#'     \item \code{FALSE} (default): Hides all axis lines.
#'     \item Character string: Selectively controls axis lines:
#'       \itemize{
#'         \item \code{"x"}: Shows only the x-axis line.
#'         \item \code{"y"}: Shows only the y-axis line.
#'         \item \code{"xy"}: Shows both axis lines.
#'       }
#'   }
#' @param ticks Logical. Whether to show axis ticks (default: `FALSE`).
#' @param border Logical. Whether to add a panel border around the plot (default: `FALSE`).
#'   If \code{TRUE}, it overrides the `axis` argument and hides axis lines.
#'
#' @return A \code{ggplot2} theme object with a white plot background and customizable
#'   grid, axis, tick, and border settings.
#'
#' @examples
#' library(ggplot2)
#' base_plot <- ggplot(mtcars,
#'                     aes(x = mpg,
#'                         y = wt)) +
#'   geom_point()
#'
#' base_plot
#'
#' base_plot +
#'   theme_minimal_white()
#'
#' base_plot +
#'   theme_minimal_white(grid = "XY")
#'
#' base_plot +
#'   theme_minimal_white(axis = "X")
#'
#' base_plot +
#'   theme_minimal_white(border = TRUE)
#'
#' base_plot +
#'   theme_minimal_white(grid = FALSE,
#'                       border = TRUE,
#'                       ticks = TRUE)
#'
#' @export
theme_minimal_white <- function(base_size = 11,
                         base_family = "",
                         base_line_size = base_size/22,
                         base_rect_size = base_size/22,
                         base_col = "#323232",
                         axis_col = base_col,
                         grid = TRUE,
                         axis = FALSE,
                        ticks = FALSE,
                        border = FALSE) {


  #### Base theme --------------------------------

  ret <- ggplot2::theme_minimal(base_family = base_family,
                                base_size = base_size,
                                base_line_size = base_line_size,
                                base_rect_size = base_rect_size)

  ret <- ret + theme(plot.background = element_rect(fill = "white", color = NA))



  # ret <- ret + theme(legend.background = element_blank())
  # ret <- ret + theme(legend.key = element_blank())
  # ret <- ret + theme(panel.background = element_blank())
  # ret <- ret + theme(strip.background = element_blank())


  #### Grid --------------------------------

  if (inherits(grid, "character") | grid == TRUE) {

    # ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    # ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    # ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }

  #### Axis lines --------------------------------

  if (border == TRUE) {axis <- FALSE}

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line = element_line(color = axis_col, linewidth = 0.3))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, linewidth = 0.3))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, linewidth = 0.3))
      }
    } else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col, linewidth = 0.3))
      ret <- ret + theme(axis.line.y = element_line(color = axis_col, linewidth = 0.3))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }


  #### Axis ticks --------------------------------

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(colour = axis_col, linewidth = 0.3))
    ret <- ret + theme(axis.ticks.x = element_line(colour = axis_col, linewidth = 0.3))
    ret <- ret + theme(axis.ticks.y = element_line(colour = axis_col, linewidth = 0.3))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  #### Border --------------------------------

  if (border == TRUE) {
    ret <- ret + theme(panel.border = element_rect(color = axis_col,
                                                   size = 0.3 * 2,
                                                   fill = NA))
  }


  #### Return theme --------------------------------

  ret

}
