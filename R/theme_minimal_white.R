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
#' @param base_col Character. Base text color (default: `"#323232"`).
#' @param axis_text_family Character. Font family for axis text (default: `base_family`).
#' @param axis_text_size Numeric. Font size for axis text (default: `base_size`).
#' @param axis_title_family Character. Font family for axis titles (default: `base_family`).
#' @param axis_title_size Numeric. Font size for axis titles (default: `base_size`).
#' @param axis_title_face Character. Font face for axis titles (default: `"plain"`).
#' @param axis_title_just Character. Axis title justification. One of `[blmcrt]` (default: `"mc"`).
#' @param axis_col Character. Color for axis lines and text (default: `base_col`).
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
                                axis_text_family = base_family,
                                axis_text_size = base_size,
                                axis_title_family = base_family,
                                axis_title_size = base_size,
                                axis_title_face = "plain",
                                axis_title_just = "mc",
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


  #### Text --------------------------------

  ret <- ret + theme(text = element_text(family = base_family,
                                         colour = base_col,
                                         size = base_size))


  #### Axis text --------------------------------

  ret <- ret + theme(axis.text.x = element_text(family = axis_text_family,
                                                colour = axis_col,
                                                size = axis_text_size,
                                                margin = margin(t = 0)))
  ret <- ret + theme(axis.text.y = element_text(family = axis_text_family,
                                                colour = axis_col,
                                                size = axis_text_size,
                                                margin = margin(r = 0)))


  #### Axis title justification --------------------------------

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)


  #### Axis title --------------------------------

  # ret <- ret + theme(axis.title = element_text(family = axis_title_family,
  #                                              colour = axis_col,
  #                                              size = axis_title_size,
  #                                              margin = margin(t = 10)))
  #
  # ret <- ret + theme(axis.title.x = element_text(hjust = xj,
  #                                                family = axis_title_family,
  #                                                colour = axis_col,
  #                                                size = axis_title_size,
  #                                                face = axis_title_face,
  #                                                margin = margin(t = 10)))
  #
  # ret <- ret + theme(axis.title.y = element_text(hjust = yj,
  #                                                family = axis_title_family,
  #                                                colour = axis_col,
  #                                                size = axis_title_size,
  #                                                face = axis_title_face,
  #                                                margin = margin(r = 10)))
  #
  # ret <- ret + theme(axis.title.y.right = element_text(hjust = yj,
  #                                                      angle = 90,
  #                                                      family = axis_title_family,
  #                                                      colour = axis_col,
  #                                                      size = axis_title_size,
  #                                                      face = axis_title_face,
  #                                                      margin = margin(t = 10)))

  ret <- ret + theme(axis.title = element_text(family = axis_title_family,
                                               colour = axis_col,
                                               size = axis_title_size,
                                               margin = margin(t = 10)))

  ret <- ret + theme(axis.title.x = element_text(hjust = xj,
                                                 family = axis_title_family,
                                                 colour = axis_col,
                                                 size = axis_title_size,
                                                 face = axis_title_face))

  ret <- ret + theme(axis.title.y = element_text(hjust = yj,
                                                 family = axis_title_family,
                                                 colour = axis_col,
                                                 size = axis_title_size,
                                                 face = axis_title_face))

  ret <- ret + theme(axis.title.y.right = element_text(hjust = yj,
                                                       angle = 90,
                                                       family = axis_title_family,
                                                       colour = axis_col,
                                                       size = axis_title_size,
                                                       face = axis_title_face))


  #### Border --------------------------------

  if (border == TRUE) {
    ret <- ret + theme(panel.border = element_rect(color = axis_col,
                                                   size = 0.3 * 2,
                                                   fill = NA))
  }


  #### Return theme --------------------------------

  ret

}



