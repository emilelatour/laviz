
#' @title A theme for my package
#'
#' @description
#' Credit here is owed to the package `hrbrthemes` by Bob Rudis. I took his code
#' and made tweaks based on my preferences and the changes I tend to make to his
#' themes.
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin
#'   plot title family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with `ggplot2::margin()`)
#' @param base_col,axis_col base & axis colors; both default to `"#323232"`, a
#'   dark gray. I also like to use `"#000000"` or `"black"`
#' @param grid_col grid colors; defaults to `"#D3D3D3"`
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_family,axis_text_size axis text font family and size
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @param border If `TRUE` adds border around plot area
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(palmerpenguins)
#'
#' laviz::view_palette(c("#323232", "#000000", "#D3D3D3", "black"))
#'
#' # Scatterplot
#' ggplot(data = penguins,
#'        aes(x = bill_length_mm,
#'            y = body_mass_g)) +
#'   geom_point(aes(colour = species)) +
#'   labs(x = "Bill length (mm)",
#'        y = "Body mass (g)",
#'        title = "ggplot2 scatterplot example",
#'        subtitle = "A plot that is only useful for demonstration purposes",
#'        caption = "Data comes from the palmerpenguins package.") +
#'   theme_laviz(axis = TRUE,
#'               ticks = TRUE)
#'
#'
#' # Bar chart example
#' count(penguins, species) %>%
#'   ggplot(data = .,
#'          aes(x = species,
#'              y = n)) +
#'   geom_col(width = 0.5) +
#'   geom_text(aes(label = n),
#'             nudge_y = 3.5) +
#'   labs(x = "Species",
#'        y = NULL,
#'        title = "ggplot2 bar chart example",
#'        subtitle = "Number of penguins by species",
#'        caption = "Data comes from the palmerpenguins package.") +
#'   theme_laviz(grid = "Y",
#'               axis = FALSE,
#'               ticks = FALSE) +
#'   theme(axis.text.y = element_blank())
#'
#' # Another scatterplot
#' ggplot(data = penguins,
#'        aes(x = flipper_length_mm,
#'            y = body_mass_g)) +
#'   geom_point(aes(colour = species,
#'                  shape = species),
#'              size = 2) +
#'   theme_laviz()  +
#'   scale_color_manual(values = c("darkorange", "darkorchid", "cyan4")) +
#'   labs(x = "Flipper length (mm)",
#'        y = "Body mass (g)",
#'        title = "Penguin flipper length versus body mass",
#'        subtitle = "There is a positive correlation between flipper length and body mass.",
#'        caption = "Data comes from the palmerpenguins package.",
#'        colour = "Species",
#'        shape = "Species")
#'
#' }

theme_laviz <- function(base_family = "Arial",
                        base_size = 11.5,
                        base_col = "#323232",
                        plot_title_family = base_family,
                        plot_title_size = 18,
                        plot_title_face = "bold",
                        plot_title_margin = 10,
                        subtitle_family = base_family,
                        subtitle_size = base_size,
                        subtitle_face = "plain",
                        subtitle_margin = 15,
                        strip_text_family = base_family,
                        strip_text_size = base_size,
                        strip_text_face = "plain",
                        caption_family = base_family,
                        caption_size = 9,
                        caption_face = "italic",
                        caption_margin = 10,
                        axis_text_family = base_family,
                        axis_text_size = base_size,
                        axis_title_family = base_family,
                        axis_title_size = base_size,
                        axis_title_face = "plain",
                        axis_title_just = "rt",
                        plot_margin = margin(t = 5, l = 5, b = 5, r = 20),
                        # plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#D3D3D3",
                        grid = TRUE,
                        axis_col = base_col,
                        axis = FALSE,
                        ticks = FALSE,
                        border = FALSE) {


  #### Base theme --------------------------------

  # ret <- ggplot2::theme_minimal(base_family = base_family,
  #                               base_size = base_size)

  ret <- ggplot2::theme_gray(base_family = base_family,
                                base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())
  ret <- ret + theme(panel.background = element_blank())
  ret <- ret + theme(strip.background = element_blank())


  #### Grid --------------------------------

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.2))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))

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
    ret <- ret + theme(axis.line = element_line(color = axis_col, size = 0.3))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.3))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.3))
      }
    } else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.3))
      ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.3))
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
    ret <- ret + theme(axis.ticks = element_line(colour = axis_col, size = 0.3))
    ret <- ret + theme(axis.ticks.x = element_line(colour = axis_col, size = 0.3))
    ret <- ret + theme(axis.ticks.y = element_line(colour = axis_col, size = 0.3))
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

  ret <- ret + theme(axis.title = element_text(family = axis_title_family,
                                               colour = axis_col,
                                               size = axis_title_size,
                                               margin = margin(t = 10)))

  ret <- ret + theme(axis.title.x = element_text(hjust = xj,
                                                 family = axis_title_family,
                                                 colour = axis_col,
                                                 size = axis_title_size,
                                                 face = axis_title_face,
                                                 margin = margin(t = 10)))

  ret <- ret + theme(axis.title.y = element_text(hjust = yj,
                                                 family = axis_title_family,
                                                 colour = axis_col,
                                                 size = axis_title_size,
                                                 face = axis_title_face,
                                                 margin = margin(r = 10)))

  ret <- ret + theme(axis.title.y.right = element_text(hjust = yj,
                                                       angle = 90,
                                                       family = axis_title_family,
                                                       colour = axis_col,
                                                       size = axis_title_size,
                                                       face = axis_title_face,
                                                       margin = margin(t = 10)))


  #### Strip text --------------------------------

  ret <- ret + theme(strip.text = element_text(hjust = 0,
                                               family = strip_text_family,
                                               colour = axis_col,
                                               size = strip_text_size,
                                               face = strip_text_face))


  #### Panel spacing and border--------------------------------

  ret <- ret + theme(panel.spacing = grid::unit(2.5, "lines"))

  ret <- ret + theme(panel.border = element_blank())


  #### Title, subtitle, caption --------------------------------

  ret <- ret + theme(plot.title = element_text(family = plot_title_family,
                                               colour = "#000000",
                                               size = plot_title_size,
                                               face = plot_title_face,
                                               margin = margin(b = plot_title_margin),
                                               hjust = 0))

  ret <- ret + theme(plot.subtitle = element_text(family = subtitle_family,
                                                  colour = "#000000",
                                                  size = subtitle_size,
                                                  face = subtitle_face,
                                                  margin = margin(b = subtitle_margin),
                                                  hjust = 0))

  ret <- ret + theme(plot.caption = element_text(family = caption_family,
                                                 colour = "#000000",
                                                 size = caption_size,
                                                 face = caption_face,
                                                 margin = margin(t = caption_margin),
                                                 hjust = 0.99))


  #### Plot margins --------------------------------

  ret <- ret + theme(plot.margin = plot_margin)


  #### Legend --------------------------------

  ret <- ret + theme(legend.text = element_text(family = base_family,
                                                colour = base_col,
                                                size = base_size,
                                                margin = margin(r = 10),
                                                hjust = 0))

  ret <- ret + theme(legend.title = element_text(family = base_family,
                                                 colour = base_col,
                                                 size = base_size,
                                                 margin = margin(r = 20)))

  ret <- ret + theme(legend.margin = margin(t = 20, b = 20),
                     # legend.key = element_rect(fill = "white"),
                     legend.key.height = unit(5, "mm"),
                     legend.key.width = unit(5, "mm"),
                     legend.direction = "vertical")


  #### Border --------------------------------

  if (border == TRUE) {
    ret <- ret + theme(panel.border = element_rect(color = axis_col,
                                                   size = 0.3 * 2,
                                                   fill = NA))
  }


  #### Return theme --------------------------------

  ret


}
