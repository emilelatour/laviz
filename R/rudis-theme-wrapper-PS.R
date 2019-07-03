#' @title
#' Wrappers for hrbrthemes that work a little more consistently on my PC using the IBM Plex Sans font.
#'
#' @references
#' /url{https://github.com/hrbrmstr/hrbrthemes}
#'
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin()])
#' @param grid_col,axis_col grid & axis colors; both default to `#cccccc`
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_size font size of axis text
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#'
#' @import hrbrthemes
#' @import extrafont
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import scales
#'
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' extrafont::loadfonts(device = "win", quiet = TRUE)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_rudis_ps()
#'
#' # seminal bar chart
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_rudis_ps(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
#'
#'
theme_rudis_ps <- function(base_family = "IBM Plex Sans",
                           base_size = 11.5,
                           plot_title_family = "IBM Plex Sans",
                           plot_title_size = 18,
                           plot_title_face = "bold",
                           plot_title_margin = 10,
                           subtitle_family = "IBM Plex Sans Light",
                           subtitle_size = 13,
                           subtitle_face = "plain",
                           subtitle_margin = 15,
                           strip_text_family = "IBM Plex Sans Medium",
                           strip_text_size = 12,
                           strip_text_face = "plain",
                           caption_family = "IBM Plex Sans Medium",
                           caption_size = 9,
                           caption_face = "plain",
                           caption_margin = 10,
                           axis_text_size = 9,
                           axis_title_family = base_family,
                           axis_title_size = 9,
                           axis_title_face = "plain",
                           axis_title_just = "rt",
                           plot_margin = margin(30, 30, 30, 30),
                           grid_col = "#cccccc",
                           grid = TRUE,
                           axis_col = "#cccccc",
                           axis = FALSE,
                           ticks = FALSE) {

  hrbrthemes::theme_ipsum_ps(base_family  = base_family ,
                             base_size  = base_size ,
                             plot_title_family  = plot_title_family ,
                             plot_title_size  = plot_title_size ,
                             plot_title_face  = plot_title_face ,
                             plot_title_margin  = plot_title_margin ,
                             subtitle_family  = subtitle_family ,
                             subtitle_size  = subtitle_size ,
                             subtitle_face  = subtitle_face ,
                             subtitle_margin  = subtitle_margin ,
                             strip_text_family  = strip_text_family ,
                             strip_text_size  = strip_text_size ,
                             strip_text_face  = strip_text_face ,
                             caption_family  = caption_family ,
                             caption_size  = caption_size ,
                             caption_face  = caption_face ,
                             caption_margin  = caption_margin ,
                             axis_text_size  = axis_text_size ,
                             axis_title_family  = axis_title_family ,
                             axis_title_size  = axis_title_size ,
                             axis_title_face  = axis_title_face ,
                             axis_title_just  = axis_title_just ,
                             plot_margin  = plot_margin ,
                             grid_col  = grid_col ,
                             grid  = grid ,
                             axis_col  = axis_col ,
                             axis  = axis ,
                             ticks  = ticks)
}


