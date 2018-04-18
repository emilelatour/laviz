

#' histogg
#'
#' Function to plot histograms with specified bin width. See reference:
#' \url{https://www.answerminer.com/blog/binning-guide-ideal-histogram}.
#'
#' @param data Data frame or tibble
#' @param var Variable to be plotted
#' @param binw_select Specify method to calculate the bin width. "FD" for
#'   Freedman-Diaconis (1981) (default), "Sturges" for Sturges (1926),
#'   "Scott" for Scott (1979), "Square-root" for Square-root (N/A), or
#'   "Rice" for Rice (1944).
#' @param facet Variable name to facet wrap by. Default is NULL.
#' @param alpha How solid / transparent should the fill be.
#' @param fill Fill colour for histrogram.
#' @param colour Outline colour of histogram blocks.
#' @param theme_for_histo Specify the ggplot theme. Default is theme_minimal().
#'
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse)
#'
#' histogg(data = diamonds,
#'         var = price,
#'         binw_select = "FD",
#'         facet = NULL)
#'
#' histogg(data = diamonds,
#'         var = price,
#'         binw_select = "Sturges",
#'         facet = NULL)
#'
#' histogg(data = diamonds,
#'         var = price,
#'         binw_select = "Scott",
#'         facet = NULL)
#'
#' histogg(data = diamonds,
#'         var = price,
#'         binw_select = "Square-root",
#'         facet = NULL)
#'
#' histogg(data = diamonds,
#'         var = price,
#'         binw_select = "Rice",
#'         facet = NULL)
#'
#' ggplot(data = diamonds, aes(x = price)) +
#'   geom_density(alpha = 0.4,
#'                fill = "darkgreen") +
#'   theme_minimal()
#'
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @importFrom stats as.formula
#' @importFrom grDevices nclass.FD
#' @importFrom grDevices nclass.Sturges
#' @importFrom grDevices nclass.scott
#'
histogg <- function(data,
                    var,
                    binw_select = "FD",
                    facet = NULL,
                    alpha = 0.8,
                    fill = "steelblue",
                    colour = "black",
                    theme_for_histo = theme_minimal()) {

  var_enq <- rlang::enquo(var)

  ## Make the base plot ----------------

  g <- ggplot(data = data, ggplot2::aes_(x = var_enq))


  ## Different ways to calc bin width ----------------

  calc_bin_width <- function(x, binw_select) {

    if (binw_select == "FD") {
      # Freedman-Diaconis (1981)
      pretty(range(x), n = nclass.FD(x), min.n = 1, right = TRUE)[[2]] -
        pretty(range(x), n = nclass.FD(x), min.n = 1, right = TRUE)[[1]]

    } else if (binw_select == "Sturges") {
      # Sturges (1926)
      pretty(range(x), n = nclass.Sturges(x), min.n = 1, right = TRUE)[[2]] -
        pretty(range(x), n = nclass.Sturges(x), min.n = 1, right = TRUE)[[1]]

    } else if (binw_select == "Scott") {
      # Scott (1979)
      pretty(range(x), n = nclass.scott(x), min.n = 1, right = TRUE)[[2]] -
        pretty(range(x), n = nclass.scott(x), min.n = 1, right = TRUE)[[1]]

    } else if (binw_select == "Square-root") {
      # Square-root (N/A)
      diff(pretty((max(x) - min(x)) / sqrt(length(x))))[[1]]

    } else if (binw_select == "Rice") {
      # Rice (1944)
      diff(pretty((max(x) - min(x)) / 2 * (length(x) ^ (1 / 3))))[[1]]

    }
  }

  bin_width <- data %>%
    dplyr::select(!! var_enq) %>%
    purrr::map_dbl(.x = .,
                   .f = ~ calc_bin_width(x = .x, binw_select = binw_select))

  ## Plot given bin width above ----------------

  g <- g + geom_histogram(
    stat = "bin",
    binwidth = bin_width,
    alpha = alpha,
    fill = fill,
    colour = colour)


  ## Add a theme ----------------

  g <- g +
    theme_for_histo


  ## facet_wrap if applicable ----------------

  if (!is.null(facet)) {

    g <- g +
      facet_wrap(as.formula(paste("~ ", facet)), scales = 'free_x')

  }


  ## Plot it ----------------

  print(g)

}
