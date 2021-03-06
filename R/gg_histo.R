

#' Histograms with options for auto-binning
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
#' @param title The main title of the plot
#' @param subtitle A subtitle for the plot
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' gg_histo(data = diamonds,
#'          var = price,
#'          binw_select = "Sturges",
#'          facet = NULL)
#'
#' gg_histo(data = diamonds,
#'          var = price,
#'          binw_select = "Scott",
#'          facet = clarity)
#'
#' gg_histo(data = diamonds,
#'          var = price,
#'          binw_select = "FD",
#'          facet = clarity)
#'
#' gg_histo(data = diamonds,
#'          var = price,
#'          binw_select = "Square-root",
#'          facet = clarity)
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
#'
gg_histo <- function(data,
                     var,
                     binw_select = "FD",
                     facet = NULL,
                     alpha = 0.8,
                     fill = "steelblue",
                     colour = "black",
                     theme_for_histo = theme_minimal(),
                     title = NULL,
                     subtitle = NULL) {

  #### Checks for errors --------------------------------

  stopifnot(class(data) %in% c("tbl_df", "tbl", "data.frame"))

  if (!(binw_select %in% c("FD", "Sturges", "Scott",
                           "Square-root", "Rice"))) {
    stop("Method to select binwidth not known")
  }


  #### quo some vars brah --------------------------------

  var_enq <- rlang::enquo(var)
  var_name <- rlang::quo_name(var_enq)
  facet_enq <- rlang::enquo(facet)
  # facet_name <- rlang::quo_name(facet_enq)


  #### Titles --------------------------------

  # TODO -- Use the names to make a default title if NULL


  #### Different ways to calc bin width --------------------------------

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
    dplyr::filter(!is.na(!! var_enq)) %>%
    purrr::map_dbl(.x = .,
                   .f = ~ calc_bin_width(x = .x, binw_select = binw_select))


  #### Make the plot --------------------------------

  ## facet_wrap if not NULL ----------------

  if (rlang::quo_is_null(facet_enq) == FALSE) {

    ggplot(data = data,
           aes(x = !! var_enq)) +
      geom_histogram(stat = "bin",
                     binwidth = bin_width,
                     alpha = alpha,
                     fill = fill,
                     colour = colour) +
      facet_wrap(facet_enq,
                 scales = "free") +
      labs(title = var_name,
           x = "",
           subtitle = subtitle) +
      theme_for_histo

  } else {

    ggplot(data = data,
           aes(x = !! var_enq)) +
      geom_histogram(stat = "bin",
                     binwidth = bin_width,
                     alpha = alpha,
                     fill = fill,
                     colour = colour) +
      labs(title = var_name,
           x = "",
           subtitle = subtitle) +
      theme_for_histo

  }


}

 library(tidyverse)

gg_histo(data = diamonds,
         var = price,
         binw_select = "Sturges",
         facet = NULL)

gg_histo(data = diamonds,
         var = price,
         binw_select = "Scott",
         facet = clarity)

gg_histo(data = diamonds,
         var = price,
         binw_select = "FD",
         facet = clarity)

gg_histo(data = diamonds,
         var = price,
         binw_select = "Square-root",
         facet = clarity)


 gg_histo(data = diamonds,
          var = price,
          binw_select = "Sturges",
          facet = NULL)

 gg_histo(data = diamonds,
          var = price,
          binw_select = "Scott",
          facet = NULL)

 gg_histo(data = diamonds,
          var = price,
          binw_select = "Square-root",
          facet = NULL)

 gg_histo(data = diamonds,
          var = price,
          binw_select = "Rice",
          facet = NULL)

 ggplot(data = diamonds, aes(x = price)) +
   geom_density(alpha = 0.4,
                fill = "darkgreen") +
   theme_minimal()
