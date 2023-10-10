
#' @title
#' Calculate bin width for a histogram using various recomended methods
#'
#' @description
#' There are many different methods that have been recommended over the years to
#' determine an appropriate bin width for a histogram. This function collects
#' those methods so that they are available easily. No one is really recommended
#' over another. A few should be considered and then choose the best to
#' visualize the distribution.
#'
#' @param x A numeric object
#' @param binw_select Character choice for which method to use to calculate bin
#'   width: "FD" (Default), "Sturges", "Scott", "Square-root", "Rice",
#'   "Shimazaki", "Juran"
#'
#' @importFrom dplyr between
#' @importFrom graphics hist
#'
#' @return
#' A numeric
#'
#' @export
#'
#' @references
#' https://en.wikipedia.org/wiki/Histogram#Square-root_choice
#' https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
#' https://arxiv.org/pdf/1612.07216.pdf
#' https://cran.r-project.org/web/packages/essHist/essHist.pdf
#' https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.170.173&rep=rep1&type=pdf
#' https://www.neuralengine.org//res/histogram.html
#' https://www.qimacros.com/histogram-excel/how-to-determine-histogram-bin-interval/#:~:text=Here's%20How%20to%20Calculate%20the,data%20points%20and%20round%20up.
#'
#' @examples
#' library(ggplot2)
#'
#' calc_bin_width(diamonds$carat, binw_select = "FD")
#'
#' ggplot(data = diamonds,
#'        aes(x = carat)) +
#'   geom_histogram(colour = "white",
#'                  binwidth = calc_bin_width(diamonds$carat, binw_select = "FD"))
#'
#'
#' calc_bin_width(diamonds$carat, "Juran")
#'
#' ggplot(data = diamonds,
#'        aes(x = carat)) +
#'   geom_histogram(colour = "white",
#'                  binwidth = calc_bin_width(diamonds$carat, binw_select = "Juran"))
#'
#'
#' ggplot(economics_long, aes(value)) +
#'   facet_wrap(~variable, scales = 'free_x') +
#'   geom_histogram(colour = "white",
#'                  binwidth = function(x) calc_bin_width(x, binw_select = "FD"))
#'
#'
#' ggplot(economics_long, aes(value)) +
#'   facet_wrap(~variable, scales = 'free_x') +
#'   geom_histogram(colour = "white",
#'                  binwidth = function(x) calc_bin_width(x, binw_select = "Sturges"))

calc_bin_width <- function(x, binw_select = "FD") {

  x <- x[!is.na(x)]


  if (binw_select == "FD") {
    # Freedman-Diaconis (1981)
    # This is the same as what Hadley shows here
    # https://ggplot2.tidyverse.org/reference/geom_histogram.html
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

  } else if (binw_select == "Shimazaki") {

    N <- 2: 100
    C <- numeric(length(N))
    D <- C

    for (i in 1:length(N)) {
      D[i] <- diff(range(x))/N[i]

      edges = seq(min(x),max(x),length=N[i])
      hp <- hist(x, breaks = edges, plot=FALSE )
      ki <- hp$counts

      k <- mean(ki)
      v <- sum((ki-k)^2)/N[i]

      C[i] <- (2*k-v)/D[i]^2	#Cost Function
    }

    idx <- which.min(C)
    optD <- D[idx]

    edges <- seq(min(x),max(x),length=N[idx])

    diff(edges)[[1]]

  } else if(binw_select == "Juran") {

    n_bin <- dplyr::case_when(
      length(x) < 20 ~ 5,
      dplyr::between(length(x), 20, 50) ~ 6,
      dplyr::between(length(x), 51, 100) ~ 7,
      dplyr::between(length(x), 101, 200) ~ 8,
      dplyr::between(length(x), 201, 500) ~ 9,
      dplyr::between(length(x), 501, 1000) ~ 10,
      # 1000+	11-20
      length(x) > 1000 ~ min(ceiling((length(x) - 1000) / 1000), 20))

    diff(pretty((max(x) - min(x)) / n_bin))[[1]]

  }
}

