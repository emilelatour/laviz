

library(ggplot2)
library(rlang)


#' @title
#' A tasteful pie chart
#'
#' @description
#' Sometimes you gotta make a pie chart. And if you do, it should make some
#' efforts to hellp it to convey information clearly. Some critics say that pie
#' charts should never be used because the area representations (i.e. pie
#' slices) are difficuly for humans to make sense of. I agree and I disagree. I
#' avoid them as much as I can with my own data viz, bar charts seem to work
#' better to me. But sometimes you have a collaborator who really wants a pie
#' chart, and you want to be a team player. Also, with less than 4 groups, I
#' think that they do just fine. Thought that should come witha couple of
#' caveats: (1) if the pie slices are really small then it becomes really
#' difficult to tell what value they represent, and (2) if mulitple pie slices
#' are close in size the it becomes difficult to tell them apart.
#'
#' @param data A data frame or tibble
#' @param var Variable with the data to be sliced
#' @param over_ride Logical; The function won't let you plot more than 4 groups
#'   when `over_ride = FALSE`.
#' @param ... Additional function arguments
#'
#' @import rlang
#' @import ggplot2
#'
#' @export
#'
#' @examples \dontrun{
#' gg_pie(data = ggplot2::mpg, var = class)
#' gg_pie(data = ggplot2::mpg, var = class, over_ride = TRUE)
#' gg_pie(data = ggplot2::diamonds, var = clarity)
#' gg_pie(data = ggplot2::diamonds, var = clarity, over_ride = TRUE)
#' }

gg_pie <- function(data, var, over_ride = FALSE, ...) {

  #### Enquo vars --------------------------------

  var <- rlang::enquo(var)

  #### Checks for errors --------------------------------

  unique_lvls <- data %>%
    dplyr::pull(!! var) %>%
    unique(.) %>%
    length(.)

  if (unique_lvls > 4 & !over_ride) {
    stop("That's a lot of categories. Consider a bar chart instead.")
  }


  #### Make the pie chart --------------------------------

  piechart_basic(data, aes(factor(1), fill = !! var))

}



#### Basic function to be used in the main --------------------------------

piechart_basic <- function(data, mapping) {
  ggplot(data, mapping) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL)
}
# piechart_basic(mpg, aes(factor(1), fill = class))

