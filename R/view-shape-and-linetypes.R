#' @title
#' Dislpay the shapes and linetypes with codes
#'
#' @description
#' Quick way to display the shapes and linetypes available in base R and ggplot2
#' with their corresponding codes
#'
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom forcats fct_inorder
#' @importFrom tibble tribble
#'
#' @return A plot
#' @export
#'
#' @examples \dontrun{
#' view_shapes()
#' view_linetypes()
#' }

view_shapes <- function() {

  x <- y <- label <- NULL

  df <- tibble::tribble(
    ~pch,                                ~label, ~x, ~y,
    0L,                            "0, square", 1L, 1L,
    1L,                            "1, circle", 1L, 1L,
    2L,                 "2, triangle point up", 1L, 1L,
    3L,                              "3, plus", 1L, 1L,
    4L,                             "4, cross", 1L, 1L,
    5L,                           "5, diamond", 1L, 1L,
    6L,               "6, triangle point down", 1L, 1L,
    7L,                      "7, square cross", 1L, 1L,
    8L,                              "8, star", 1L, 1L,
    9L,                      "9, diamond plus", 1L, 1L,
    10L,                      "10, circle plus", 1L, 1L,
    11L,            "11, triangles up and down", 1L, 1L,
    12L,                      "12, square plus", 1L, 1L,
    13L,                     "13, circle cross", 1L, 1L,
    14L,         "14, square and triangle point up", 1L, 1L,
    15L,                   "15, filled square", 1L, 1L,
    16L,                   "16, filled circle", 1L, 1L,
    17L,        "17, filled triangle point-up", 1L, 1L,
    18L,                  "18, filled diamond", 1L, 1L,
    19L,                    "19, solid circle", 1L, 1L,
    20L,          "20, bullet (smaller circle)", 1L, 1L,
    21L,              "21, filled circle blue", 1L, 1L,
    22L,              "22, filled square blue", 1L, 1L,
    23L,             "23, filled diamond blue", 1L, 1L,
    24L,   "24, filled triangle point-up blue", 1L, 1L,
    25L, "25, filled triangle point down blue", 1L, 1L
  )

  df %>%
    mutate(label = forcats::fct_inorder(label)) %>%
    ggplot(data = .,
           aes(x = x,
               y = y)) +
    geom_point(aes(shape = label),
               size = 2.5) +
    facet_wrap(~ label,
               ncol = 4) +
    scale_shape_manual(values = df$pch , guide = "none") +
    theme_void() +
    theme(strip.text = element_text(size = 12,
                                    margin = margin(b = 5)))

}


#' @rdname view_shapes
#' @export
view_linetypes <- function() {

  x <- xend <- y <- label <- NULL

  lty_df <- tibble::tribble(
    ~label, ~lty_n,       ~lty, ~x, ~xend,
    "6, twodash",     6L,  "twodash", 0L,    1L,
    "5, longdash",     5L, "longdash", 0L,    1L,
    "4, dotdash",     4L,  "dotdash", 0L,    1L,
    "3, dotted",     3L,   "dotted", 0L,    1L,
    "2, dashed",     2L,   "dashed", 0L,    1L,
    "1, solid",     1L,    "solid", 0L,    1L,
    "0, blank",     0L,    "blank", 0L,    1L
  )

  lty_df %>%
    mutate(label = forcats::fct_inorder(label)) %>%
    ggplot(data = .,
           aes(x = x,
               y = label)) +
    geom_segment(aes(x = x,
                     xend = xend,
                     y = label,
                     yend = label,
                     linetype = label),
                 size = 1.0) +
    scale_linetype_manual(values = lty_df$lty) +
    theme_void() +
    theme(axis.text.y = element_text(colour = "black"),
          legend.position = "none")

}
