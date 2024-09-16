
#### 1. Named vector of hex codes for the corporate colors --------------------------------

# Create a named vector of corporate colors

# wom colors
wom_colors <- c(
  `blue`      = "#007bff",
  `indigo`    = "#6610f2",
  `purple`    = "#6f42c1",
  `pink`      = "#e83e8c",
  `red`       = "#dc3545",
  `orange`    = "#fd7e14",
  `yellow`    = "#ffc107",
  `green`     = "#28a745",
  `teal`      = "#20c997",
  `cyan`      = "#17a2b8",
  `white`     = "#ffffff",
  `gray`      = "#6c757d",
  `graydark`  = "#343a40",
  `primary`   = "#007bff",
  `secondary` = "#6c757d",
  `success`   = "#28a745",
  `info`      = "#17a2b8",
  `warning`   = "#ffc107",
  `danger`    = "#dc3545",
  `light`     = "#f8f9fa",
  `dark`      = "#343a40")




#### 2. Function to access hex codes (in 1) --------------------------------

# Write a function that extracts the hex codes from this vector by name. This
# allows us to get hex colors in a robust and flexible way. For example, you can
# have all colors returned as they are, specify certain colors, in a particular
# order, add additional function arguments and checks, and so on.

#' @title
#' WoM colors
#'
#' @description
#' Function to extract wom colors as hex codes
#'
#' \tabular{ll}{
#'   blue      \tab #007bff\cr
#'   indigo    \tab #6610f2\cr
#'   purple    \tab #6f42c1\cr
#'   pink      \tab #e83e8c\cr
#'   red       \tab #dc3545\cr
#'   orange    \tab #fd7e14\cr
#'   yellow    \tab #ffc107\cr
#'   green     \tab #28a745\cr
#'   teal      \tab #20c997\cr
#'   cyan      \tab #17a2b8\cr
#'   white     \tab #ffffff\cr
#'   gray      \tab #6c757d\cr
#'   graydark  \tab #343a40\cr
#'   primary   \tab #007bff\cr
#'   secondary \tab #6c757d\cr
#'   success   \tab #28a745\cr
#'   info      \tab #17a2b8\cr
#'   warning   \tab #ffc107\cr
#'   danger    \tab #dc3545\cr
#'   light     \tab #f8f9fa\cr
#'   dark      \tab #343a40
#' }
#'
#' @param ... Character names of wom_cols
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' wom_cols()
#' wom_cols("red")
#' wom_cols("red", "blue")
#' wom_cols("blue", "red")
#'
#' ggplot(data = mtcars,
#'        aes(x = hp,
#'            y = mpg)) +
#'   geom_point(color = wom_cols("red"),
#'              size = 4,
#'              alpha = 0.8) +
#'   theme_minimal()

wom_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (wom_colors)

  wom_colors[cols]
}


#### 3. Named list of corporate color palettes (combinations of colors via 2) --------------------------------

# we can now create palettes (various combinations) of these colors. Similar to
# how we deal with colors, first define a list like such:

#' @title
#' WoM Colour Palettes
#'
#' @description
#' A collection of colour palettes
#'
#' @export
#'
#'@examples
#'
#' # Make an x-y plot using the wom palette
#' library(ggplot2)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#'
#' ggplot(df, aes(x, y, colour = cl, shape = cl)) +
#'   geom_point(size = 4) +
#'   scale_colour_wom() +
#'   theme_minimal() +
#'   theme(aspect.ratio = 1)
#'
#' ggplot(df, aes(x, fill = cl)) +
#'   geom_histogram() +
#'   scale_fill_wom(palette = "mixed")
#'
#' @export
wom_palettes <- list(
  `full`  = wom_cols("blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green", "teal", "cyan", "white", "gray", "graydark"),
  `full2` = wom_cols("primary", "secondary", "success", "info", "warning", "danger", "light", "dark"),
  `main`  = wom_cols("blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green", "teal", "cyan"),
  `cool`  = wom_cols("blue", "indigo", "purple", "green", "teal", "cyan"),
  `hot`   = wom_cols("pink", "red", "orange", "yellow"),
  `mixed` = wom_cols("blue", "green", "yellow", "orange", "red"),
  `grey`  = wom_cols("gray", "graydark")
)

#### 4. Function to access palettes (in 3) --------------------------------

#' Return function to interpolate a wom color palette
#'
#' @param palette Character name of palette in wom_palettes
#' @param alpha transparency
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' wom_pal("cool")
#'
#' library(scales)
#' scales::show_col(wom_pal("cool")(10))
#'
#' filled.contour(volcano, color.palette = wom_pal(), asp = 1)
wom_pal <- function(palette = "main",
                    alpha = 1,
                    reverse = FALSE, ...) {

  pal <- wom_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  colorRampPalette(pal, ...)
}


#### 5. ggplot2-compatible scale functions that use the corporate palettes (via 4) --------------------------------

# https://github.com/ropenscilabs/ochRe

# One function is created for color and another for fill, and each contains a
# boolean argument for the relevant aesthetic being discrete or not.

#' Color scale constructor for wom colors
#'
#' @rdname scale_color_wom
#'
#' @param palette Character name of palette in wom_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# #' @inheritParams viridis::scale_color_viridis
# #' @inheritParams wom_pal
#' @importFrom ggplot2 scale_colour_manual
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Color by discrete variable using default palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_point(size = 4) +
#'     scale_color_wom()
#'
#' # Color by numeric variable with cool palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'     geom_point(size = 4, alpha = .6) +
#'     scale_color_wom(discrete = FALSE, palette = "cool")

scale_color_wom <- function(palette = "main",
                            discrete = TRUE,
                            reverse = FALSE, ...) {

  pal <- wom_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("wom_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_wom
#' @export
scale_colour_wom <- scale_color_wom


#' Fill scale constructor for wom colors
#'
#' @param palette Character name of palette in wom_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# #' @inheritParams viridis::scale_fill_viridis
# #' @inheritParams wom_pal
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Fill by discrete variable with different palette + remove legend (guide)
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'     geom_bar() +
#'     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'     scale_fill_wom(palette = "mixed", guide = "none")
scale_fill_wom <- function(palette = "main",
                           discrete = TRUE,
                           reverse = FALSE, ...) {

  pal <- wom_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("wom_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}





