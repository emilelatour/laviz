# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

#### 1. Named vector of hex codes for the corporate colors --------------------------------

# Create a named vector of corporate colors

# drsimonj corporate colors
drsimonj_colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")


#### 2. Function to access hex codes (in 1) --------------------------------

# Write a function that extracts the hex codes from this vector by name. This
# allows us to get hex colors in a robust and flexible way. For example, you can
# have all colors returned as they are, specify certain colors, in a particular
# order, add additional function arguments and checks, and so on.

#' @title
#' drsimonj colors
#'
#' @description
#' Function to extract drsimonj colors as hex codes
#'
#' \tabular{ll}{
#'   red        \tab #d11141\cr
#'   green      \tab #00b159\cr
#'   blue       \tab #00aedb\cr
#'   orange     \tab #f37735\cr
#'   yellow     \tab #ffc425\cr
#'   light grey \tab #cccccc\cr
#'   dark grey  \tab #8c8c8c
#' }
#'
#' @param ... Character names of drsimonj_colors
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' drsimonj_cols()
#' drsimonj_cols("red")
#' drsimonj_cols("red", "blue")
#' drsimonj_cols("blue", "red")
#'
#' ggplot(data = mtcars,
#'        aes(x = hp,
#'            y = mpg)) +
#'   geom_point(color = drsimonj_cols("red"),
#'              size = 4,
#'              alpha = 0.8) +
#'   theme_minimal()

drsimonj_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (drsimonj_colors)

  drsimonj_colors[cols]
}


#### 3. Named list of corporate color palettes (combinations of colors via 2) --------------------------------

# we can now create palettes (various combinations) of these colors. Similar to
# how we deal with colors, first define a list like such:

#' drsimonj Colour Palettes
#'
#'A collection of colour palettes
#'
#' @export
#'
#'@examples
#'
#' # Make an x-y plot using the drsimonj palette
#' library(ggplot2)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#'
#' ggplot(df, aes(x, y, colour = cl, shape = cl)) +
#'   geom_point(size = 4) +
#'   scale_colour_drsimonj() +
#'   theme_minimal() +
#'   theme(aspect.ratio = 1)
#'
#' ggplot(df, aes(x, fill = cl)) +
#'   geom_histogram() +
#'   scale_fill_drsimonj(palette = "mixed")
#'
#' @export
drsimonj_palettes <- list(
  `main`  = drsimonj_cols("blue", "green", "yellow"),
  `cool`  = drsimonj_cols("blue", "green"),
  `hot`   = drsimonj_cols("yellow", "orange", "red"),
  `mixed` = drsimonj_cols("blue", "green", "yellow", "orange", "red"),
  `grey`  = drsimonj_cols("light grey", "dark grey")
)


#### 4. Function to access palettes (in 3) --------------------------------

#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param alpha transparency
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' drsimonj_pal("cool")
#'
#' library(scales)
#' scales::show_col(drsimonj_pal("cool")(10))
#'
#' filled.contour(volcano, color.palette = drsimonj_pal(), asp = 1)
drsimonj_pal <- function(palette = "main",
                         alpha = 1,
                         reverse = FALSE, ...) {

  pal <- drsimonj_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  colorRampPalette(pal, ...)
}


#### 5. ggplot2-compatible scale functions that use the corporate palettes (via 4) --------------------------------

# https://github.com/ropenscilabs/ochRe

# One function is created for color and another for fill, and each contains a
# boolean argument for the relevant aesthetic being discrete or not.

#' Color scale constructor for drsimonj colors
#'
#' @rdname scale_color_drsimonj
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# #' @inheritParams viridis::scale_color_viridis
# #' @inheritParams drsimonj_pal
#' @importFrom ggplot2 scale_colour_manual
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Color by discrete variable using default palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_point(size = 4) +
#'     scale_color_drsimonj()
#'
#' # Color by numeric variable with cool palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'     geom_point(size = 4, alpha = .6) +
#'     scale_color_drsimonj(discrete = FALSE, palette = "cool")

scale_color_drsimonj <- function(palette = "main",
                                 discrete = TRUE,
                                 reverse = FALSE, ...) {

  pal <- drsimonj_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_drsimonj
#' @export
scale_colour_drsimonj <- scale_color_drsimonj


#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
# #' @inheritParams viridis::scale_fill_viridis
# #' @inheritParams drsimonj_pal
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
#'     scale_fill_drsimonj(palette = "mixed", guide = "none")
scale_fill_drsimonj <- function(palette = "main",
                                discrete = TRUE,
                                reverse = FALSE, ...) {

  pal <- drsimonj_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}





