

#### 1. Named vector of hex codes for the corporate colors --------------------------------

# Create a named vector of corporate colors

# ohsu corporate colors
ohsu_colors <- c(
  `horizon yellow`     = "#FFC939",
  `marquam blue`       = "#5E97C9",
  `ohsu gray`          = "#585E60",
  `terwilliger green`  = "#57B147",
  `dark blue`          = "#002776",
  `orange`             = "#ED6D23",
  `pink red`           = "#ED1941",
  `cyan blue`          = "#0E4D8F",
  `red`                = "#C34D36")



#### 2. Function to access hex codes (in 1) --------------------------------

# Write a function that extracts the hex codes from this vector by name. This
# allows us to get hex colors in a robust and flexible way. For example, you can
# have all colors returned as they are, specify certain colors, in a particular
# order, add additional function arguments and checks, and so on.

#' @title
#' OHSU colors
#'
#' @description
#' Function to extract ohsu colors as hex codes
#'
#' \tabular{ll}{
#'   horizon yellow    \tab #FFC939\cr
#'   marquam blue      \tab #5E97C9\cr
#'   ohsu gray         \tab #585E60\cr
#'   terwilliger green \tab #57B147\cr
#'   dark blue         \tab #002776\cr
#'   orange            \tab #ED6D23\cr
#'   pink red          \tab #ED1941\cr
#'   cyan blue         \tab #0E4D8F\cr
#'   red               \tab #C34D36
#' }
#'
#' @param ... Character names of ohsu_colors
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ohsu_cols()
#' ohsu_cols("horizon yellow")
#' ohsu_cols("horizon yellow", "terwilliger green")
#' ohsu_cols("terwilliger green", "horizon yellow")
#'
#' ggplot(data = mtcars,
#'        aes(x = hp,
#'            y = mpg)) +
#'   geom_point(color = ohsu_cols("terwilliger green"),
#'              size = 4,
#'              alpha = 0.8) +
#'   theme_minimal()

ohsu_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (ohsu_colors)

  ohsu_colors[cols]
}


#### 3. Named list of corporate color palettes (combinations of colors via 2) --------------------------------

# we can now create palettes (various combinations) of these colors. Similar to
# how we deal with colors, first define a list like such:

#' OHSU Colour Palettes
#'
#'A collection of colour palettes
#'
#' @export
#'
#'@examples
#'
#' # Make an x-y plot using the OHSU palette
#' library(ggplot2)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#'
#' ggplot(df, aes(x, y, colour = cl, shape = cl)) +
#'   geom_point(size = 4) +
#'   scale_colour_ohsu() +
#'   theme_minimal() +
#'   theme(aspect.ratio = 1)
#'
#' ggplot(df, aes(x, fill = cl)) +
#'   geom_histogram() +
#'   scale_fill_ohsu(palette = "mixed")
#'
#' @export
ohsu_palettes <- list(
  `main`  = ohsu_cols("horizon yellow", "marquam blue", "ohsu gray", "terwilliger green"),
  `full`  = ohsu_cols("horizon yellow", "marquam blue", "ohsu gray", "terwilliger green", "dark blue", "orange", "pink red", "cyan blue", "red"),
  `cool`  = ohsu_cols("marquam blue", "terwilliger green", "dark blue", "cyan blue"),
  `hot`   = ohsu_cols("horizon yellow", "orange", "pink red", "red"),
  `mixed` = ohsu_cols("marquam blue", "terwilliger green", "horizon yellow", "orange", "pink red"),
  `grey`  = ohsu_cols("ohsu gray", "marquam blue")
)


#### 4. Function to access palettes (in 3) --------------------------------

#' Return function to interpolate an OHSU color palette
#'
#' @param palette Character name of palette in ohsu_palettes
#' @param alpha transparency
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' ohsu_pal("cool")
#'
#' library(scales)
#' scales::show_col(ohsu_pal("cool")(10))
#' scales::show_col(ohsu_pal("main")(4))
#' scales::show_col(ohsu_pal("full")(9))
#' scales::show_col(ohsu_pal("cool")(4))
#' scales::show_col(ohsu_pal("hot")(4))
#' scales::show_col(ohsu_pal("mixed")(5))
#' scales::show_col(ohsu_pal("grey")(4))
#'
#' filled.contour(volcano, color.palette = ohsu_pal(), asp = 1)
ohsu_pal <- function(palette = "main",
                     alpha = 1,
                     reverse = FALSE, ...) {

  pal <- ohsu_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  colorRampPalette(pal, ...)
}

#### 5. ggplot2-compatible scale functions that use the corporate palettes (via 4) --------------------------------

# https://github.com/ropenscilabs/ochRe

# One function is created for color and another for fill, and each contains a
# boolean argument for the relevant aesthetic being discrete or not.

#' Color scale constructor for ohsu colors
#'
#' @rdname scale_color_ohsu
#'
#' @param palette Character name of palette in ohsu_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @inheritParams viridis::scale_color_viridis
#' @inheritParams ohsu_pal
#' @importFrom ggplot2 scale_colour_manual
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Color by discrete variable using default palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_point(size = 4) +
#'     scale_color_ohsu()
#'
#' # Color by numeric variable with cool palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'     geom_point(size = 4, alpha = .6) +
#'     scale_color_ohsu(discrete = FALSE, palette = "cool")

scale_color_ohsu <- function(palette = "main",
                             discrete = TRUE,
                             reverse = FALSE, ...) {

  pal <- ohsu_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("ohsu_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_ohsu
#' @export
scale_colour_ohsu <- scale_color_ohsu


#' Fill scale constructor for ohsu colors
#'
#' @param palette Character name of palette in ohsu_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams ohsu_pal
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
#'     scale_fill_ohsu(palette = "mixed", guide = "none")
scale_fill_ohsu <- function(palette = "main",
                            discrete = TRUE,
                            reverse = FALSE, ...) {

  pal <- ohsu_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("ohsu_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
