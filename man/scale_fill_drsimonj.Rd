% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drsimonj_palettes.R
\name{scale_fill_drsimonj}
\alias{scale_fill_drsimonj}
\title{Fill scale constructor for drsimonj colors}
\usage{
scale_fill_drsimonj(palette = "main", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Character name of palette in drsimonj_palettes}

\item{discrete}{Boolean indicating whether color aesthetic is discrete or not}

\item{reverse}{Boolean indicating whether the palette should be reversed}

\item{...}{Additional arguments passed to discrete_scale() or
scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE}
}
\description{
Fill scale constructor for drsimonj colors
}
\examples{
library(ggplot2)
# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_drsimonj(palette = "mixed", guide = "none")
}
