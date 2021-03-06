% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drsimonj_palettes.R
\docType{data}
\name{drsimonj_palettes}
\alias{drsimonj_palettes}
\title{drsimonj Colour Palettes}
\format{
An object of class \code{list} of length 5.
}
\usage{
drsimonj_palettes
}
\description{
A collection of colour palettes
}
\examples{

# Make an x-y plot using the drsimonj palette
library(ggplot2)
df <- data.frame(x = rnorm(100, 0, 20),
          y = rnorm(100, 0, 20),
          cl = sample(letters[1:8], 100, replace=TRUE))

ggplot(df, aes(x, y, colour = cl, shape = cl)) +
  geom_point(size = 4) +
  scale_colour_drsimonj() +
  theme_minimal() +
  theme(aspect.ratio = 1)

ggplot(df, aes(x, fill = cl)) +
  geom_histogram() +
  scale_fill_drsimonj(palette = "mixed")

}
\keyword{datasets}
