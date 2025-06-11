#' List available palettes
#'
#' @param n Number of rows to print (default: 10). Use `Inf` to print all rows.
#' @return A tibble with collection, palette, and n
#' @export
#'
#' @examples
#' list_palettes(20)
#' list_palettes(Inf)
list_palettes <- function(n = 10) {
  tbl <- purrr::map_dfr(names(color_palettes), function(coll) {
    purrr::map_dfr(names(color_palettes[[coll]]), function(pal) {
      tibble::tibble(
        collection = coll,
        palette = pal,
        n = length(color_palettes[[coll]][[pal]])
      )
    })
  })

  print(tbl, n = n)
  invisible(tbl)  # So it can still be used programmatically
}

