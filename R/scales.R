#' Default scale mit SGB-Farben
#'
#' @param ... Beliebige Parameter die an die Funktion \code{scale_color_manual()} weitergegeben werden koennen.
#'
#' @return see return of \code{scale_color_manual()}.
#' @export
scale_colour_discrete <-
  function(...) {
    scale_color_manual(values = usecol(pal = pal_sgb_pref))
}

#' Default scale mit SGB-Farben
#'
#' @param ... Beliebige Parameter die an die Funktion \code{scale_fill_manual()} weitergegeben werden koennen.
#'
#' @return see return of \code{scale_fill_manual()}.
#' @export
scale_fill_discrete <- function(...) {
  scale_fill_manual(values = usecol(pal = pal_sgb_pref))
}

# scale_colour_gradient <- function(...) scale_color_gradient(low = pal_sgb_rot[1], high = pal_sgb_rot[5])
# scale_colour_gradient <- function(...) scale_color_gradient(low = "#fdc0b0", high = "#660000")

# scale_fill_gradient2 <- function(...) scale_fill_gradientn(colours = usecol(pal = pal_sgb_rot))


