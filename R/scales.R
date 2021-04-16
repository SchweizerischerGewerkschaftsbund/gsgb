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


# Continuous colour:
# scale_colour_gradient <- function(...) scale_color_gradient(low = pal_sgb_rot[1], high = pal_sgb_rot[5])
# scale_colour_gradient <- function(...) scale_color_gradient(low = "#fdc0b0", high = "#660000")


# Continuous fill - das funktioniert schon, nun noch andere machen udn tests durchfuehren
scale_fill_gradient <- function(..., low = "#660000", high = "#fdc0b0", space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") {
  ggplot2::scale_fill_gradient(..., low = low, high = high, space = space,
                               na.value = na.value, guide = guide, aesthetics = aesthetics)
}

# Continuous colour / color
scale_colour_gradient <- function(..., low = "#660000", high = "#fdc0b0", space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "colour") {
  ggplot2::scale_colour_gradient(..., low = low, high = high, space = space,
                               na.value = na.value, guide = guide, aesthetics = aesthetics)
}
scale_color_gradient <- function(..., low = "#660000", high = "#fdc0b0", space = "Lab",
                                  na.value = "grey50", guide = "colourbar", aesthetics = "color") {
  ggplot2::scale_color_gradient(..., low = low, high = high, space = space,
                                 na.value = na.value, guide = guide, aesthetics = aesthetics)
}

# nun noch:
# gradientn
# gradient2
# scale_fill_hue ?
# scale_fill_gradient2 <- function(...) scale_fill_gradientn(colours = usecol(pal = pal_sgb_rot))


# Zum testen:
# ggplot(faithfuld, aes(waiting, eruptions, color = density)) +
#   geom_tile() +
#   scale_color_gradient()

