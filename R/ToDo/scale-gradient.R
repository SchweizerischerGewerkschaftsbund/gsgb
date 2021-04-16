## scale_gradient.R  |  gsgb
## SGB | 25.03.2021 | Kristina Schuepbach
## ---------------------------

# Idee: zusaetzliche scale_color_* und scale_fill_* von ggplot ueberschreiben, damit
# default-maessig die sgb-Farben verwendet werden.

# Von hadley (https://ggplot2-book.org/scale-colour.html):
# The default scale for continuous fill scales is scale_fill_continuous()
# which in turn defaults to scale_fill_gradient().
# scale_fill_gradient() produces a two-colour gradient
# scale_fill_gradient2() produces a three-colour gradient with specified midpoint
# scale_fill_gradientn() produces an n-colour gradient

# The default scale for discrete colours is scale_fill_discrete() which in turn defaults to
# scale_fill_hue().


# To Do:
# scale_fill_gradient
# scale_fill_gradient2
# scale_fill_gradientn
# ev. scale_fill_continuous (but defaults to scale_fill_gradient)
# scale_fill_hue
# ev. scale_fill_steps oder scale_fill_binned
# same for scale_colour_* and scale_color_*


# Kommentar David:
# Bei einer ersten Anwendung gestern ist mir aufgefallen, dass die facet_grid/facet_wrap-plots (https://ggplot2.tidyverse.org/reference/facet_grid.html) nicht schön kommen. Hier müssten wir sicher nochmals speziell anschauen, was genau schief läuft.
# Evt. wäre es zudem hilfreich die SGB-Farbskalen automatisch eingebaut zu haben, also das
# Standardfarbschema von ggplot2 mit dem Laden des Pakets direkt zu überschreiben.
# Wir müssten dann nicht bei jedem Plot + scale_color_sgb() hinzufügen.
# Das geht, indem man die ggplot2-Funktionen für die Farben überschreibt. Z.B.:
#   scale_colour_discrete <- function(...) scale_color_tableau(palette="Classic 10")
#   scale_fill_discrete <- function(...) scale_fill_tableau(palette="Classic 10")
#   scale_colour_gradient2 <- function(...) scale_colour_gradient2_tableau(palette="Classic Orange-Blue")
#   scale_fill_gradient2 <- function(...) scale_fill_gradient2_tableau(palette="Classic Orange-Blue")
#   Anstatt der scale_color_tableau()-Funktion aus dem ggthemes Paket würden wir dann  scale_color_sgb()-Funktion einsetzen.



# das ist der code von ggplot, der ueberschrieben werden soll - Beschreibung kann auch gekuerzt werden

#' Gradient colour scales
#'
#' `scale_*_gradient` creates a two colour gradient (low-high),
#' `scale_*_gradient2` creates a diverging colour gradient (low-mid-high),
#' `scale_*_gradientn` creates a n-colour gradient.
#'
#' Default colours are generated with \pkg{munsell} and
#' `mnsl(c("2.5PB 2/4", "2.5PB 7/10"))`. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams scale_colour_hue
#' @param low,high Colours for low and high ends of the gradient.
#' @param guide Type of legend. Use `"colourbar"` for continuous
#'   colour bar, or `"legend"` for discrete colour legend.
#' @inheritDotParams continuous_scale -na.value -guide -aesthetics
#' @seealso [scales::seq_gradient_pal()] for details on underlying
#'   palette
#' @family colour scales
#' @rdname scale_gradient
#' @export
#' @examples
#' df <- data.frame(
#'   x = runif(100),
#'   y = runif(100),
#'   z1 = rnorm(100),
#'   z2 = abs(rnorm(100))
#' )
#'
#' df_na <- data.frame(
#'   value = seq(1, 20),
#'   x = runif(20),
#'   y = runif(20),
#'   z1 = c(rep(NA, 10), rnorm(10))
#' )
#'
#' # Default colour scale colours from light blue to dark blue
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z2))
#'
#' # For diverging colour scales use gradient2
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z1)) +
#'   scale_colour_gradient2()
#'
#' # Use your own colour scale with gradientn
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z1)) +
#'   scale_colour_gradientn(colours = terrain.colors(10))
#'
#' # Equivalent fill scales do the same job for the fill aesthetic
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradient()
#'
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradientn(colours = terrain.colors(10))
#'
#' # Adjust colour choices with low and high
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z2)) +
#'   scale_colour_gradient(low = "white", high = "black")
#' # Avoid red-green colour contrasts because ~10% of men have difficulty
#' # seeing them
#'
#'# Use `na.value = NA` to hide missing values but keep the original axis range
#' ggplot(df_na, aes(x = value, y)) +
#'   geom_bar(aes(fill = z1), stat = "identity") +
#'   scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
#'
#'  ggplot(df_na, aes(x, y)) +
#'    geom_point(aes(colour = z1)) +
#'    scale_colour_gradient(low = "yellow", high = "red", na.value = NA)
#'
scale_colour_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1], space = "Lab",
                                  na.value = "grey50", guide = "colourbar", aesthetics = "colour") {
  ggplot2::continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_gradient
#' @export
scale_fill_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1], space = "Lab",
                                na.value = "grey50", guide = "colourbar", aesthetics = "fill") {
  ggplot2::continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @inheritParams scales::div_gradient_pal
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'   Defaults to 0.
#' @rdname scale_gradient
#' @export
scale_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                   midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar",
                                   aesthetics = "colour") {
  continuous_scale(aesthetics, "gradient2",
                   div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_gradient
#' @export
scale_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                 midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar",
                                 aesthetics = "fill") {
  continuous_scale(aesthetics, "gradient2",
                   div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}

#' @inheritParams scales::gradient_n_pal
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @rdname scale_gradient
#' @export
scale_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "colour", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
#' @rdname scale_gradient
#' @export
scale_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                 guide = "colourbar", aesthetics = "fill", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
