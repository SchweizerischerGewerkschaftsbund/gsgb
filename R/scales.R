## scale-Functions.R  |  gsgb
## SGB | 25.03.2021 | Kristina Schuepbach
## ---------------------------

# Idee: scale_color_* und scale_fill_* von ggplot ueberschreiben, damit
# default-maessig die sgb-Farben verwendet werden.

# Von hadley (https://ggplot2-book.org/scale-colour.html):
# The default scale for continuous fill scales is scale_fill_continuous()
# which in turn defaults to scale_fill_gradient().
# scale_fill_gradient() produces a two-colour gradient
# scale_fill_gradient2() produces a three-colour gradient with specified midpoint
# scale_fill_gradientn() produces an n-colour gradient

# The default scale for discrete colours is scale_fill_discrete() which in turn defaults to
# scale_fill_hue().

#' # Discrete fill/colour scales -----------------------------------

#' Scale colour discrete with SGB-colors
#'
#' Overwrites the default discrete colour scale with SGB-palette \code{pal_sgb_pref}. To use
#' another palette or custom colours, use \code{scale_color_manual()}.
#'
#' @param ... parameters passed on to \code{scale_color_manual()}.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 2) +
#'   labs(x = "", y = "")
#'
#' # This gives the same:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 2) +
#'   labs(x = "", y = "") +
#'   scale_fill_discrete()
#'
#' # To change color scale:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 2) +
#'   labs(x = "", y = "") +
#'   scale_color_manual(values = rev(usecol(pal_sgb_rot)))
#'
#' @export
scale_colour_discrete <-  function(...) {
  ggplot2::scale_colour_manual(values = usecol(pal = pal_sgb_pref))
}
scale_color_discrete <- function(...) {
  ggplot2::scale_color_manual(values = usecol(pal = pal_sgb_pref))
}

#' Scale fill discrete with SGB-colors
#'
#' Overwrites the default discrete fill scale with SGB-palette \code{pal_sgb_pref}. To use
#' another palette or custom colours, use \code{scale_fill_manual()}.
#'
#' @param ... parameters passed on to \code{scale_fill_manual()}.
#'
#' @examples
#' ggplot(mpg, aes(as.factor(year), fill = drv)) +
#'   geom_bar(position = "dodge") +
#'     labs(x = "", y = "")
#'
#' # This gives the same:
#' ggplot(mpg, aes(as.factor(year), fill = drv)) +
#'   geom_bar(position = "dodge") +
#'     labs(x = "", y = "") +
#'   scale_fill_discrete()
#'
#' # To change fill scale:
#' ggplot(mpg, aes(as.factor(year), fill = drv)) +
#'   geom_bar(position = "dodge") +
#'     labs(x = "", y = "") +
#'  scale_fill_manual(values = rev(usecol(pal_sgb_dunkelblau)))
#'
#' @export
scale_fill_discrete <- function(...) {
  ggplot2::scale_fill_manual(values = usecol(pal = pal_sgb_pref))
}


# This could be another option to override palette defaults:
# options(ggplot2.discrete.fill = function() scale_fill_manual(values = usecol(pal = pal_sgb)))
# options(ggplot2.discrete.colour = function() scale_fill_manual(values = usecol(pal = pal_sgb)))

# Continuous fill/colour scales ----------------------------------

#' Scale fill continuous with SGB-colors
#'
#' Overwrites the default continuous fill scale with SGB-colors. To use
#' another palette or custom colours, use \code{scale_fill_gradient()}.
#'
#' @param ... parameters passed on to \code{scale_fill_gradient()}.
#'
#' @examples
#' @export
scale_fill_continuous <- function(...) {
  ggplot2::scale_fill_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1])
}
scale_fill_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1]) {
  ggplot2::scale_fill_gradient(..., low = low, high = high)
}
scale_fill_gradient2 <- function(..., low = pal_sgb_dunkelblau[5],
                                 mid = pal_sgb_hellblau[2],
                                 high = pal_sgb_rot[5]) {
  ggplot2::scale_fill_gradient2(..., low = low, mid = mid, high = high)
}
scale_fill_gradientn <- function(..., colors = pal_sgb, colours = pal_sgb) {
  ggplot2::scale_fill_gradientn(..., colors = colors, colours = pal_sgb)
}


#' Scale color continuous with SGB-colors
#'
#' Overwrites the default continuous color scale with SGB-colors. To use
#' another palette or custom colours, use \code{scale_fill_gradient()}.
#'
#' @param ... parameters passed on to \code{scale_fill_gradient()}.
#'
#' @examples
#' @export
scale_color_continuous <- function(...) {
  ggplot2::scale_color_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1])
}
scale_colour_continuous <- function(...) {
  ggplot2::scale_colour_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1])
}

scale_color_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1]) {
  ggplot2::scale_color_gradient(..., low = low, high = high)
}
scale_colour_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1]) {
  ggplot2::scale_colour_gradient(..., low = low, high = high)
}

scale_color_gradient2 <- function(..., low = pal_sgb_dunkelblau[5],
                                 mid = pal_sgb_hellblau[2],
                                 high = pal_sgb_rot[5]) {
  ggplot2::scale_color_gradient2(..., low = low, mid = mid, high = high)
}
scale_colour_gradient2 <- function(..., low = pal_sgb_dunkelblau[5],
                                  mid = pal_sgb_hellblau[2],
                                  high = pal_sgb_rot[5]) {
  ggplot2::scale_colour_gradient2(..., low = low, mid = mid, high = high)
}

scale_color_gradientn <- function(..., colors = pal_sgb, colours = pal_sgb) {
  ggplot2::scale_color_gradientn(..., colors = colors, colours = pal_sgb)
}
scale_colour_gradientn <- function(..., colors = pal_sgb, colours = pal_sgb) {
  ggplot2::scale_colour_gradientn(..., colors = colors, colours = pal_sgb)
}

# To Do:
# - mit beispielen ergaenzen
# - testen (scale_fill_gradient* ist plus minus schon, scale_colour_gradient* noch nicht)
# - Tests schreiben fuer alle Funktionen in diesem Script
