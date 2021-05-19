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

#' Scale color discrete with SGB-colors
#'
#' Overwrites the default discrete color scale with SGB-palette \code{pal_sgb_pref}. To use
#' another palette or custom colors, use \code{scale_color_manual()}.
#'
#' @param ... parameters passed on to \code{scale_color_manual()}.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point()
#'
#' # This gives the same:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_discrete()
#'
#' # To change color scale:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
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
#' @export
#'
#' @examples
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density))
#'
#' # This gives the same:
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradient()
#'
#' # Custom gradient colors:
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradient(low = pal_sgb_dunkelblau[5], high = pal_sgb_gelb[5])
#'
#' # 3-color gradient (default):
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradient2(midpoint = .02)
#'
#' # 3-color gradient (custom):
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradient2(low = pal_sgb_rot[5], mid = "white", high = pal_sgb_gelb[5],
#'                        midpoint = .02)
#'
#' # n-colour gradient (default):
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradientn()
#'
#' # n-colour gradient (custom):
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density)) +
#'   scale_fill_gradientn(colors = pal_sgb_pref)
#'
#' # Use `na.value = NA` to hide missing values but keep the original axis range
#' df_na <- data.frame(
#'   value = seq(1, 20),
#'   x = runif(20),
#'   y = runif(20),
#'   z1 = c(rep(NA, 10), rnorm(10)))
#' ggplot(df_na, aes(x = value, y)) +
#'   geom_bar(aes(fill = z1), stat = "identity") +
#'   scale_fill_gradient(na.value = NA)
#'
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
scale_fill_gradientn <- function(..., colors, colours) {
  if (missing(colours) & missing(colors)) colours <- pal_sgb
  colours <- if (missing(colours)) colors  else colours
  ggplot2::scale_fill_gradientn(..., colours = colours)
}


#' Scale color continuous with SGB-colors
#'
#' Overwrites the default continuous color scale with SGB-colors. To use
#' another palette or custom colours, use \code{scale_color_gradient()}.
#'
#' @param ... parameters passed on to \code{scale_color_gradient()}.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length))
#'
#' # This gives the same:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradient()
#'
#' # Custom gradient colors:
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradient(low = pal_sgb_dunkelblau[5], high = pal_sgb_gelb[5])
#'
#' # 3-color gradient (default):
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradient2(midpoint = 6)
#'
#' # 3-color gradient (custom):
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradient2(low = pal_sgb_rot[5], mid = "white", high = pal_sgb_gelb[5],
#'                         midpoint = 6)
#'
#' # n-color gradient (default):
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradientn()
#'
#' # n-color gradient (custom):
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(aes(color = Sepal.Length)) +
#'   scale_color_gradientn(colors = pal_sgb_pref)
#'
#' # Use `na.value = NA` to hide missing values but keep the original axis range
#' df_na <- data.frame(
#'   value = seq(1, 20),
#'   x = runif(20),
#'   y = runif(20),
#'   z1 = c(rep(NA, 10), rnorm(10)))
#'
#' ggplot(df_na, aes(x = value, y)) +
#'   geom_point(aes(color = z1)) +
#'   scale_color_gradient(na.value = NA)
#'
#' # or select custom color for missing values
#' ggplot(df_na, aes(x = value, y)) +
#'   geom_point(aes(color = z1)) +
#'   scale_color_gradient(na.value = SGBdunkelblau)
#'
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

scale_color_gradientn <- function(..., colours, colors) {
  if (missing(colours) & missing(colors)) colours <- pal_sgb
  colours <- if (missing(colours)) colors  else colours
  ggplot2::scale_color_gradientn(..., colours = colours)
}
scale_colour_gradientn <- function(..., colours = pal_sgb, colors = pal_sgb) {
  if (missing(colours) & missing(colors)) colours <- pal_sgb
  colours <- if (missing(colours)) colors  else colours
  ggplot2::scale_color_gradientn(..., colours = colours)
}

# To Do:
# - testen (scale_fill_gradient* ist plus minus schon, scale_colour_gradient* noch nicht)
# - Tests schreiben fuer alle Funktionen in diesem Script
