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
#' @family colour scales
#' @rdname scale_colour_discrete
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gsgb)
#' theme_set(theme_sgb(base_size = 9))
#'
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
#'}
#'
#' @export
scale_colour_discrete <-  function(...) {
  ggplot2::scale_colour_manual(values = usecol(pal = pal_sgb_pref))
}

#' @rdname scale_colour_discrete
#' @export
#' @usage NULL
scale_color_discrete <- scale_colour_discrete

#' Scale fill discrete with SGB-colors
#'
#' Overwrites the default discrete fill scale with SGB-palette \code{pal_sgb_pref}. To use
#' another palette or custom colours, use \code{scale_fill_manual()}.
#'
#' @param ... parameters passed on to \code{scale_fill_manual()}.
#'
#' @family colour scales
#' @rdname scale_fill_discrete
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gsgb)
#' theme_set(theme_sgb(base_size = 9))
#'
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
#'}
#'
#' @export
scale_fill_discrete <- function(...) {
  ggplot2::scale_fill_manual(values = usecol(pal = pal_sgb_pref))
}

# Continuous fill/colour scales ----------------------------------

#' Scale fill continuous with SGB-colors
#'
#' Overwrites the default continuous fill scale with SGB-colors. To use
#' another palette or custom colours, use \code{scale_fill_gradient()}.
#'
#' @param ... parameters passed on to \code{scale_fill_gradient()}.
#'
#' @family colour scales
#' @name scale_fill_gradient
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gsgb)
#' theme_set(theme_sgb(base_size = 9))
#'
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
#' }
#'
NULL

#' @rdname scale_fill_gradient
#' @param low,high Colours for low and high end of the gradient
#' @export
scale_fill_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1]) {
  ggplot2::scale_fill_gradient(..., low = low, high = high)
}

#' @rdname scale_fill_gradient
#' @param mid Colour for mid point
#' @export
scale_fill_gradient2 <- function(..., low = pal_sgb_dunkelblau[5],
                                 mid = pal_sgb_hellblau[2],
                                 high = pal_sgb_rot[5]) {
  ggplot2::scale_fill_gradient2(..., low = low, mid = mid, high = high)
}

#' @rdname scale_fill_gradient
#' @param colors,colours Vector of colours to use for n-colour gradient.
#' @export
scale_fill_gradientn <- function(..., colors, colours) {
  if (missing(colours) & missing(colors)) colours <- pal_sgb
  colours <- if (missing(colours)) colors  else colours
  ggplot2::scale_fill_gradientn(..., colours = colours)
}

#' @export
#' @rdname scale_fill_gradient
#' @usage NULL
scale_fill_continuous <- scale_fill_gradient

#' Scale color continuous with SGB-colors
#'
#' Overwrites the default continuous color scale with SGB-colors. To use
#' another palette or custom colours, use \code{scale_color_gradient()}.
#'
#' @param ... parameters passed on to \code{scale_color_gradient()}.
#'
#' @family colour scales
#' @name scale_colour_gradient
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gsgb)
#' theme_set(theme_sgb(base_size = 9))
#'
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
#' }
#'
NULL

#' @rdname scale_colour_gradient
#' @param low,high Colours for low and high end of the gradient
#' @export
scale_colour_gradient <- function(..., low = pal_sgb_rot[5], high = pal_sgb_rot[1]) {
  ggplot2::scale_colour_gradient(..., low = low, high = high)
}

#' @rdname scale_colour_gradient
#' @param mid Colour for mid point
#' @export
scale_colour_gradient2 <- function(..., low = pal_sgb_dunkelblau[5],
                                  mid = pal_sgb_hellblau[2],
                                  high = pal_sgb_rot[5]) {
  ggplot2::scale_colour_gradient2(..., low = low, mid = mid, high = high)
}

#' @rdname scale_colour_gradient
#' @param colors,colours Vector of colours to use for n-colour gradient.
#' @export
scale_colour_gradientn <- function(..., colours = pal_sgb, colors = pal_sgb) {
  if (missing(colours) & missing(colors)) colours <- pal_sgb
  colours <- if (missing(colours)) colors  else colours
  ggplot2::scale_color_gradientn(..., colours = colours)
}

#' @rdname scale_colour_gradient
#' @export
#' @usage NULL
scale_colour_continuous <- scale_colour_gradient

#' @export
#' @rdname scale_colour_gradient
#' @usage NULL
scale_color_gradient <- scale_colour_gradient

#' @export
#' @rdname scale_colour_gradient
#' @usage NULL
scale_color_continuous <- scale_colour_continuous

#' @export
#' @rdname scale_colour_gradient
#' @usage NULL
scale_color_gradient2 <- scale_colour_gradient2

#' @export
#' @rdname scale_colour_gradient
#' @usage NULL
scale_color_gradientn <- scale_colour_gradientn
