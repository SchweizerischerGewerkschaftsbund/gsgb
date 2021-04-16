# ggplot-theme SGB
# Autorin: Kristina Schuepbach
# Datum: 14.04.2021

# Description -------------------------------
# based on ggplot theme_gray and theme_classic
# https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r


#' Standard SGB-Themes fuer ggplot2
#'
#' Das Theme basiert auf dem ggplot2 theme_gray und theme_classic. Je nach verwendeter Grafik
#' muessen einzelne Elemente selber noch angepasst werden (z.B. Position der Legende oder
#' Entfernen der y-Achsenbeschriftung).
#' Zuesaetzlich wird auch ein minimalistisches Theme definiert, etwa fuer Kuchendiagramme.
#'
#' @param base_size Standard Schriftgroesse (optional, numeric).
#' Default: \code{base_size = 8}.
#'
#' @param base_family Standard Schriftart (optional, character).
#' Default: \code{base_family = ""}.
#'
#' @param base_line_size Standard Liniengroesse (optional, numeric).
#' Default: \code{base_line_size = base_size/22}.
#'
#' @param base_rect_size Standard Groesse rechteckige Elemente (optional, numeric).
#' Default: \code{base_rect_size = base_size/22}.
#'
#' @details
#' \describe{
#'
#' \item{`theme_sgb`}{
#' Das Standard SGB-Theme, mit weissem Hintergrund, schwarzer x- und y-Achse, ohne
#' Rahmen um den Plot und grauen Rasterlinien.}
#'
#' \item{`theme_sgb_blank`}{
#' Minimalistisches Theme basierend auf theme_sgb. Ohne Achsen, Raster, Beschriftungen etc.
#' Geeignet etwa fuer Kuchendiagramme.
#' }
#' }
#'
#' @examples
#'
#' \donttest{
#'   # Plotten mit iris Daten (mit ggplot2, theme_sgb, and SGB Farben):
#'
#'   library("ggplot2")  # theme_sgb benoetigt ggplot2
#'
#'   ggplot(datasets::iris) +
#'      geom_point(aes(Sepal.Width, Sepal.Length, color = Species),
#'        size = 2) +
#'      scale_color_manual(values = usecol(pal = pal_sgb_pref, n = 3)) +
#'      theme_sgb(base_size = 10) +
#'      theme(axis.title.y = element_blank()) # manually remove y-Axis title
#' }
#'
#' @family plot functions
#'
#' @import ggplot2
#'
#' @name ggtheme
#' @aliases NULL
NULL

#' @export
#' @rdname ggtheme
theme_sgb <- function(base_size = 8, base_family = "",
                      base_line_size = base_size / 22,
                      base_rect_size = base_size / 22) {
  half_line <- base_size / 2

  # Starts with theme_grey and then modify some parts
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      # axis text
      axis.text =          ggplot2::element_text(size = ggplot2::rel(1), colour = "black"), # axis text color and size
      axis.ticks =         ggplot2::element_line(colour = "black"), # axis ticks color
      axis.title.x =       ggplot2::element_text(margin = margin(t = half_line),
                                                 vjust = 1, size = ggplot2::rel(1)), # x-axis title size
      axis.title.y =       ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line),
                                                 vjust = 1, size = ggplot2::rel(1)), # y-axis title size
      # axis lines (black)
      axis.line  =         ggplot2::element_line(colour = "black", size = ggplot2::rel(0.8)),

      # legend
      legend.title =       ggplot2::element_blank(),  # no legend title
      legend.key =         ggplot2::element_rect(fill = "white", colour = NA), # match legend key to background
      legend.text =        ggplot2::element_text(size = ggplot2::rel(1)), # legend text size
      legend.position =    "top", # legend at the bottom of graph
      # -> this often has to be changed manually
      legend.direction =   "horizontal",
      legend.justification = c("center", "top"),
      legend.margin =      ggplot2::margin(0, 0, 0, 0, "cm"), # smaller margin btw legend text and plot

      # Titles if you have several plots combined (facet labels)
      strip.background  = element_blank(),
      strip.text =         ggplot2::element_text(colour = "black", size = ggplot2::rel(1), face = "bold",
                                                 margin = ggplot2::margin(half_line, half_line, half_line, half_line)),

      # Background and grid
      panel.background =   ggplot2::element_rect(fill = "white", colour = NA), # white background
      panel.border =       ggplot2::element_blank(), # panel border
      panel.grid.major =   ggplot2::element_line(colour = "grey80"), # grey gridlines
      panel.grid.minor =   ggplot2::element_blank(), # only major grid
      plot.margin =        ggplot2::margin(half_line, half_line, half_line, half_line), # only small margins around plot

      # Plot Title
      plot.title.position =  'plot',
      plot.title =         ggplot2::element_text(
        size =             ggplot2::rel(1.2),
        hjust =            0,
        vjust =            1,
        margin =           ggplot2::margin(b = half_line)
      ),
      plot.subtitle =      ggplot2::element_text(
        size =             ggplot2::rel(1),
        hjust =            0,
        vjust =            1,
        margin =           ggplot2::margin(b = half_line)
      ),
      plot.caption.position = "plot",
      plot.caption =       ggplot2::element_text(
        size =             ggplot2::rel(1),
        hjust =            0, # left-align
        vjust =            1,
        margin =           ggplot2::margin(t = half_line*2)
      ),

      complete = TRUE
    )
}


#' @export
#' @rdname ggtheme
theme_sgb_blank <- function(base_size = 9, base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  # Starts with theme_sgb and remove most parts
  theme_sgb(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      line =              element_blank(),
      rect =              element_blank(),
      axis.ticks =        element_blank(),
      axis.title =        element_blank(),
      axis.text  =        element_blank(),
      legend.background = element_blank(),
      legend.key =        element_blank(),
      legend.box =        NULL,
      panel.background  = element_blank(),
      panel.border =      element_blank(),
      panel.grid   =      element_blank(),
      strip.background =  element_blank(),
      plot.background =   element_blank(),
      complete = TRUE
    )
}
