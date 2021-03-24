# Wrapper function for ggsave
# Autor: Samuel Meier
# Datum: 04.03.2

# Packages ------------------------------------


library(ggplot2)

# Beispieldaten laden -----------------------------


# Wrapper for ggsave ---------

#' Wrapper for ggsave
#'
#' This function wraps around the ggsave function of gglpot2 (\link[ggplot2]{ggsave}) by setting the defaults to the SGB standard output.
#'
#' @param filename see documentation of \link[ggplot2]{ggsave}
#' @param plot see documentation of \link[ggplot2]{ggsave}
#' @param device see documentation of \link[ggplot2]{ggsave}
#' @param path see documentation of \link[ggplot2]{ggsave}
#' @param scale see documentation of \link[ggplot2]{ggsave}
#' @param width see documentation of \link[ggplot2]{ggsave}
#' @param height see documentation of \link[ggplot2]{ggsave}
#' @param units see documentation of \link[ggplot2]{ggsave}
#' @param dpi see documentation of \link[ggplot2]{ggsave}
#' @param limitsize see documentation of \link[ggplot2]{ggsave}
#' @param ... see documentation of \link[ggplot2]{ggsave}
#'
#' @return see documentation of \link[ggplot2]{ggsave}
#' @export
#'
#' @examples
#' ggsave("my_sgb_plot.png")
ggsave <- function(filename,
                   plot = last_plot(),
                   device = NULL,
                   path = NULL,
                   scale = 1,
                   width = 3.5,
                   height = 2.5,
                   units = "in",
                   dpi = "retina",
                   limitsize = TRUE,
                   ...) {
  ggplot2::ggsave(filename = filename,
                  plot = plot,
                  device = device,
                  path = path,
                  scale = scale,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi,
                  limitsize = limitsize,
                  ... = ...)
}


