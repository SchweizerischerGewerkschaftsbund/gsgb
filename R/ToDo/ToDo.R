# Uebersicht
#
# Done
# color_def_1.R
#     Farben angepasst, dokumentiert.
#     Wird aber idealerweise noch durch Grafikerin definierte Farben ersetzt und erweitert.
# color_def_2.R
#     pal_sgb_light und pal_sgb_dark fehlt - braucht dazu aber mehr Farben
# color_fun.R
# color_util.R
# plot_themes.R
#     SGB-Theme implementiert
# plot_examples.R
#     mehrere Beispiele fuer helpfile
# data.R
#     Beispieldaten


# --> NEXT
# - Theme als Default setzen
# - scale-Funktionen von ggplot ueberschreiben (s. unten) -> bisher nur Notloesung
# - Fehler mit facet_grid
# - ausserdem auch start_gsgb.R nochmal anschauen


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

