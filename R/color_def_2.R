## color_def_2.R | gsgb
## SGB | 08.04.2020
## ---------------------------

## SGB-Farben und Farbpaletten definieren (2 von 2).

# (C) Andere Kombinationen der SGB-Farbpaletten: --------

#  (1) pal_sgb: Kombination von pal_sgb_rot und pal_sgb_web: ------

#' SGB Standard-Farbpalette (11 colors).
#'
#' \code{pal_sgb} kombiniert die 5 roten Farben der
#' Farbpalette \code{\link{pal_sgb_rot}}
#' mit den 6 nicht-roten Farben von \code{\link{pal_sgb}}
#' zu einer Palette mit 11 Farbwerten. Damit ist \code{weiss}
#' in der Mitte der Palette. Das ist hilfreich, wenn Gradienten
#' verwendet werden.
#'
#' @examples
#' pal_sgb
#'
#' # Auswahl nach Position
#' pal_sgb[1]    # Farbname "rot5" (as df)
#' pal_sgb[[1]]  # Farbwert "#660000"
#'
#' # Auswahl nach Name
#' pal_sgb["rot5"]   # Farbname "rot5" (as df)
#' pal_sgb[["rot5"]] # Farbwert "#660000"
#'
#' # Palette plotten:
#' seecol(pal_sgb)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_rot}} fuer die rote SGB-Farbpalette;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb <- c(rev(pal_sgb_rot), pal_sgb_web[5:10])



#  (2) pal_sgb_pref: Palette mit allen bevorzugten Farben: ------

#' Bevorzugte SGB-Farben in einer Farbpalette
#'
#' \code{pal_sgb_pref} ist eine zusaetzliche SGB-Farbpalette
#' mit allen bevorzugten SGB-Farben (rot, hellblau, dunkelblau, gelb, grau).
#'
#' @examples
#' pal_sgb_pref
#'
#' # Palette plotten:
#' seecol(pal_sgb_pref)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export

pal_sgb_pref <- c(
  "SGBrot"        = SGBrot,
  "SGBhellblau"   = SGBhellblau,
  "SGBdunkelblau" = SGBdunkelblau,
  "SGBgelb"       = SGBgelb,
  "SGBgrau"       = SGBgrau)


#  (3) pal_sgb_pair: Palette mit 10 gepaarten Farben (in 5 Paaren): ------

#' Paarweise SGB-Farben in einer Farbpalette
#'
#' \code{pal_sgb_pref} ist eine zusaetzliche SGB-Farbpalette, welche die
#' 5 SGB-Farben (rot, hellblau, dunkelblau, gelb, grau) in 10 Paaren zusammenstellt.
#'
#' @examples
#' pal_sgb_pair
#'
#' # Palette plotten:
#' seecol(pal_sgb_pair)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_rot}} fuer die rote SGB-Farbpalette;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export

pal_sgb_pair <- c(
  pal_sgb_rot[c(4, 2)],
  pal_sgb_hellblau[c(5, 3)],
  pal_sgb_dunkelblau[c(5, 3)],
  pal_sgb_gelb[c(5, 3)],
  pal_sgb_grau[c(5, 3)])



## Lookup list of palettes: ----------

all_palsgb_basic <- c("pal_sgb", "pal_sgb_web")
# all_palsgb_basic <- c("pal_sgb", "pal_sgb_web", "pal_sgb_ppt") # ToDo: add pal_sgb_ppt
# all_palsgb_pair <- c("pal_sgb_light", "pal_sgb_dark", "pal_sgb_pair") # ToDo: erstellen wenn wir mehr Farben haben
all_palsgb_pair <- c("pal_sgb_pair")
all_palsgb_grad <- c("pal_sgb_rot", "pal_sgb_gelb", "pal_sgb_dunkelblau", "pal_sgb_hellblau", "pal_sgb_grau")
all_palsgb_pref <- c("pal_sgb_pref", all_palsgb_grad)
all_palsgb <- c(all_palsgb_basic, all_palsgb_pair, all_palsgb_pref)


