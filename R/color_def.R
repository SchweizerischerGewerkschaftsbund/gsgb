# Definition SGB-Farben und Farbpaletten
# Autorin: Kristina Schuepbach
# Stand: 14.04.2021

# (1) SGB Standard-Farbpalette (web/sRGB): ----

#' SGB Standard-Farbpalette (10 Farben, web)
#'
#' \code{pal_sgb_web} gibt die Standard-Farbpalette des SGB als data frame mit 10 Farben
#'
#' Das ist die primaere SGB (web/sRGB) Palette.
#'
#' @examples
#' pal_sgb_web
#'
#' # Auswahl nach Position:
#' pal_sgb_web[2]    # 2. Farbname "rot3" (als df)
#' pal_sgb_web[[2]]  # 2. Farbwert "#cb452c"
#'
#' # Auswahl nach Name:
#' pal_sgb_web["rot3"]    # Farbe "rot3" (als df)
#' pal_sgb_web[["rot3"]]  # Farbwert "#cb452c"
#'
#' # Palette plotten:
#' seecol(pal_sgb_web)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_web <- c(
  "rot1" = '#fdc0b0',
  "rot2" = '#eb836a',
  "rot3" = '#cb452c',
  "rot4" = '#990000',
  "weiss" = '#ffffff',
  "grau1" = '#d1d1d1',
  "grau2" = '#a6a6a6',
  "grau3" = '#7c7c7c',
  "grau4" = '#545454',
  "schwarz" = '#000000')[c(4:1, 5:10)] # rot (4 as default) > weiss (5) > grau > schwarz (10)

#  (1) Rot Palette: -----

#' Rote SGB-Farbpalette
#'
#' \code{pal_sgb_rot} ist eine zusaetzliche SGB Farbpalette als data frame mit 5 Farben
#' (Abstufungen von \code{\link{SGBrot}}).
#'
#' @examples
#' pal_sgb_rot
#'
#' # Auswahl nach Position:
#' pal_sgb_rot[4]    # 2. Farbname "rot4" (als df)
#' pal_sgb_rot[[4]]  # 2. Farbwert "#990000"
#'
#' # Auswahl nach Name:
#' pal_sgb_rot["rot4"]    # Farbe "rot4" (als df)
#' pal_sgb_rot[["rot4"]]  # Farbwert "#990000"
#'
#' # Palette plotten:
#' seecol(pal_sgb_rot)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_rot <- c(
  "rot1" = '#fdc0b0',
  "rot2" = '#eb836a',
  "rot3" = '#cb452c',
  "rot4" = '#990000',
  "rot5" = '#660000')

# Rote SGB-Farbe ----

#' Rote SGB-Farbe
#'
#' \code{SGBrot} gibt die bevorzugte Farbe von \code{\link{pal_sgb_rot}}
#' (als atomic HEX character Wert) und ist definiert als \code{\link{pal_sgb_rot}[[4]]}.
#'
#' @examples
#' SGBrot  # HEX character "#990000" (als Wert)
#' all.equal(SGBrot, pal_sgb_rot[[4]])  # TRUE (gleiche HEX Werte)
#'
#' seecol(SGBrot)  # Farbe und Details ansehen
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{pal_sgb_rot}} fuer die entsprechende Farbpalette;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
SGBrot <- pal_sgb_rot[[4]]  # HEX Farbwert von rot4
names(SGBrot) <- "SGBrot"

# Gelb Palette: ----

#' Gelbe SGB-Palette
#'
#' \code{pal_sgb_gelb} ist eine zusaetzliche SGB-Farbpalette als data frame mit 5 Farben
#' (Abstufungen von \code{\link{SGBgelb}}).
#'
#' @examples
#' pal_sgb_gelb
#'
#' # Auswahl nach Position:
#' pal_sgb_gelb[4]    # 2. Farbname "gelb4" (als df)
#' pal_sgb_gelb[[4]]  # 2. Farbwert "#ffce58"
#'
#' # Auswahl nach Name:
#' pal_sgb_gelb["gelb4"]    # Farbe "gelb4" (als df)
#' pal_sgb_gelb[["gelb4"]]  # Farbwert "#ffce58"
#'
#' # Palette plotten:
#' seecol(pal_sgb_gelb)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_gelb <- c(
  "gelb1" = '#fff3d9',
  "gelb2" = '#ffe7b2',
  "gelb3" = '#ffdb88',
  "gelb4" = '#ffce58',
  "gelb5" = '#ffc000')


# Gelb Farbe: ----

#' Gelbe SGB-Farbe
#'
#' \code{SGBgelb} gibt die bevorzugte Farbe von \code{\link{pal_sgb_gelb}}
#' (als atomic HEX character Wert) und ist definiert als \code{\link{pal_sgb_gelb}[[5]]}.
#'
#' @examples
#' SGBgelb  # HEX character "#ffc000" (als Wert)
#' all.equal(SGBgelb, pal_sgb_gelb[[5]])  # TRUE (gleiche HEX Werte)
#'
#' seecol(SGBgelb)  # Farbe und Details ansehen
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{pal_sgb_gelb}} fuer die entsprechende Farbpalette;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
SGBgelb <- pal_sgb_gelb[[5]]  # HEX Farbwert von gelb5
names(SGBgelb) <- "SGBgelb"


# Dunkelblau Palette: -----

#' Dunkelblaue SGB-Palette
#'
#' \code{pal_sgb_dunkelblau} ist eine zusaetzliche SGB-Farbpalette als data frame mit 5 Farben
#' (Abstufungen von \code{\link{SGBdunkelblau}}).
#'
#' @examples
#' pal_sgb_dunkelblau
#'
#' # Auswahl nach Position:
#' pal_sgb_dunkelblau[4]    # 2. Farbname "dunkelblau4" (als df)
#' pal_sgb_dunkelblau[[4]]  # 2. Farbwert "#42467e"
#'
#' # Auswahl nach Name:
#' pal_sgb_dunkelblau["dunkelblau4"]    # Farbe "dunkelblau4" (als df)
#' pal_sgb_dunkelblau[["dunkelblau4"]]  # Farbwert "#42467e"
#'
#' # Palette plotten:
#' seecol(pal_sgb_dunkelblau)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_dunkelblau <- c(
  "dunkelblau1" = '#cecdde',
  "dunkelblau2" = '#9f9dbd',
  "dunkelblau3" = '#71709e',
  "dunkelblau4" = '#42467e',
  "dunkelblau5" = '#002060')


# Dunkelblau Farbe: ----

#' Dunkelblaue SGB-Farbe
#'
#' \code{SGBdunkelblau} gibt die bevorzugte Farbe von \code{\link{pal_sgb_dunkelblau}}
#' (als atomic HEX character Wert) und ist definiert als \code{\link{pal_sgb_dunkelblau}[[5]]}.
#'
#' @examples
#' SGBdunkelblau  # HEX character "#002060" (als Wert)
#' all.equal(SGBdunkelblau, pal_sgb_dunkelblau[[5]])  # TRUE (gleiche HEX Werte)
#'
#' seecol(SGBdunkelblau)  # Farbe und Details ansehen
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{pal_sgb_dunkelblau}} fuer die entsprechende Farbpalette;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
SGBdunkelblau <- pal_sgb_dunkelblau[[5]]  # HEX Farbwert von dunkelblau5
names(SGBdunkelblau) <- "SGBdunkelblau"

# Hellblau Palette: ----

#' Hellblaue SGB-Palette
#'
#' \code{pal_sgb_hellblau} ist eine zusaetzliche SGB-Farbpalette als data frame mit 5 Farben
#' (Abstufungen von \code{\link{SGBhellblau}}).
#'
#' @examples
#' pal_sgb_hellblau
#'
#' # Auswahl nach Position:
#' pal_sgb_hellblau[4]    # 2. Farbname "hellblau4" (als df)
#' pal_sgb_hellblau[[4]]  # 2. Farbwert "#63bae6"
#'
#' # Auswahl nach Name:
#' pal_sgb_hellblau["hellblau4"]    # Farbe "hellblau4" (als df)
#' pal_sgb_hellblau[["hellblau4"]]  # Farbwert "#63bae6"
#'
#' # Palette plotten:
#' seecol(pal_sgb_hellblau)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_hellblau <- c(
  "hellblau1" = '#dcedf9',
  "hellblau2" = '#b8dcf3',
  "hellblau3" = '#91cbed',
  "hellblau4" = '#63bae6',
  "hellblau5" = '#00a9e0')


# Hellblau Farbe ----

#' Hellblaue SGB-Farbe
#'
#' \code{SGBhellblau} gibt die bevorzugte Farbe von \code{\link{pal_sgb_hellblau}}
#' (als atomic HEX character Wert) und ist definiert als \code{\link{pal_sgb_hellblau}[[5]]}.
#'
#' @examples
#' SGBhellblau  # HEX character "#00a9e0" (als Wert)
#' all.equal(SGBhellblau, pal_sgb_hellblau[[5]])  # TRUE (gleiche HEX Werte)
#'
#' seecol(SGBhellblau)  # Farbe und Details ansehen
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{pal_sgb_hellblau}} fuer die entsprechende Farbpalette;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
SGBhellblau <- pal_sgb_hellblau[[5]]  # HEX Farbwert von hellblau5
names(SGBhellblau) <- "SGBhellblau"


# Grau Palette: ----

#' Graue SGB-Palette
#'
#' \code{pal_sgb_grau} ist eine zusaetzliche SGB-Farbpalette als data frame mit 5 Farben
#' (Abstufungen von \code{\link{SGBgrau}}).
#'
#' @examples
#' pal_sgb_grau
#'
#' # Auswahl nach Position:
#' pal_sgb_grau[4]    # 2. Farbname "grau4" (als df)
#' pal_sgb_grau[[4]]  # 2. Farbwert "#545454"
#'
#' # Auswahl nach Name:
#' pal_sgb_grau["grau4"]    # Farbe "grau4" (als df)
#' pal_sgb_grau[["grau4"]]  # Farbwert "#545454"
#'
#' # Palette plotten:
#' seecol(pal_sgb_grau)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_grau <- c(
  "grau1" = '#d1d1d1',
  "grau2" = '#a6a6a6',
  "grau3" = '#7c7c7c',
  "grau4" = '#545454',
  "grau5" = '#303030')

# Grau Farbe: ----

#' Graue SGB-Farbe
#'
#' \code{SGBgrau} gibt die bevorzugte Farbe von \code{\link{pal_sgb_grau}}
#' (als atomic HEX character Wert) und ist definiert als \code{\link{pal_sgb_grau}[[3]]}.
#'
#' @examples
#' SGBgrau  # HEX character "#7c7c7c" (als Wert)
#' all.equal(SGBgrau, pal_sgb_grau[[3]])  # TRUE (gleiche HEX Werte)
#'
#' seecol(SGBgrau)  # Farbe und Details ansehen
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{pal_sgb_grau}} fuer die entsprechende Farbpalette;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
SGBgrau <- pal_sgb_grau[[3]]  # HEX Farbwert von grau4
names(SGBgrau) <- "SGBgrau"


# Ampel Palette -----

#' Ampel SGB-Palette
#'
#' \code{pal_sgb_signal} ist eine zusaetzliche SGB-Farbpalette als data frame mit 3 Farben
#' (Ampel- oder Signalfarben).
#'
#' Die Farben sind wie bei einer Ampel angeordnet:
#' \enumerate{
#' \item top: rot oder "schlecht"
#' \item mit: gelb oder "Achtung"
#' \item unt: gruen oder "gut"
#' }
#'
#' @examples
#' pal_sgb_signal
#'
#' pal_sgb_signal[2]    # (benannte) Farbe "signal2"
#' pal_sgb_signal[[2]]  # Farbe "signal2" oder "#EFDC60"
#'
#' # Palette plotten:
#' seecol(pal_sgb_signal)
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette mit allen 5 Farben von \code{\link{pal_sgb_rot}};
#' \code{\link{pal_sgb_pref}} fuer eine SGB Farbpalette mit allen bevorzugten Farben;
#' \code{\link{seecol}} um die Farbpaletten anzusehen;
#' \code{\link{usecol}} um die Farbpaletten zu verwenden.
#'
#' @export
pal_sgb_signal <- c(
  "signal1" = '#990000',
  "signal2" = '#ffc000',
  "signal3" = '#699903')

# pal_sgb: SGB-Standard-Farbpalette rot/grau ------

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
pal_sgb <- cbind(rev(pal_sgb_rot), pal_sgb_web[5:10])

# pal_sgb_pref: SGB-Standard-Farbpalette alle Farben ------

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
pal_sgb_pref <- data.frame(
  "SGBrot"        = SGBrot,
  "SGBhellblau"   = SGBhellblau,
  "SGBdunkelblau" = SGBdunkelblau,
  "SGBgelb"       = SGBgelb,
  "SGBgrau"       = SGBgrau,
  stringsAsFactors = FALSE)


# pal_sgb_pair: Palette mit 10 gepaarten Farben (in 5 Paaren): ------

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
pal_sgb_pair <- data.frame(
  pal_sgb_rot[c(4, 2)],
  pal_sgb_hellblau[c(5, 3)],
  pal_sgb_dunkelblau[c(5, 3)],
  pal_sgb_gelb[c(5, 3)],
  pal_sgb_grau[c(5, 3)],
  stringsAsFactors = FALSE)



## Lookup list of palettes: ----------
all_palsgb_basic <- c("pal_sgb", "pal_sgb_web")
all_palsgb_pair <- c("pal_sgb_pair")
all_palsgb_grad <- c("pal_sgb_rot", "pal_sgb_gelb", "pal_sgb_dunkelblau", "pal_sgb_hellblau", "pal_sgb_grau")
all_palsgb_pref <- c("pal_sgb_pref", all_palsgb_grad)
all_palsgb <- c(all_palsgb_basic, all_palsgb_pair, all_palsgb_pref)


