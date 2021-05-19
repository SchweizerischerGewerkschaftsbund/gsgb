## color_fun.R  |  gsgb
## SGB | 08.04.2020
## ---------------------------

## Definiere Farb-Funktionen
## (z.B. Paletten auswaehlen, plotten, generieren)


## usecol: Benutze eine Farbpalette (einge gegebene): ---------

#' Benutze eine Farbpalette
#'
#' \code{usecol} erlaubt die Verwendung einer Farbpalette \code{pal} (z.B. zum plotten).
#'
#' @param pal Eine Farbpalette (als Vektor mit Farben oder eine Farbpalette).
#' Default: \code{pal = \link{pal_sgb}}.
#'
#' @param n Ein ganzzahliger Wert, der die gewuenschte Anzahl von Farben aus der Palette angibt.
#' Fuer alle Paletten, die in \code{gsgb} definiert sind, verwendet es standardmaessig eine vordefinierte
#' Auswahl von Farben, wenn die gewuenschte Anzahl von Farben kleiner als die verfuegbare Anzahl ist.
#' Fuer alle anderen Paletten und \code{n} groesser als \code{length(pal)} erweitert es die Palette mit
#' \code{\link{colorRampPalette}}.
#'
#' @param alpha Ein Faktor, der die Deckkraft (opacity alpha) modifiziert
#' (wie in \code{\link{adjustcolor}}); typischerweise in [0,1].
#' Default: \code{NA} (d.h. keine aenderung der Deckkraft).
#'
#' @param use_names Ein logischer Wert der definiert, ob Farben als benannter Vektor zurueckgegeben werden
#' sollen. (Default ist \code{FALSE}, zur Kompatibilitaet mit \code{ggplot}).
#'
#' @param use_col_ramp Ein logischer Wert, der angibt, ob der Default der vorausgewaehlten Farben
#' ueberschrieben werden soll und stattdessen \code{\link{colorRampPalette}} zur Verarbeitung von
#' \code{n} verwendet werden soll.
#'
#' @examples
#' usecol(pal = pal_sgb, n = "all")  # default Farbpalette
#' usecol(pal = pal_sgb, n =  4)     # Auswahl von n vorausgewaehlten Farben
#' usecol(pal = pal_sgb, n = 20)     # Farbpalette erweitern
#'
#' # Neue Farbpalette mischen:
#' pal_1 <- usecol(pal = c(rev(pal_sgb_rot), "white", pal_sgb_gelb))
#' seecol(pal_1)
#'
#' # Farbpalette mischen und erweitern:
#' pal_2 <- usecol(pal = c(rev(pal_sgb_dunkelblau), "white", pal_sgb_hellblau), n = 20)
#' seecol(pal_2)
#'
#' # Eine eigene Farbpalette definieren und verwenden:
#' pal_ahv13 <- c("#E94C25", "white", "black")
#' names(pal_ahv13) <- c("orange_ahv", "white", "black")
#'
#' pal_3 <- usecol(pal_ahv13, n = 7)
#' seecol(pal_3)
#'
#' @family Farbfunktionen
#'
#' @seealso
#' \code{\link{seecol}} um Farbpaletten zu plotten;
#' \code{\link{pal_sgb}} fuer die SGB Standard-Farbpalette
#'
#' @export
usecol <- function(pal = pal_sgb,
                   n = "all",
                   alpha = NA,  # regulate transparency.
                   use_names = FALSE,  # should colors be returned as a named vector?
                   use_col_ramp = FALSE) {

  ## Parse the input:
  parenv <- parent.frame()
  parse_pal(pal = pal)
  pal_inp <- tryCatch(suppressWarnings(parse_pal(pal = pal)),  # suppress any warnings in parsing.
                      error = function(e) {
                        # seecol always triggers this part
                        pal <- deparse(substitute(expr = pal, env = parenv))

                        ## Remove slashes and quotes:
                        pal <- gsub("\\\\", "", pal)
                        pal <- gsub("\"", "", pal)

                        ## Reparse with new input:
                        parse_pal(pal = pal)
                      }
  )

  ## Set n to length pal_inp, if n == "all": -----
  if (n == "all")  n <- length(pal_inp)

  pal_def <- FALSE  # assume default, that an undefined palette is used.
  # Check this in the next step (this variable serves to control flow).

  if (all(unlist(lapply(all_palsgb, exists)))) {  # test whether the palettes in all_palsgb are defined.
    # Test whether equal to any palette:
    all_pals1 <- lapply(all_palsgb, get)  # get all palettes from the first part.
  } else {  # if not all palettes are defined:
    all_pals1 <- NA
  }

  ## Is the input one of the defined palettes?
  if (!use_col_ramp) {
    # execute, if not always the colorRamp should be used.
    pal_ix <-
      sapply(all_pals1, function(x) { return(isTRUE(all.equal(pal_inp, unlist(x)))) }
      )  # Test, whether specified palette is there.

    ## If none fits, test for reversed palettes:
    rev_pal <- FALSE  # should the palette be reversed?
    if (!any(pal_ix)) {
      pal_ix <-
        sapply(all_pals1, function(x)
          isTRUE(all.equal(rev(pal_inp), x)))
      if (any(pal_ix)) {
        rev_pal <- TRUE
      }  # if palette is reversed, set pal_rev to TRUE.
    }

    # If input fits with any palette:
    if (any(pal_ix) & length(pal_inp) >= n) {
      pal_name <- all_palsgb[pal_ix]  # get name of the palette.
      pal <- pal_inp  # redefine.
      # Define sets of palettes:
      set1 <- pal_name %in% c("pal_sgb_grau",
                              "pal_sgb_gelb",
                              "pal_sgb_dunkelblau",
                              "pal_sgb_hellblau")
      set2 <- pal_name %in% c("pal_sgb_rot")
      set3 <- pal_name %in% c("pal_sgb_web")
      # set3 <- pal_name %in% c("pal_sgb_web", "pal_sgb_ppt") # ToDo: add pal_sgb_ppt
      set4 <- pal_name %in% "pal_sgb"
      set5 <- pal_name %in% c("pal_sgb_pair", "pal_sgb_dark", "pal_sgb_light", "pal_sgb_pref")
      set6 <- pal_name %in% "pal_signal"
      pal_set <- which(c(set1, set2, set3, set4, set5, set6))  # define a set number.

      # Determine the color output:
      out_col <- switch(pal_set,
                        ## Get the indices for pal_set:
                        # Set1 (grau, gelb, dunkelblau, hellblau): -----
                        switch(n,
                          pal[5],
                          pal[c(5, 3)],
                          pal[c(5, 3, 1)],
                          pal[c(5, 4, 3, 2)],
                          pal),
                        # Set2 (rot): -----
                        switch(n,
                          pal[4],
                          pal[c(4, 2)],
                          pal[c(4, 3, 2)],
                          pal[c(5, 4, 3, 2)],
                          pal),
                        # Set3: -----
                        switch(n,
                          pal[1],
                          pal[c(1, 3)],
                          pal[c(1, 2, 3)],
                          pal[c(1, 3, 6, 9)],
                          pal[c(1, 3, 5, 7, 9)],
                          pal[c(1, 3, 4, 7, 9, 10)],
                          pal[c(1, 3, 4, 5, 7, 9, 10)],
                          pal[c(1, 2, 3, 4, 6, 7, 9, 10)],
                          pal[c(1, 2, 3, 4, 5, 6, 7, 9, 10)],
                          pal),
                        # Set4: -----
                        switch(n,
                          pal[2],
                          pal[c(2, 4)],
                          pal[c(2, 3, 4)],
                          pal[c(2, 4, 7, 10)],
                          pal[c(2, 4, 6, 8, 10)],
                          pal[c(1, 2, 4, 7, 9, 11)],
                          pal[c(1, 2, 4, 6, 7, 9, 11)],
                          pal[c(1, 2, 3, 4, 7, 9, 10, 11)],
                          pal[c(1, 2, 3, 4, 6, 7, 9, 10, 11)],
                          pal[c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)],
                          pal  # all 11 colors of pal_sgb
                        ),
                        # Set 5: -----
                        pal[1:n],
                        # Set 6: -----
                        pal[c("signal1", "signal3", "signal2")[1:n]]
      )

      if (rev_pal) {
        out_col <-
          rev(out_col)
      }  # if palette was reversed, reverse result as well.
      pal_def <- TRUE  # set flag that palette is defined.
    }
  }

  ## If no defined palette is used or the number exceeds the number of colors simply use colorRamp:
  if (!pal_def) {
    ## Decide, whether to use colorRamp or not:
    if (n == length(pal_inp)) {
      out_col <- pal_inp
    } else {
      out_col <-
        grDevices::colorRampPalette(pal_inp)(n)  # use the colorRamp (this swallows all names).
    }
  }

  ## Give the palette a name (as comment attribute):
  comment(out_col) <- ifelse(pal_def, pal_name, "custom")

  ## Do a quick name search if no names are given:
  if (all(is.null(names(out_col)))) {
    tst <- out_col
    # Names from defined sgb palettes:
    sgb_names <-  names(unlist(all_pals1))[match(tst, unlist(all_pals1))]

    # Predefined color names:
    col_names <- grDevices::colors()[match(
      grDevices::rgb(t(grDevices::col2rgb(tst)), maxColorValue = 255),
      c(grDevices::rgb(t(grDevices::col2rgb(grDevices::colors())), maxColorValue = 255))
    )]

    sgb_names[is.na(sgb_names)] <- ""
    col_names[is.na(col_names)] <- ""

    # Processs name vectors to avoid duplicates:
    col_names[col_names == sgb_names] <- ""  # remove duplicates in col names.
    col_names[!col_names == "" & !sgb_names == ""] <-
      paste0("/", col_names[!col_names == "" & !sgb_names == ""])
    # adding a slash to distinguish different names for the same color.
    names(out_col) <- paste0(sgb_names, col_names)
  }

  # Remove names if required (default):
  if (!use_names)  out_col <- unname(out_col)
  if (!(is.null(alpha) | is.na(alpha))) {
    cmnt <- comment(out_col)  # save paletten name.
    out_col <- grDevices::adjustcolor(out_col, alpha.f = alpha)
    comment(out_col) <- cmnt  # restor name.
  }
  return(out_col)
}

## seecol: Plotte die Farben einer Palette oder mehrere Paletten: ----------

#' Farbpaletten plotten (um die Farben anzusehen).
#'
#' \code{seecol} bietet eine Schnittstelle zum Plotten (oder "Sehen")
#' der Farben einer Palette oder fuer den Vergleich mehrerer Farbpaletten.
#'
#' \code{seecol} hat 2 Hauptfunktionen, basierend auf den gegebenen \code{pal}-Argumenten:
#'
#' \enumerate{
#'
#' \item wenn \code{pal = "sgb_all"} (oder eine Liste mit mehreren Farbpaletten):
#' Plottet visuelle Vektoren aller aktuellen Farbpaletten, um sie zu vergleichen.
#'
#' \item wenn mit \code{pal} eine bestimmte Farbpalette (oder ein Vektor mit mehreren Farben
#' oder Farbpaletten) gewaehlt wird:
#' Zeichnet die aktuelle Farbpalette und optionale Details zu ihren Farben.
#' }
#'
#' @param pal Eine Farbpalette (als Vektor mit Farben),
#' ein character string, die von seecol als keyword erkannt wurde, oder
#' mehrere Paletten als list.
#' Default: \code{pal = "sgb_all"}.
#'
#' Erkannte keywords sind:
#' \enumerate{
#'   \item \code{"sgb_all"}: Alle Farbpaletten, die in \code{gsgb} definiert sind.
#'   \item \code{"sgb_basic"}: Alle SGB Standard-Farbpaletten.
#'   \item \code{"pair_all"}: Alle Farbpaletten mit paarweisen Farben.
#'   \item \code{"pref_all"}: Alle bevorzugten Farben und ihre Gradienten.
#'   \item \code{"grad_all"}:
#' }
#'
#' \code{seecol} erkennt auch umgekehrte keywords (e.g., \code{"all_sgb"}) oder
#' keywords ohne \code{"sgb"} (z.B. \code{"basic"}).
#'
#' @param n Anzahl Farben die angezeigt oder verwendet werden sollen.
#' Wenn \code{n} kleiner oder groesser als die Laenge der angegebenen Farbpalette \code{pal} ist,
#' wird die Farbalette reduziert oder extrapoliert
#' (mit \code{grDevices::colorRampPalette}).
#' Default: \code{n = "all"} (sprich zeige alle Farben der gewaehlten Palette).
#'
#' @param alpha Ein Faktor, der die Deckkraft (opacity alpha) modifiziert
#' (wie in \code{\link{adjustcolor}}); typischerweise in [0,1].
#' Default: \code{NA} (d.h. keine aenderung der Deckkraft).
#'
#' @param hex Sollen HEX Farbwerte gezeigt werden?
#' Default: \code{hex = NULL} (sprich zeige HEX Farbwerte
#' wenn es genug Platz hat um sie zu zeigen).
#'
#' @param rgb Sollen RGB Farbwerte gezeigt werden?
#' Default: \code{rgb = NULL} (sprich zeige RGB Farbwerte
#' wenn es genug Platz hat um sie zu zeigen).
#'
#' @param col_bg Hintergrundfarbe des Plots.
#' Default: \code{col_bg = NULL}.
#'
#' @param col_brd Farbe der Umrandung (falls gezeigt).
#' Default: \code{col_brd = NULL}.
#'
#' @param lwd_brd Linienstaerke der Umrandung (falls gezeigt).
#' Default: \code{lwd_brd = NULL}.
#'
#' @param grid Raster im Plot verwenden?
#' Default: \code{grid = TRUE}.
#'
#' @param title Plot-Titel
#' Default: \code{title = NA} generiert einen default Titel.
#'
#' @param ... Weitere grafische Parameter.
#' (passed to \code{plot}).
#'
#' @examples
#' # Alle Farbpaletten anschauen:
#' seecol()  # gleich wie seecol(pal = "all")
#'
#' # Details einer spezifischen Farbpalette anschauen:
#' seecol(pal_sgb_rot)
#'
#' # Farben oder Farbpaletten kombinieren:
#' seecol(c(rev(pal_sgb_rot), pal_sgb_dunkelblau))  # Farbpalette kombinieren
#' seecol(c(rev(pal_sgb_hellblau), "white", pal_sgb_grau))  # Farbpalette und Farbnamen kombinieren
#' seecol(c("black", "blueviolet", "gold"))          # Farbnamen kombinieren
#'
#' # Mit n die Farbpalette reduzieren oder erweitern:
#' seecol(n =  3)  # reduzierte Auswahl aller Paletten
#' seecol(n = 12)  # erweiterte Auswahl aller Paletten
#'
#' seecol(pal_sgb, n = 5,
#'        title = "Reduzierte Version von pal_sgb (n = 5)")  # pal_sgb reduzieren
#' seecol(pal_sgb_dunkelblau, n = 8,
#'        title = "Erweiterte Version von pal_sgb_dunkelblau (n = 8)")  # pal_sgb_dunkelblau erweitern
#'
#' # Farbpaletten erweitern und kombinieren
#' seecol(c(rev(pal_sgb_dunkelblau), "white", pal_sgb_gelb), n = 17,
#'        title = "Divergierende benutzerdefinierte Farbpalette mit 17 Farben")
#'
#' # Benutzerdefinierte Farbpaletten definieren:
#' pal_mpg <- c("#007367", "white", "#D0D3D4")
#' names(pal_mpg) <- c("mpg green", "mpg white", "mpg grey")
#'
#' # Erweiterte Farbpalette ansehen:
#' seecol(pal_mpg, n = 9, title = "Benutzerdefinierte Farbpalette fuer die Max-Planck-Gesellschaft")
#'
#' # Farbpaletten vergleichen:
#' seecol(list(pal_mpg, pal_sgb_hellblau, pal_sgb), n = 5)
#'
#' ## Farbpaletten von anderen Packages ansehen:
#' # library(RColorBrewer)
#' # seecol(brewer.pal(name = "RdBu", n = 11))  # "RdBu" Palette von RColorBrewer
#'
#' ## Farbpalette erweitern
#' # seecol(brewer.pal(name = "RdBu", n = 11), n = 15)  # Farbpalette auf 15 Farben erweitern
#'
#' @family Farbfunktionen
#'
#' @aliases seepal
#'
#' @seealso
#' \code{\link{usecol}} um eine Farbpalette zu verwenden;
#' \code{\link{pal_sgb}} die SGB Standard-Farbpalette.
#'
#' @export
seecol <- function(pal = "sgb_all",  # which palette to output?
                   n = "all",
                   alpha = NA,
                   hex = NULL,      # determine by crowdedness, whether hex values should be shown in detail view.
                   rgb = NULL,      # determine, whether rgb values should be shown in detail view (defaults to TRUE)
                   col_bg = NULL,   # color of background
                   col_brd = NULL,  # border color of the boxes
                   lwd_brd = NULL,  # line width of box borders
                   grid = TRUE,     # show grid?
                   title = NA,      # plot title? Using default title = NA constructs a default title
                   ...) {              # additional arguments to plot.default().

  ## 0. Preparations: -----

  op <- graphics::par(no.readonly = TRUE)  # save original plotting settings.
  keys <- c("all", "sgb_all", "all_sgb",  # all palettes
            "basic", "sgb_basic", "basic_sgb",  # the basic palettes.
            "pair", "all_pair", "pair_all",  # all paired palettes.
            "pref", "pref_all", "all_pref",  # the preferred palettes and gradients.
            "grad", "grad_all", "all_grad"  # the gradients.
  )

  # Robustify inputs:

  ## Plotting parameters: ----
  if (!(is.null(hex) | is.logical(hex))) stop("Please specify a valid value for 'hex'.")
  if (!(is.null(rgb) | is.logical(rgb))) stop("Please specify a valid value for 'rgb'.")

  ## Check, whether keyword is used:
  by_key <- tryCatch(all(pal %in% keys),
                     error = function(e) FALSE,
                     silent = TRUE)

  ## Check whether pal input is a list:
  compare <- tryCatch(is.list(pal) & any(lapply(pal, length) > 1),   # get length of each component.
    error = function(e) FALSE,
    silent = TRUE)

  ## Getting a list of palettes by keyword:
  if (by_key) {
    ## Plot title:
    ## Define title given keyword:
    if (is.na(title)){
      if (pal %in% c("all", "sgb_all", "all_sgb") ) title <- "Alle SGB Farbpaletten"
      if (pal %in% c("basic", "sgb_basic", "basic_sgb")) title <- "Alle SGB Standard-Farbpaletten"
      if (pal %in% c("pair", "all_pair", "pair_all")) title <- "Alle paarweisen SGB Farbpaletten"
      if (pal %in% c("pref", "pref_all", "all_pref")) title <- "Alle bevorzugten SGB-Farben und Gradienten"
      if (pal %in% c("grad", "grad_all", "all_grad")) title <- "Alle SGB-Farbgradienten"
    }

    pal_tmp <- getpal_key(pal = pal, n = n, alpha = alpha)  # get the color by key.

  } else if (compare){
    pal_tmp <- lapply(X = pal, usecol, n = n, alpha = alpha, use_names = TRUE)  # get all palettes separately.

    if (is.na(title)){
      title <- "Compare a custom set of color palettes"
    }

    names(pal_tmp) <- lapply(pal_tmp, comment)  # assign names from comment attribute.

    ## Check for names:
    if (is.null(names(pal_tmp))) {
      names(pal_tmp) <- paste0("pal", 1:length(pal_tmp))
      # ToDo: Use argument name.

    } else if (any(names(pal_tmp) == "custom")) {
      names(pal_tmp)[names(pal_tmp) == "custom"] <- paste0("pal", which(names(pal_tmp) == "custom"))
      # ToDo: Use argument name.
    }

  } else {  # if no keyword or list for comparison was given:
    ## Get palette:
    pal_tmp <- usecol(pal = pal, n = n, alpha = alpha, use_names = TRUE)  # create a list of length 1.
    nm <- ifelse(length(unlist(pal_tmp)) == 1 | comment(pal_tmp) == "custom",
                 "", paste0(" ", comment(pal_tmp)))
    pl <- ifelse(length(unlist(pal_tmp)) == 1, names(pal_tmp), "palette")  # classify as palette or not.
    cst <- ifelse(comment(pal_tmp) == "custom" & length(unlist(pal_tmp)) != 1, "custom ", "")

    if (is.na(title)){
      title <- paste0("See ", cst, "color ", pl, nm)  # assemble title.
    }

    pal_tmp <- list(pal_tmp)  # now list the palette and leave the comment attribute.
  }

  if (n != "all" | !is.na(alpha)) {
    n_txt <- ifelse(n != "all", paste0("n = ", n), "")
    alp_txt <- ifelse(!is.na(alpha), paste0("alpha = ", alpha), "")
    comma <- ifelse(nchar(n_txt) == 0 | nchar(alp_txt) == 0, "", ", ")

    if (is.na(title)){
      title <- paste0(title, " (", alp_txt, comma, n_txt, ")")
    }
  }

  ## Check interplay of col_brd and lwd_brd:
  if (!is.null(lwd_brd) && (lwd_brd <= 0)){
    message("Setting (lwd_brd <= 0) is not allowed: Using lwd_brd = NULL.")
    lwd_brd <- NULL  # correct to default
  }

  if (!is.null(col_brd) && is.null(lwd_brd)){
    message("Setting col_brd requires lwd_brd: Using lwd_brd = 1.")
    lwd_brd <- 1   # correct to sensible value
  }

  if (!is.null(lwd_brd) && is.null(col_brd)){
    message("Setting lwd_brd requires col_brd: Using col_brd = 'white'.")
    col_brd <- "white"   # correct to sensible value
  }

  ## 2. Plotting parameters: ------

  ## Plotting preparations:
  distance <- 0   # set distance of boxes?
  xlen <- 1       # set x length of color boxes.

  # Get maximum number of colors:
  max_ncol <- max(unlist(lapply(pal_tmp, FUN = length)))

  # Determine xlim based on distance, maximum number of colors, and box length:
  xlim <- c(0 - distance, (1 + distance) * xlen) * max_ncol * xlen

  # Determine ylim as number of colors in the palette:
  ylim <- c(0, length(pal_tmp) + 0.2)

  # Bind palette(s) to their color index:
  pal_mat <- cbind(pal_tmp, length(pal_tmp):1)  # TODO: Note, that a single palette needs to be a list of length 1!

  ## 3. Plotting: ------

  ## 3.1 Plot an overview for a list of palettes:
  ## Possible solution: (a) 1 list entry --> details; (b) more than 1 list entry --> comparison:
  if (length(pal_tmp) > 1) {

    # Set margins:
    graphics::par(mar = c(3, 6, 3, 1))

    # Set bg color:
    graphics::par(bg = col_bg)

    # Create empty plot:
    graphics::plot(x = 0, type = "n", xlim = xlim, ylim = ylim,
         xaxt = "n", yaxt = "n",  # hide axes.
         xlab = "", ylab = "",
         main = title,
         bty = "n",
         ...  # other graphical parameters
    )

    if (grid) {
      x_vals <- 0:max(ylim)
      dims <- max(ylim) * max_ncol
      grfac <- c(3, 4, 5)[c(dims < 100, dims > 100 & dims < 150, dims > 150)]
      # ensure an appropriate number of vertical lines using gridfactor.
      y_vals <- 1:max_ncol
      y_vals <- y_vals[(y_vals %% grfac) == 0]  # steps of 5
      y_vals <- y_vals - xlen/2
      graphics::abline(h = x_vals,
             v = y_vals,
             col = grDevices::grey(.50, .50),
             lwd = .5)
    } # if (grid) etc.

    ## Dynamic updating of ylen on number of palettes:
    ylen <- 0.8

    # Add the color vectors:
    # if (is.null(lwd_brd)) { lwd_brd <- 0 } # set default lwd_brd
    apply(pal_mat, MARGIN = 1, FUN = function(row) {
      plot_col(x = row[[1]], ypos = row[2], plot.new = FALSE, ylen = ylen, col_brd = col_brd, lwd = lwd_brd)
    })

    # Add color names and indices:
    cex_lbl <- .90
    pal_nm <- names(pal_tmp)  # get palette names.
    graphics::text(x = 0, y = 1:length(pal_tmp), labels = rev(pal_nm),
         cex = cex_lbl, pos = 2, xpd = TRUE,
         offset = 1)  # 1 character.

    txt_ind <- paste0("[", 1:max_ncol, "]")
    cex_ind <- graphics::par("cex")
    wdth_ind <- sum(graphics::strwidth(txt_ind, cex = cex_ind))
    pos_ind <- seq(0.5, (max_ncol - 0.5), by = 1)

    while (wdth_ind > xlim[2]) {
      txt_ind <- txt_ind[seq(1, length(txt_ind), by = 2)]  # only show every second index.
      pos_ind <- pos_ind[seq(1, length(pos_ind), by = 2)]
      wdth_ind <- sum(graphics::strwidth(txt_ind, cex = cex_ind))  # is the width small enough?
    }

    # Color indices:
    cex_ixs <- .80
    yix <- -0.02 * length(pal_tmp)  # dnamic positioning of indices.

    graphics::text(x = pos_ind, y = yix, labels = txt_ind, pos = 1, xpd = TRUE,
         cex = cex_ixs, col = grDevices::grey(0, 2/3))

  } else {  # if length(pal_tmp) list is NOT > 1:

    # 3.2 Detailed view of 1 palette: ------
    names(pal_tmp) <- NULL  # remove first order names!
    pal_tmp <- unlist(pal_tmp)

    # Set margins:
    graphics::par(mar = c(3, 2, 3, 1))

    # Set bg color:
    graphics::par(bg = col_bg)

    # Create empty plot:
    graphics::plot(x = 0, type = "n", xlim = xlim, ylim = c(-1, 2),
         xaxt = "n", yaxt = "n",  # hide axes.
         xlab = "", ylab = "",
         main = title,
         bty = "n",
         ...  # other graphical parameters
    )

    # Text elements:
    txt_pos <- seq(0.5, length(pal_tmp) - 0.5)

    # y positions:
    y_names <- 1.5
    y_circ  <- 1.2
    y_rect  <- 0.6
    y_rgb   <- c(-0.50, -0.65, -0.80)

    # Grid:
    if (grid) {
      graphics::abline(# h = c(c(0.6, 1.2, 1.6, -0.1), c(y_rgb, -0.95) + 0.07),
        # h = c(-.88, -.43, -.10, y_rect, y_circ),
        h = c(-1, -.05, y_rect, y_circ),
        # v = txt_pos,
        col = grDevices::grey(.5, .5),
        lwd = .3)

      xpos <- txt_pos
      if (length(xpos) > 10) xpos <- xpos[xpos %% 5 == 0.5]  # steps of five.

      graphics::segments(
        x0 = xpos,
        y0 = 2, y1 = -0.2,
        col = grDevices::grey(.5, .5),
        lwd = .3)
    }

    # Find cex so that it is as large as possible:
    cex_lim <- 0.7  # lower limit for cex.

    # Determine whether to display hex values:
    cex_hex <- 0.9  # was par("cex")
    placeholder <- ifelse(is.na(alpha), " #XXXXXX", " #XXXXXXXX")
    wdth_hex <- graphics::strwidth(placeholder, cex = cex_hex) * max_ncol + graphics::strwidth("Hex: ")  # is the width small enough?

    while (wdth_hex > xlim[2]) {
      cex_hex <- cex_hex - 0.1
      wdth_hex <- graphics::strwidth(placeholder, cex = cex_hex) * max_ncol + graphics::strwidth("Hex: ")  # is the width small enough?
    }

    # If hex is NULL, determine based on width and max cex.
    # Otherwise use the provided value:
    if (is.null(hex)) {
      hex <- ifelse(wdth_hex > xlim[2] | cex_hex < cex_lim, FALSE, TRUE)  # test, whether hex can be displayed.
    }

    # Determine, whether to display rgb values:
    cex_rgb <- 0.9
    wdth_rgb <- graphics::strwidth(" 999 ", cex = cex_rgb) * max_ncol
    while (wdth_rgb > xlim[2]) {
      cex_rgb <- cex_rgb - 0.1
      wdth_rgb <- graphics::strwidth(" 999 ", cex = cex_rgb) * max_ncol  # is the width small enough?
    }

    # If rgb is NULL, determine based on width and max cex.
    # Otherwise use the provided value:
    if (is.null(rgb) ) {
      rgb <- ifelse(wdth_rgb > xlim[2] | cex_rgb < cex_lim, FALSE, TRUE)
    }

    # Plot rectangles:
    # if (is.null(lwd_brd)) { lwd_brd <- 1 } # set default lwd_brd
    plot_col(x = pal_tmp, ypos = y_rect, shape = "rect", ylen = 0.5, plot.new = FALSE, col_brd = col_brd, lwd = lwd_brd#,
             # ...  # other graphical parameters
    )

    # Plot circles:
    circle_len <- ifelse(((xlim[2] / 10) < 0.7), (xlim[2] / 10), .70)
    plot_col(x = pal_tmp, ypos = y_circ, shape = "circle", xlen = circle_len, plot.new = FALSE, col_brd = col_brd, lwd = lwd_brd#,
             # ...  # other graphical parameters
    )

    # Color names:
    col_names <- names(pal_tmp)
    y_names <- y_circ + (circle_len * 4 / max_ncol)  # determine y_names based on circle position and size.
    graphics::text(x = txt_pos, y = y_names, labels = col_names, # pos = 3,
         srt = 45, xpd = TRUE, offset = 1, cex = 0.8,
         adj = c(0, 0))

    # Spacing of indices:
    txt_ind <- paste0("[", 1:length(pal_tmp), "]")
    cex_ind <- graphics::par("cex")
    wdth_ind <- sum(graphics::strwidth(txt_ind, cex = cex_ind))
    pos_ind <- txt_pos

    while (wdth_ind > xlim[2]) {
      txt_ind <- txt_ind[seq(1, length(txt_ind), by = 2)]  # only show every second index.
      pos_ind <- pos_ind[seq(1, length(pos_ind), by = 2)]
      wdth_ind <- sum(graphics::strwidth(txt_ind, cex = cex_ind))  # is the width small enough?
    }

    # Color indices:
    cex_ixs <- .80
    graphics::text(x = pos_ind, y = 0, labels = txt_ind, pos = 3, xpd = TRUE,
         cex = cex_ixs, col = grDevices::grey(0, 2/3))

    # Hex values:
    if (hex) {
      ## Convert to hex if not already given in this format:
      if (!all(isHexCol(pal_tmp))) {
        pal_tmp <- grDevices::rgb(t(grDevices::col2rgb(pal_tmp)), maxColorValue = 255)
      }
      yhex <- -0.25

      # Plot the values:
      graphics::text(x = 0, y = yhex, labels = "Hex:", font = 2, pos = 2, offset = 0, xpd = TRUE,
           cex = cex_hex)
      graphics::text(x = txt_pos, y = yhex, labels = pal_tmp, pos = NULL, xpd = TRUE,
           cex = cex_hex, srt = 0)
    } # if (hex) etc.

    # RGB values:
    if (rgb) {
      graphics::text(x = rep(0, 3),
           y = y_rgb,
           labels = c("R:", "G:", "B:"), font = 2,
           pos = 2, offset = 0, xpd = TRUE,
           cex = cex_rgb)

      graphics::text(x = matrix(rep(txt_pos, 3), nrow = 3, byrow = TRUE),
           y = matrix(rep(y_rgb, length(txt_pos) + 1), nrow = 3),
           labels = grDevices::col2rgb(pal_tmp),
           pos = 2, offset = -.67, xpd = TRUE,
           cex = cex_rgb)

    } # if (rgb) etc.
  }  # if (length(pal_tmp) > 1) etc.

  # Reset plotting parameters:
  graphics::par(op)

  # Invisibly return pal_tmp palette(s):
  invisible(pal_tmp)
}


## newpal: Neue Farbpalette definieren: ----------

#' Definiere eine neue Farbpalette
#'
#' \code{newpal} ermoeglicht die Definition neuer Farbpaletten
#' (als data frames).
#'
#' @param col Ein erforderlicher Farbvektor
#' (angegeben durch R-Farbnamen, HEX-Codes oder RGB-Werte).
#'
#' @param names Ein optionaler character Vektor mit Namen.
#' Default: \code{names = NA}, ergibt numerische Namen.
#'
#' @param as_df Soll die neue Farbpalette zurueckgegeben werden als
#' data frame (statt als Vektor)?
#' Default: \code{as_df = FALSE}.
#'
#' @examples
#'
#' newpal(col = c("black", "white"), names = c("dark", "bright"))
#'
#' # Example: 3 Arten eine neue Farbpalette zu definieren:
#'
#' # (1) Mit R Farbnamen:  -----
#' pal_flag_de <- newpal(col = c("black", "firebrick3", "gold"),
#'                       names = c("Schwarz", "Rot", "Gold"))
#'
#' seecol(pal_flag_de, title = "Farben der deutschen Flagge")
#'
#' # (2) Mit HEX-Codes:  -----
#' # (a) Google logo colors:
#' # Source: https://www.schemecolor.com/google-logo-colors.php
#' color_google <- c("#4285f4", "#34a853", "#fbbc05", "#ea4335")
#' names_google <- c("blueberry", "sea green", "selective yellow", "cinnabar")
#' pal_google   <- newpal(color_google, names_google)
#' seecol(pal_google, title = "Colors of the Google logo", col_brd = "white", lwd_brd = 10)
#'
#' # (b) German flag revised:
#' # Based on a different source at
#' # <https://www.schemecolor.com/germany-flag-colors.php>:
#' pal_flag_de_2 <- newpal(col = c("#000000", "#dd0000", "#ffce00"),
#'                         names = c("black", "red", "gold")
#'                         )
#' seecol(pal_flag_de_2, title = "Colors of the German flag (www.schemecolor.com)")
#'
#' # (c) MPG colors:
#' pal_mpg <- newpal(col = c("#007367", "white", "#D0D3D4"),
#'                   names = c("mpg green", "white", "mpg grey")
#'                   )
#' seecol(pal_mpg, title = "Colors of the Max Planck Society")
#'
#' # (3) From RGB values:  -----
#' \dontrun{
#' library(grDevices)
#'
#' # Barrier-free color palette
#' # Source: Okabe & Ito (2002): Color Universal Design (CUD):
#' #         Fig. 16 of <https://jfly.uni-koeln.de/color/>:
#'
#' # (a) Vector of colors (as RGB values):
#' o_i_colors <- c(rgb(  0,   0,   0, maxColorValue = 255),  # black
#'                 rgb(230, 159,   0, maxColorValue = 255),  # orange
#'                 rgb( 86, 180, 233, maxColorValue = 255),  # skyblue
#'                 rgb(  0, 158, 115, maxColorValue = 255),  # green
#'                 rgb(240, 228,  66, maxColorValue = 255),  # yellow
#'                 rgb(  0, 114, 178, maxColorValue = 255),  # blue
#'                 rgb(213,  94,   0, maxColorValue = 255),  # vermillion
#'                 rgb(204, 121, 167, maxColorValue = 255)   # purple
#' )
#'
#' # (b) Vector of color names:
#' o_i_names <- c("black", "orange", "skyblue", "green", "yellow", "blue", "vermillion", "purple")
#'
#' # (c) Use newpal() to combine colors and names:
#' pal_okabe_ito <- newpal(col = o_i_colors,
#'                         names = o_i_names)
#'
#' seecol(pal_okabe_ito,
#'        title = "Color-blind friendly color scale (Okabe & Ito, 2002)")
#'
#' # Compare custom color palettes:
#' my_pals <- list(pal_flag_de, pal_flag_de_2, pal_google, pal_mpg, pal_okabe_ito)
#' seecol(my_pals, col_brd = "white", lwd_brd = 5,
#'        title = "Comparing custom color palettes")
#'
#' }
#'
#' @family Farbfunktionen
#'
#' @aliases defpal
#' @aliases defcol
#'
#' @seealso
#' \code{\link{seepal}} um eine Farbpalette zu plotten;
#' \code{\link{usecol}} um eine Farbpalette zu verwenden.
#'
#' @export
newpal <- function(col,            # a vector of colors
                   names = NA,     # a vector of names
                   as_df = FALSE   # return palette as df?
                   # ...           # additional arguments to usecol().
) {

  ## 0. Preparations: -----
  outpal <- NA  # initialize

  # 0. Robustify inputs:
  if (any(is.na(col))) stop("'col' must be a vector of (named or hex) colors without NA values.")
  if (any(!isCol(col))) stop("'col' must be a vector containing ONLY (named or hex) colors.")
  if (any(!is.na(names)) && ((length(col) != length(names))) ) {
    message(paste0("Length of 'col' = ", length(col),
                   " vs. 'names' = ",    length(names), ". Using default (numeric) names..."))
    names <- NA
  }

  # 1. Create data.frame or vector of col: -----
  outpal <- col  # copy col vector

  # 2. Add names:
  if (all(!is.na(names))) {
    names(outpal) <- names
  } else {
    names(outpal) <- as.character(1:length(col))
  }

  # # Apply ... arguments:
  # outpal <- usecol(pal = outpal, use_names = TRUE, ...)

  # If return as_df:
  if (as_df) {
    outpal <- data.frame(outpal, stringsAsFactors = FALSE) # df as column
    outpal <- t(outpal) # df as row
    outpal <- data.frame(outpal, row.names = NULL, stringsAsFactors = FALSE)
  }

  # 2. Return: -----
  return(outpal)
} # newpal end.


