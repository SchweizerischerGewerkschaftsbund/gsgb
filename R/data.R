# SGB CI
# Autor: Kristina Schuepbach
# Datum: 10.06.2020

# Packages ------------------------------------

# Bei den Packages wird nicht die gesamte Library geladen, sondern nur die benoetigten
# Funktionen eines Package direkt mit "package::" aufgerufen.


# Daten laden -----------------------------

# Die benoetigten Daten werden aus dem data/-Ordner geladen
data("kof", envir = environment())
data("mangelindikator", envir = environment())

#' KOF-Beispieldaten
#'
#' Der Datensatz enthaelt eine Variable aus den KOF-Konjunkturdaten fuer drei Branchen.
#' Die Daten sind im long-Format, sodass sie direkt mit ggplot verwendet werden koennen.
#'
#' @format Ein Dataframe mit 591 Zeilen und 4 Variablen.
"kof"

#' Mangelindikator Beispieldaten
#'
#' Der Datensatz enthaelt den Mangelindikator fuer eine Branche, mit vier Ausbildungslevels.
#' Die Daten sind im long-Format, sodass sie direkt mit ggplot verwendet werden koennen.
#'
#' @format Ein Dataframe mit 260 Zeilen und 4 Variablen.
"mangelindikator"

