# Testet color_fun.R
# AutorInnen: Kristina Schuepbach
# Datum: 10.06.2020

# Einfuehrung ----------------------------------

# Diese Datei tested das Script "color_fun.R" indem es die einzelnen Funktionen untersucht
# und prueft, ob die erwarteten Outputs generiert werden. Nach jeder Veraenderung im getesten Script
# muss diese Test-Datei ausgefuehrt werden (CTRL + Shift + T), um allfaellige Fehler sofort zu entdecken.

# Testing  ------------------------------------

## pal_sgb_web ----------------------------------------

# Erstelle einen Kontext, um die zu testenden Teile voneinander abzugrenzen
testthat::context("Teste die Funktion usecol")

testthat::test_that("Unchanged pal_sgb_pref" , {
  mycol <- usecol(pal = pal_sgb_pref)
  testthat::expect_equivalent(
    mycol,
    c("#990000",
      "#ffc000",
      "#002060",
      "#00a9e0",
      "#7c7c7c")
  )
})

testthat::test_that("Reduced pal_sgb_web" , {
  mycol <- usecol(pal = pal_sgb, n = 5)
  testthat::expect_equivalent(
    mycol,
    c("#990000",
      "#eb836a",
      "#ffffff",
      "#a6a6a6",
      "#545454")
  )
})
