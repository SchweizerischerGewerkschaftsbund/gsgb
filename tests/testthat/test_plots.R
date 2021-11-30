## test_plots.R  |  gsgb
## SGB | 19.05.2021
## ---------------------------

library(ggplot2)
theme_set(theme_sgb())

testthat::context("Teste ob Standardplots ohne Error funktionieren")

testthat::test_that("No error with geom_line" , {
  p <- ggplot(kof) +
    geom_line(aes(x = time, y = value, colour = branche), size = 1) +
    geom_hline(yintercept = 0, size = 0.5) +
    labs(x = "", y = "")
  testthat::expect_error(p, NA) # NA = no error
})

testthat::test_that("Correct colors with geom_line & colour" , {
  p <- ggplot_build(ggplot(kof) +
    geom_line(aes(x = time, y = value, colour = branche), size = 1) +
    geom_hline(yintercept = 0, size = 0.5) +
    labs(x = "", y = ""))
  used_colors <- p$data[[1]]["colour"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

