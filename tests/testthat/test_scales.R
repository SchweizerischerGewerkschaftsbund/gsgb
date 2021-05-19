## test_scales.R  |  gsgb
## SGB | 19.05.2021
## ---------------------------

library(ggplot2)
theme_set(theme_sgb())

# Scale color discrete ----
testthat::context("Teste ob richtige Farben mit discrete colors verwendet werden.")

testthat::test_that("Correct colors (default)" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                      geom_point())
  used_colors <- p$data[[1]]["colour"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

testthat::test_that("Correct colors with scale_color_discrete()" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
                      geom_point() +
                      scale_color_discrete())
  used_colors <- p$data[[1]]["colour"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

testthat::test_that("Correct colors with scale_colour_discrete()" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
                      geom_point() +
                      scale_colour_discrete())
  used_colors <- p$data[[1]]["colour"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

testthat::test_that("Colors change if selected manually" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
                      geom_point() +
                      scale_color_manual(values = usecol(pal_sgb_rot)))
  used_colors <- p$data[[1]]["colour"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_rot[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})


# Scale fill discrete ----
testthat::context("Teste ob richtige Farben mit discrete fill verwendet werden.")

testthat::test_that("Correct colors (default)" , {
  p <- ggplot_build(ggplot(mpg, aes(as.factor(year), fill = drv)) +
                      geom_bar(position = "dodge"))
  used_colors <- p$data[[1]]["fill"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

testthat::test_that("Correct colors with scale_fill_discrete()" , {
  p <- ggplot_build(ggplot(mpg, aes(as.factor(year), fill = drv)) +
                      geom_bar(position = "dodge") +
                      scale_fill_discrete())
  used_colors <- p$data[[1]]["fill"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_pref[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

testthat::test_that("Colors change if selected manually" , {
  p <- ggplot_build(ggplot(mpg, aes(as.factor(year), fill = drv)) +
                      geom_bar(position = "dodge") +
                      scale_fill_manual(values = usecol(pal_sgb_dunkelblau)))
  used_colors <- p$data[[1]]["fill"][[1]] %>%
    unique() %>% unname()
  testthat::expect_equal(used_colors,
                         pal_sgb_dunkelblau[1:3] %>% as.matrix() %>% as.vector() %>% unname())
})

# Scale fill continuous ----
testthat::context("Teste ob richtige Farben mit continuous fill verwendet werden.")

testthat::test_that("Correct colors (default)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5))
  used_colors <- p$data[[1]]["fill"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colors[10], unname(pal_sgb_rot[1]))
})

testthat::test_that("Correct colors with scale_fill_continuous() (default)" , {
  p <- ggplot_build(ggplot(faithfuld, aes(waiting, eruptions)) +
                      geom_raster(aes(fill = density))+
                      scale_fill_continuous())
  p_true <- ggplot_build(ggplot(faithfuld, aes(waiting, eruptions)) +
                           geom_raster(aes(fill = density)) +
                           scale_fill_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1]))
  testthat::expect_equal(p$data[[1]]["fill"][[1]], p_true$data[[1]]["fill"][[1]])
})

testthat::test_that("Correct colors with scale_fill_gradient (custom)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5) +
                      scale_fill_gradient(low = pal_sgb_gelb[1], high = pal_sgb_dunkelblau[1]))
  used_colors <- p$data[[1]]["fill"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_dunkelblau[1]))
  testthat::expect_equal(used_colors[10], unname(pal_sgb_gelb[1]))
})

testthat::test_that("Correct colors with gradient2 (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5) +
                      scale_fill_gradient2(midpoint = 5))
  used_colors <- p$data[[1]]["fill"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_dunkelblau[5]))
  testthat::expect_equal(used_colors[6], unname(pal_sgb_hellblau[2]))
  testthat::expect_equal(used_colors[11], unname(pal_sgb_rot[5]))
})

testthat::test_that("Correct colors with gradient2 (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5) +
                      scale_fill_gradient2(midpoint = 5, low = pal_sgb_rot[5],
                                           mid = "white", high = pal_sgb_gelb[5]))
  used_colors <- p$data[[1]]["fill"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colors[6], "#ffffff")
  testthat::expect_equal(used_colors[11], unname(pal_sgb_gelb[5]))
})

testthat::test_that("Correct colors with gradientn (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5) +
                      scale_fill_gradientn())
  used_colors <- p$data[[1]]["fill"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb[1]))
  testthat::expect_equal(used_colors[6], "#ffffff")
  testthat::expect_equal(used_colors[11], unname(pal_sgb[11]))
})

testthat::test_that("Correct colors with gradientn (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(fill = z), size = 5) +
                      scale_fill_gradientn(colors = pal_sgb_pref))
  used_colors <- p$data[[1]]["fill"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_pref[1]))
  testthat::expect_equal(used_colors[6], unname(SGBdunkelblau))
  testthat::expect_equal(used_colors[11], unname(pal_sgb_pref[5]))
})


# Scale color continuous -----
testthat::context("Teste ob richtige Farben mit continuous color verwendet werden.")

testthat::test_that("Correct colors (default)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA))
  used_colors <- p$data[[1]]["colour"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colors[10], unname(pal_sgb_rot[1]))
})

testthat::test_that("Correct colors with scale_color_continuous() (default)" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
                      geom_point(aes(color = Sepal.Length)) +
                      scale_color_continuous())
  p_true <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
                           geom_point(aes(color = Sepal.Length)) +
                           scale_color_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1]))
  testthat::expect_equal(p$data[[1]]["colour"][[1]], p_true$data[[1]]["colour"][[1]])
})

testthat::test_that("Correct colors with scale_color_gradient (custom)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA) +
                      scale_color_gradient(low = pal_sgb_gelb[1], high = pal_sgb_dunkelblau[1]))
  used_colors <- p$data[[1]]["colour"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_dunkelblau[1]))
  testthat::expect_equal(used_colors[10], unname(pal_sgb_gelb[1]))
})

testthat::test_that("Correct colors with gradient2 (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA) +
                      scale_color_gradient2(midpoint = 5))
  used_colors <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_dunkelblau[5]))
  testthat::expect_equal(used_colors[6], unname(pal_sgb_hellblau[2]))
  testthat::expect_equal(used_colors[11], unname(pal_sgb_rot[5]))
})

testthat::test_that("Correct colors with gradient2 (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA) +
                      scale_color_gradient2(midpoint = 5, low = pal_sgb_rot[5],
                                           mid = "white", high = pal_sgb_gelb[5]))
  used_colors <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colors[6], "#ffffff")
  testthat::expect_equal(used_colors[11], unname(pal_sgb_gelb[5]))
})

testthat::test_that("Correct colors with gradientn (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA) +
                      scale_color_gradientn())
  used_colors <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb[1]))
  testthat::expect_equal(used_colors[6], "#ffffff")
  testthat::expect_equal(used_colors[11], unname(pal_sgb[11]))
})

testthat::test_that("Correct colors with gradientn (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(color = z), size = 5, fill = NA) +
                      scale_color_gradientn(colors = pal_sgb_pref))
  used_colors <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colors[1], unname(pal_sgb_pref[1]))
  testthat::expect_equal(used_colors[6], unname(SGBdunkelblau))
  testthat::expect_equal(used_colors[11], unname(pal_sgb_pref[5]))
})


# Scale colour continuous -----
testthat::context("Teste ob richtige Farben mit continuous colour verwendet werden.")

testthat::test_that("Correct colours (default)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA))
  used_colours <- p$data[[1]]["colour"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colours[10], unname(pal_sgb_rot[1]))
})

testthat::test_that("Correct colours with scale_colour_continuous() (default)" , {
  p <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
                      geom_point(aes(colour = Sepal.Length)) +
                      scale_colour_continuous())
  p_true <- ggplot_build(ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
                           geom_point(aes(colour = Sepal.Length)) +
                           scale_colour_gradient(low = pal_sgb_rot[5], high = pal_sgb_rot[1]))
  testthat::expect_equal(p$data[[1]]["colour"][[1]], p_true$data[[1]]["colour"][[1]])
})

testthat::test_that("Correct colours with scale_colour_gradient (custom)" , {
  df <- data.frame(x = 1, y = 1:10, z = c(1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA) +
                      scale_colour_gradient(low = pal_sgb_gelb[1], high = pal_sgb_dunkelblau[1]))
  used_colours <- p$data[[1]]["colour"][[1]] %>% unname() %>% sort() %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb_dunkelblau[1]))
  testthat::expect_equal(used_colours[10], unname(pal_sgb_gelb[1]))
})

testthat::test_that("Correct colours with gradient2 (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA) +
                      scale_colour_gradient2(midpoint = 5))
  used_colours <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb_dunkelblau[5]))
  testthat::expect_equal(used_colours[6], unname(pal_sgb_hellblau[2]))
  testthat::expect_equal(used_colours[11], unname(pal_sgb_rot[5]))
})

testthat::test_that("Correct colours with gradient2 (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA) +
                      scale_colour_gradient2(midpoint = 5, low = pal_sgb_rot[5],
                                            mid = "white", high = pal_sgb_gelb[5]))
  used_colours <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb_rot[5]))
  testthat::expect_equal(used_colours[6], "#ffffff")
  testthat::expect_equal(used_colours[11], unname(pal_sgb_gelb[5]))
})

testthat::test_that("Correct colours with gradientn (default)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA) +
                      scale_colour_gradientn())
  used_colours <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb[1]))
  testthat::expect_equal(used_colours[6], "#ffffff")
  testthat::expect_equal(used_colours[11], unname(pal_sgb[11]))
})

testthat::test_that("Correct colours with gradientn (custom)" , {
  df <- data.frame(x = 1, y = 0:10, z = c(0, 1, 3, 4, 7, 5, 8, 9, 2, 6, 10))
  p <- ggplot_build(ggplot(df, aes(x, y)) +
                      geom_tile(aes(colour = z), size = 5, fill = NA) +
                      scale_colour_gradientn(colours = pal_sgb_pref))
  used_colours <- p$data[[1]]["colour"][[1]] %>% tolower()

  testthat::expect_equal(used_colours[1], unname(pal_sgb_pref[1]))
  testthat::expect_equal(used_colours[6], unname(SGBdunkelblau))
  testthat::expect_equal(used_colours[11], unname(pal_sgb_pref[5]))
})




