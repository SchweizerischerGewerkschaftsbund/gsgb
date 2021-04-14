## plot_examples.R  |  gsgb
## SGB | 10.06.2020
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

# Weitere Tests mit :
## Linien mit 4 Variablen, im EMF-Format sichern ----
# (emf ist ein Vektorformat, das von Word akzeptiert wird)
devEMF::emf(file = "docs/lineplot2.emf", width = 6.2, height = 2.75) # width/height in inches
ggplot(mangelindikator, aes(x = time, y = value, colour = label)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "", y = "Mangelindikator")
dev.off()


## Saeulen (bar), als PDF speichern ----
pdf("docs/barplot.pdf", width = 10, height = 7)
ggplot(mpg, aes(as.factor(year), fill = drv)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "")
dev.off()


## Saeulen gestapelt (geom_bar) ----
# plus Reihenfolge der Palette anpassen und Ausrichtung der x-Achsen-Labels
ggplot(mpg, aes(manufacturer, fill = fl)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = rev(usecol(pal = pal_sgb_pref, n = 5))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("docs/stack_bar.jpeg", wide = TRUE)


## Kuchen-Diagramm, theme_sgb_minimal ----
library(magrittr)
df <- data.frame(group = c("Oberstes 1%", "Uebrige 9%", "Unterste 90%"),
                 value = c(42, 33, 25))
df <- df %>%
  dplyr::arrange(dplyr::desc(group)) %>%
  dplyr::mutate(prop = value / sum(df$value) * 100) %>%
  dplyr::mutate(ypos = cumsum(prop) - 0.5 * prop)

ggplot(df, aes(x = "", y = prop, fill = group)) +
  geom_bar(stat = "identity",
           width = 1,
           color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = scales::percent(prop / 100)),
            color = "white",
            size = 5) +
  theme_sgb_blank() + # minimales Theme ohne Achsen und andere Beschriftungen
  theme(
    legend.position = "right",
    legend.direction =   "vertical",
    legend.justification = c("right", "center"))
 ggsave("docs/pie_chart.jpeg", wide = TRUE)


## Gestapeltes Flaechendiagramm ----
df2 <- data.frame(
  time = as.numeric(rep(seq(1, 5), each = 5)),
  value = runif(25, 10, 100),
  group = rep(LETTERS[1:5], times = 5))
ggplot(df2, aes(x = time, y = value, fill = group)) +
  geom_area() +
  scale_fill_manual(values = usecol(pal = pal_sgb_rot, n = 5))
ggsave("docs/stacked_area.jpeg", wide = TRUE)


## Punkte discrete (Scatterplot) ----
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 2) +
  labs(x = "", y = "")
ggsave("docs/scatterplot.jpeg")
ggsave("docs/scatterplot_wide.jpeg", wide = TRUE)


## Punkte continous (Scatterplot mit gradient) ----
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 2) +
  scale_color_gradientn(colours = usecol(pal = pal_sgb_rot)) +
  labs(x = "", y = "")
ggsave("docs/scatter_continous_wide.jpeg", wide = TRUE)
