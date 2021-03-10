# Fonts
# install.packages("showtext")
# library(showtext)
# font_paths() #check that font is installed here
# font <- font_files() # see how available fonts are named
# font_add("NimbusSanNov", regular = "NimbusSanNov-Reg.otf")

# Vector graphs for Word
# install.packages("devEMF")
# library(officer)
library(devEMF)


# Beispiele --------------------------------------
library(ggplot2)
library(tidyverse)
library(gsgb)
library(scales)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Beispieldaten
# Die benoetigten Daten werden aus dem data/-Ordner geladen
data("kof", envir = environment())
data("mangelindikator", envir = environment())

df <- data.frame(
  group = c("Oberstes 1%", "Übrige 9%", "Unterste 90%"),
  value = c(42, 33, 25))
df2 <- tibble(time = as.numeric(rep(seq(1,5), each = 5)),
       value = runif(25, 10, 100),
       group = rep(LETTERS[1:5], times = 5))
mpg <- mpg
iris <- iris
econ <- economics_long

# Alle Farben ansehen (inkl. Farbcode)
seecol()

# Linien ----
ggplot(kof,
  aes(x = time, y = value, colour = branche)) +
  geom_line(size = 1.5) +  # Liniendiagramm, Dicke der Linie
  scale_colour_manual(
    values = usecol(pal = pal_sgb_pref,
                    n = nlevels(as.factor(kof$branche))),
    guide = guide_legend(nrow = 1)) +
  geom_hline(yintercept = 0, size = 1) + # dickere Linie bei 0
  theme_sgb(base_size = 12) +
  theme(axis.title.y = element_blank())

ggsave("docs/lineplot.jpeg", width = 15.5, height = 7,
       units = "cm", dpi = "retina")

# Linien 4 vars----
emf(file = "docs/lineplot2.emf", width = 6.2, height = 2.75)
ggplot(mangelindikator,
       aes(x = time, y = value, colour = label)) +
  geom_line(size = 1.2) +  # Liniendiagramm, Dicke der Linie
  scale_colour_manual(
    values = usecol(pal = pal_sgb_pref,
                    n = nlevels(as.factor(mangelindikator$label))),
    guide = guide_legend(nrow = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_sgb(base_size = 8) +
  theme(axis.title.y = element_blank())
dev.off()
# ggsave("docs/lineplot2.jpeg", width = 30, height = 14,
#        units = "cm", dpi = "retina")


# Linien 5 vars----
emf(file = "docs/lineplot3.emf", width = 6.2, height = 2.75)
ggplot(econ, aes(date, value01, colour = variable)) +
  geom_line(size = 1.2) +
  scale_colour_manual(
    values = usecol(pal = pal_sgb_pair,
                    n = nlevels(as.factor(econ$variable))),
    guide = guide_legend(nrow = 1)) +
  theme_sgb(base_size = 8) +
  theme(axis.title.y = element_blank())
dev.off()
# ggsave("docs/lineplot3.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Saeulen (bar) ----
emf(file = "docs/barplot1.emf", width = 6.2, height = 2.75)
ggplot(mpg, aes(as.factor(year), fill = drv)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = usecol(pal = pal_sgb_pref,
                    n = nlevels(as.factor(mpg$drv)))) +
  theme_sgb(base_size = 8) +
  theme(axis.title.y = element_blank())
dev.off()
library(ggplot2)
econ
# ggsave("docs/barplot1.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Saeulen gestapelt (bar) ----
emf(file = "docs/barplot2.emf", width = 6.2, height = 2.75)
ggplot(mpg, aes(manufacturer, fill = fl)) +
  geom_bar(position = "stack") +
  scale_fill_manual(
    values = usecol(pal = rev(c(pal_sgb_pref)), n = 5)) +
  theme_sgb(base_size = 8) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# ggsave("docs/barplot2.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Kuchen
df <- df %>%
  arrange(desc(group)) %>%
  mutate(prop = value / sum(df$value) * 100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(df, aes(x = "", y = prop, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = percent(prop / 100)),
            color = "white",
            size = 5) +
  scale_fill_manual(values = usecol(pal = pal_sgb_pref, n = 3)) +
  theme_void() +
  theme(legend.title = element_blank())

# ggsave("docs/pieplot.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")


# Gestapeltes Flächendiagramm
emf(file = "docs/areaplot.emf", width = 6.2, height = 2.75)
ggplot(df2, aes(x = time, y = value, fill = group)) +
  geom_area() +
  scale_fill_manual(values = usecol(pal = pal_sgb_rot,
                                    n = nlevels(as.factor(df2$group)))) +
  theme_sgb(base_size = 8) +
  theme(axis.title.y = element_blank())
dev.off()

# ggsave("docs/areaplot.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Punkte discrete
emf(file = "docs/pointplot1.emf", width = 6.2, height = 2.75)
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 2) +
  theme_sgb(base_size = 8) +
  scale_color_manual(values = usecol(pal = pal_sgb_pref,
                                     n = nlevels(as.factor(iris$Species)))) +
  theme(axis.title.y = element_blank(),
        legend.position = "top")
dev.off()

# ggsave("docs/pointplot1.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Punkte continous
emf(file = "docs/pointplot2.emf", width = 6.2, height = 2.75)
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 2) +
  theme_sgb(base_size = 8) +
  scale_color_gradientn(colours = usecol(pal = pal_sgb_rot)) +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.box.background = element_blank())
dev.off()

# ggsave("docs/pointplot2.jpeg", width = 15.5, height = 7,
#        units = "cm", dpi = "retina")

# Alle Farbpaletten
pdf("docs/allcolors.pdf", width = 10, height = 7)
seecol()
seecol(pal_sgb)
# seecol(pal_sgb_web)
# seecol(pal_sgb_ppt)
seecol(pal_sgb_pair)
seecol(pal_sgb_pref)
seecol(pal_sgb_rot)
seecol(pal_sgb_gelb)
seecol(pal_sgb_dunkelblau)
seecol(pal_sgb_hellblau)
# seecol(pal_sgb_grau)
dev.off()




