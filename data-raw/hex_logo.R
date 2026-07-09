# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Reproducible generator for man/figures/logo.png (the rmorie hex sticker).
#
#   Rscript data-raw/hex_logo.R
#
# Pure ggplot2 + showtext (no magick/hexSticker system deps). Standard
# hex-sticker canvas: 518 x 600 px, pointy-top hexagon (aspect 2/sqrt(3)).
# Palette: GNOME Adwaita -- dark #241f31, green #26a269 (morie 森 = forest),
# light #ffffff, dim #9a9996.

library(ggplot2)
library(showtext)

sysfonts::font_add_google("Noto Sans JP", "notojp")
sysfonts::font_add_google("Cantarell", "cantarell")
showtext_auto()
showtext_opts(dpi = 300)

ang <- seq(90, 330, by = 60) * pi / 180
hex <- data.frame(x = cos(ang), y = sin(ang))

g <- ggplot() +
  geom_polygon(
    data = hex, aes(x, y),
    fill = "#241f31", colour = "#26a269", linewidth = 2.4
  ) +
  annotate("text", 0, 0.30, label = "森",
           family = "notojp", size = 15, colour = "#26a269") +
  annotate("text", 0, -0.38, label = "rmorie",
           family = "cantarell", fontface = "bold",
           size = 8, colour = "#ffffff") +
  annotate("text", 0, -0.62, label = "rootcoder007.r-universe.dev",
           family = "cantarell", size = 2.1, colour = "#9a9996") +
  coord_fixed(xlim = c(-0.95, 0.95), ylim = c(-1.10, 1.10), expand = FALSE) +
  theme_void()

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("man/figures/logo.png", g,
       width = 518 / 300, height = 600 / 300, dpi = 300, bg = "transparent")
cat("wrote man/figures/logo.png\n")
