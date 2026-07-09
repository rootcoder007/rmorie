# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Reproducible generator for man/figures/logo.png (the rmorie hex sticker).
#
#   Rscript data-raw/hex_logo.R
#
# Pure ggplot2 + showtext (no magick/hexSticker system deps). Standard
# hex-sticker canvas: 518 x 600 px, pointy-top hexagon. Design follows the
# classic CRAN sticker language (light face, thin accent border, a central
# plot motif): a Hawkes-style monthly-count series with its dashed
# background-rate line over a residual histogram -- morie's signature
# point-process diagnostics. Palette: GNOME Adwaita (blue #3584e4,
# orange #ff7800, green #26a269, dark #241f31).

library(ggplot2)
library(showtext)

sysfonts::font_add_google("Cantarell", "cantarell")
showtext_auto()
showtext_opts(dpi = 300)

# --- hexagon (pointy-top, radius 1, generous margins so no edge clips) ----
ang <- seq(30, 330, by = 60) * pi / 180  # six vertices, pointy-top
hex <- data.frame(x = cos(ang), y = sin(ang))

# --- top motif: Hawkes-like monthly counts + dashed baseline --------------
set.seed(7)
n <- 60
counts <- pmax(0.5, 4 + arima.sim(list(ar = 0.35), n) * 1.6 +
                 ifelse(seq_len(n) %in% c(14, 15, 33, 34, 35, 48), 5.5, 0))
ts_x <- seq(-0.60, 0.60, length.out = n)
ts_y <- -0.14 + 0.62 * (counts - min(counts)) / diff(range(counts))
series <- data.frame(x = ts_x, y = ts_y)
baseline_y <- -0.14 + 0.62 * (4 - min(counts)) / diff(range(counts))


g <- ggplot() +
  geom_polygon(
    data = hex, aes(x, y),
    fill = "#fcfcfc", colour = "#26a269", linewidth = 2.0
  ) +
  # top plot: L-axis, dashed baseline, spiky series
  annotate("segment", x = -0.66, xend = -0.66, y = -0.20, yend = 0.55,
           colour = "#241f31", linewidth = 0.35) +
  annotate("segment", x = -0.66, xend = 0.66, y = -0.20, yend = -0.20,
           colour = "#241f31", linewidth = 0.35) +
  annotate("segment", x = -0.60, xend = 0.60,
           y = baseline_y, yend = baseline_y,
           colour = "#ff7800", linewidth = 0.5, linetype = "22") +
  geom_line(data = series, aes(x, y), colour = "#3584e4", linewidth = 0.55) +
  # wordmark
  annotate("text", 0, -0.52, label = "rmorie",
           family = "cantarell", fontface = "bold",
           size = 5.8, colour = "#241f31") +
  coord_fixed(xlim = c(-1.045, 1.045), ylim = c(-1.2103, 1.2103),
              expand = FALSE) +
  theme_void()

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("man/figures/logo.png", g,
       width = 518 / 300, height = 600 / 300, dpi = 300, bg = "transparent")
cat("wrote man/figures/logo.png\n")
