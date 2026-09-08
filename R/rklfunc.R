# R arm of morie/fn/rklfunc.py -- Besag's L function on bare coordinates.
#
# The Python body was a placeholder: it averaged `coords` and never used
# `r_grid`. There was no R arm at all.
#
#   L(r) = sqrt(K(r) / pi) - r
#
# The same transform as ripL, taking only coordinates: the window is the
# bounding box of `coords`. Prefer ripL when the real window is larger than
# the bounding box, which otherwise overstates the intensity and biases K
# downwards.
#
# Besag, J. (1977). Discussion of "Modelling spatial patterns" by B. D.
# Ripley. JRSS B 39(2):193-195. Baddeley & Turner (2005) JSS 12(6) p. 17.

#' @noRd
morie_ripley_l <- function(coords, r_grid = NULL, correction = "border") {
  res <- morie_ripley_l_function(coords, NULL, r_grid, correction)
  list(r = res$r, l = res$l, l_uncentred = res$l_uncentred, k = res$k,
       lambda_est = res$lambda_est,
       method = "Besag L function on the bounding box of the coordinates")
}

#' @noRd
Rklfunc <- morie_ripley_l
