# R arm of morie/fn/empfun.py -- the empty space function F(r).
#
# The Python body was a placeholder: it averaged `coords` and used neither
# `r_grid` nor `window`. There was no R arm at all.
#
# F(r) is the CDF of the distance from an ARBITRARY location to the nearest
# event (the contact or point-to-event distribution), estimated from a
# deterministic n_grid x n_grid lattice of sample locations over the window,
# so repeated calls return the same numbers. Under CSR
#
#   F(r) = 1 - exp(-lambda pi r^2)
#
# the same form as G, but the two move oppositely under departures:
# clustering leaves large empty gaps, so F-hat falls BELOW the CSR curve
# while G-hat rises above it.
#
# Baddeley & Turner (2005) JSS 12(6) p. 16: "F(r), the empty space function
# (contact distribution or 'point-to-event' distribution)". Schabenberger &
# Gotway (2005) sec. 3.3.4, pp. 97-98.

#' @noRd
morie_empty_space_function <- function(coords, r_grid = NULL, window = NULL,
                                       n_grid = 40) {
  res <- spffun(coords, window, r_grid, n_grid)
  d <- res$empty_space_distances
  list(r = res$r, f = res$f, f_csr = res$f_csr, empty_space_distances = d,
       lambda_est = res$lambda_est, n_sample = length(d),
       method = "Empty space function F(r) on a fixed lattice of sample locations")
}

#' @noRd
Empfun <- morie_empty_space_function
