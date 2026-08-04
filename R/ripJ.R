# R arm of morie/fn/ripJ.py -- the J function of van Lieshout & Baddeley.
#
# The Python body was a placeholder: it averaged the leading `points`
# argument and used neither `window` nor `r`. There was no R arm at all.
#
#   J(r) = (1 - G(r)) / (1 - F(r))
#
# G is the event-to-event nearest neighbour CDF and F the point-to-event
# empty space CDF. Under CSR the two coincide, so J = 1; J < 1 indicates
# clustering and J > 1 regularity. Both factors carry the same CSR form, so
# the ratio cancels the intensity.
#
# J is undefined once F(r) = 1, that is beyond the largest empty-space
# distance in the sample; those entries are NA rather than a
# division-by-zero infinity.
#
# van Lieshout & Baddeley (1996), Statistica Neerlandica 50(3):344-361,
# doi:10.1111/j.1467-9574.1996.tb01501.x. Baddeley & Turner (2005) JSS
# 12(6) p. 16 states J = (1 - G)/(1 - F) and p. 17 attributes it to
# van Lieshout & Baddeley (1996).

#' @noRd
morie_ripley_j_function <- function(points, window = NULL, r = NULL,
                                    n_grid = 40) {
  p <- as.matrix(points)
  reg <- .sp_region(window, p)
  if (is.null(r)) {
    nn <- .sp_nn(p)
    if (length(nn) == 0L) {
      stop("at least two events are needed for the J function", call. = FALSE)
    }
    r <- seq(0, max(nn), length.out = 25)
  }
  r <- as.numeric(r)

  gr <- spgfun(p, r, reg)
  fr <- spffun(p, reg, r, n_grid)
  g <- as.numeric(gr$g)
  f <- as.numeric(fr$f)

  denom <- 1 - f
  j <- ifelse(denom <= 0, NA_real_, (1 - g) / denom)
  n_defined <- sum(!is.na(j))

  list(r = r, j = j, g = g, f = f, j_csr = rep(1, length(r)),
       lambda_est = gr$lambda_est, n_defined = n_defined,
       method = "J function, (1 - G(r)) / (1 - F(r))")
}

#' @noRd
RipJ <- morie_ripley_j_function
