# Bet-proofness. Muller & Norets (2016) Econometrica 84(6), 2183-2213.
# A valid confidence set can be empty or arbitrarily short in
# nonstandard problems; bet-proofness rules out a recognisable subset
# where coverage drops below the nominal level.

.bndpcb_GHC_EPS <- 1e-12

morie_truncated_normal_interval <- function(x, level = 0.95,
                                             lower_bound = 0) {
  z <- qnorm(0.5 + as.numeric(level) / 2)
  lo <- max(as.numeric(x) - z, as.numeric(lower_bound))
  hi <- as.numeric(x) + z
  empty <- hi < as.numeric(lower_bound)
  list(lower = lo, upper = if (empty) lo else hi,
       width = max(hi - lo, 0), empty = empty, z = z,
       level = as.numeric(level),
       note = "empty when x + z < the parameter bound; a valid set that describes nothing")
}

morie_coverage_by_region <- function(theta, level = 0.95, lower_bound = 0,
                                     draws = 20000L, seed = 0,
                                     split = NULL) {
  e <- .ghc_rng(seed)
  th <- as.numeric(theta)
  cut <- if (is.null(split)) as.numeric(lower_bound) else as.numeric(split)
  tot <- 0; cov <- 0
  stot <- 0; scov <- 0
  widths <- numeric()
  n <- as.integer(draws)
  z <- qnorm(0.5 + level / 2)
  xs <- .ghc_norm(e, n)
  for (i in seq_len(n)) {
    x <- th + xs[i]
    lo <- max(x - z, as.numeric(lower_bound))
    hi <- x + z
    empty <- hi < as.numeric(lower_bound)
    ok <- !empty && lo <= th && th <= hi
    tot <- tot + 1
    cov <- cov + as.integer(ok)
    widths <- c(widths, max(hi - lo, 0))
    if (x < cut) { stot <- stot + 1; scov <- scov + as.integer(ok) }
  }
  list(marginal_coverage = cov / tot,
       subset_coverage = if (stot > 0) scov / stot else NaN,
       subset_share = stot / tot,
       mean_width = mean(widths),
       p_empty = sum(widths <= .bndpcb_GHC_EPS) / length(widths),
       split = cut, theta = th, draws = n)
}

morie_bet_violation <- function(theta, level = 0.95, lower_bound = 0,
                                draws = 20000L, seed = 0, grid = NULL) {
  cuts <- if (is.null(grid))
    seq(as.numeric(lower_bound) - 3, as.numeric(lower_bound) - 3 + 0.25 * 24,
        by = 0.25)
  else as.numeric(grid)
  worst <- list(shortfall = 0, cut = NULL, coverage = NULL, share = 0)
  for (c_ in cuts) {
    r <- morie_coverage_by_region(theta, level, lower_bound, draws, seed,
                                  split = c_)
    if (r$subset_share < 0.01) next
    if (is.nan(r$subset_coverage)) next
    short <- as.numeric(level) - r$subset_coverage
    if (short > worst$shortfall)
      worst <- list(shortfall = short, cut = c_,
                    coverage = r$subset_coverage,
                    share = r$subset_share)
  }
  list(max_shortfall = worst$shortfall, at_cut = worst$cut,
       subset_coverage = worst$coverage, subset_share = worst$share,
       bet_proof = worst$shortfall <= 0.02, level = as.numeric(level),
       note = "a positive shortfall on a subset the analyst can SEE is a bettable edge; marginal validity does not rule it out")
}

morie_bet_proof_interval <- function(x, level = 0.95, lower_bound = 0,
                                     min_width = NULL) {
  z <- qnorm(0.5 + as.numeric(level) / 2)
  w <- if (is.null(min_width)) z else as.numeric(min_width)
  if (w <= 0) stop("bndpcb: min_width must be positive")
  naive <- morie_truncated_normal_interval(x, level, lower_bound)
  lo <- max(as.numeric(x) - z, as.numeric(lower_bound))
  hi <- max(as.numeric(x) + z, as.numeric(lower_bound) + w)
  if (hi - lo < w) hi <- lo + w
  list(lower = lo, upper = hi, width = hi - lo, empty = FALSE,
       min_width = w, naive_width = naive$width,
       naive_empty = naive$empty,
       widened = (hi - lo) > naive$width + 1e-12,
       method = "bet-proof by construction: never empty, never shorter than the floor (Muller & Norets 2016 Sec. 1)")
}

morie_pseudobayescredible <- morie_bet_proof_interval
morie_bound_pseudo_credible <- morie_bet_proof_interval

# house entry point: the package exports one morie_<module>
morie_bndpcb <- morie_bet_proof_interval
