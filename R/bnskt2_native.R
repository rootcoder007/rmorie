# bnskt2_native.R
# Base R port of the regression kink design (Card, Lee, Pei & Weber
# 2015, Econometrica 83(6), 2453-2483): a ratio of kinks, local
# polynomial regression on each side, smooth-density condition and
# covariate sanity check.

.EPS <- 1e-12

# Weighted least squares (no intercept; rows are powers of d from 1).
.wls <- function(rows, ys, w) {
  X <- do.call(rbind, rows)
  y <- as.numeric(ys); w <- sqrt(pmax(as.numeric(w), 0.0))
  Xw <- X * w; yw <- y * w
  coef <- as.numeric(solve(crossprod(Xw), crossprod(Xw, yw)))
  list(coef = coef)
}

.side_fit <- function(v, y, k_pt, bandwidth, order, side, kernel) {
  rows <- list(); ys <- c(); ws <- c()
  for (i in seq_along(v)) {
    d <- as.numeric(v[i]) - as.numeric(k_pt)
    if (side == "right" && d < 0) next
    if (side == "left"  && d > 0) next
    if (abs(d) > as.numeric(bandwidth)) next
    u <- abs(d) / as.numeric(bandwidth)
    kw <- if (kernel == "triangular") (1.0 - u) else 1.0
    if (kw <= 0.0) next
    rows[[length(rows) + 1L]] <- sapply(seq_len(as.integer(order)),
                                       function(p) d ^ p)
    ys <- c(ys, as.numeric(y[i]))
    ws <- c(ws, kw)
  }
  if (length(rows) < as.integer(order) + 1L)
    stop(sprintf("bnskt2: too few observations on the %s of the kink within the bandwidth (%d for order %d)",
                 side, length(rows), order))
  fit <- .wls(rows, ys, ws)
  list(slope = fit$coef[1L], coef = fit$coef, n = length(rows))
}

local_polynomial_slope <- function(v, y, kink, bandwidth, order = 2L,
                                   side = "right", kernel = "triangular") {
  if (!(side %in% c("left", "right")))
    stop(sprintf("bnskt2: side must be left or right, got %r", side))
  if (!(kernel %in% c("triangular", "uniform")))
    stop(sprintf("bnskt2: kernel must be triangular or uniform, got %r", kernel))
  if (as.integer(order) < 1L)
    stop("bnskt2: the polynomial order must be at least 1")
  if (as.numeric(bandwidth) <= 0.0)
    stop("bnskt2: the bandwidth must be positive")
  .side_fit(v, y, kink, bandwidth, as.integer(order), side, kernel)
}

rkd_estimate <- function(V, Y, kink, bandwidth, order = 2L,
                         kernel = "triangular",
                         policy_slope_change = NULL, B = NULL,
                         fuzzy = FALSE) {
  v <- as.numeric(V); y <- as.numeric(Y)
  if (length(v) != length(y))
    stop(sprintf("bnskt2: V and Y must agree in length (%d, %d)", length(v), length(y)))
  r <- .side_fit(v, y, kink, bandwidth, as.integer(order), "right", kernel)
  l <- .side_fit(v, y, kink, bandwidth, as.integer(order), "left",  kernel)
  num <- r$slope - l$slope
  if (isTRUE(fuzzy)) {
    if (is.null(B)) stop("bnskt2: fuzzy RKD needs the observed treatment B")
    b <- as.numeric(B)
    if (length(b) != length(v))
      stop(sprintf("bnskt2: B has %d entries for %d observations", length(b), length(v)))
    rb <- .side_fit(v, b, kink, bandwidth, as.integer(order), "right", kernel)
    lb <- .side_fit(v, b, kink, bandwidth, as.integer(order), "left",  kernel)
    den <- rb$slope - lb$slope
    den_src <- "estimated from observed treatment"
  } else {
    if (is.null(policy_slope_change))
      stop("bnskt2: sharp RKD needs policy_slope_change, the known change in the slope of the policy rule")
    den <- as.numeric(policy_slope_change)
    den_src <- "known policy rule"
  }
  if (abs(den) <= .EPS)
    stop(sprintf("bnskt2: the change in the policy slope is zero (%.3g) -- there is no kink to identify from", den))
  list(estimate = num / den, tau = num / den,
       outcome_kink = num, policy_kink = den,
       slope_right = r$slope, slope_left = l$slope,
       n_right = r$n, n_left = l$n,
       bandwidth = as.numeric(bandwidth), order = as.integer(order),
       kernel = kernel, fuzzy = isTRUE(fuzzy),
       denominator_source = den_src,
       method = "regression kink design; Card, Lee, Pei & Weber (NBER WP 18564 / Econometrica 2015)",
       requires = "the density of V must be smooth at the kink -- test it")
}

density_kink_test <- function(V, kink, bandwidth, n_bins = 20L, order = 1L) {
  v <- as.numeric(V); kp <- as.numeric(kink); bw <- as.numeric(bandwidth)
  inside <- v[abs(v - kp) <= bw]
  if (length(inside) < 4L * as.integer(n_bins))
    stop(sprintf("bnskt2: too few observations within the bandwidth for %d bins", as.integer(n_bins)))
  edges <- kp - bw + 2.0 * bw * (0:as.integer(n_bins)) / as.integer(n_bins)
  ctr <- numeric(as.integer(n_bins)); dens <- numeric(as.integer(n_bins))
  for (b in seq_len(as.integer(n_bins))) {
    ctr[b]  <- 0.5 * (edges[b] + edges[b + 1L])
    dens[b] <- sum(inside >= edges[b] & inside < edges[b + 1L]) / length(inside)
  }
  right <- .side_fit(ctr, dens, kp, bw, as.integer(order), "right", "uniform")
  left  <- .side_fit(ctr, dens, kp, bw, as.integer(order), "left",  "uniform")
  change <- right$slope - left$slope
  scale <- max(sum(dens) / length(dens), .EPS)
  list(slope_change = change, relative = change / scale,
       slope_right = right$slope, slope_left = left$slope,
       n_inside = length(inside), n_bins = as.integer(n_bins),
       smooth = abs(change / scale) < 1.0,
       interpretation = "a kink in the DENSITY suggests precise manipulation of the assignment variable, which invalidates the design")
}

covariate_kink_test <- function(V, Z, kink, bandwidth, order = 2L,
                                kernel = "triangular") {
  r <- .side_fit(V, Z, kink, bandwidth, as.integer(order), "right", kernel)
  l <- .side_fit(V, Z, kink, bandwidth, as.integer(order), "left",  kernel)
  list(slope_change = r$slope - l$slope,
       slope_right = r$slope, slope_left = l$slope,
       n_right = r$n, n_left = l$n,
       interpretation = "a kink here is evidence the design is picking up composition rather than the policy")
}

.bnskt2_cheatsheet <- function() {
  paste0(
    "bnskt2: regression KINK design. RD uses a JUMP in treatment; ",
    "RKD uses a change in SLOPE -- benefits rising with earnings up ",
    "to a cap, then flat. tau = (change in the slope of E[Y|V]) / ",
    "(change in the slope of the policy). The denominator is usually ",
    "KNOWN from legislation, so the first stage is not estimated. ",
    "Needs the density of V SMOOTH at the kink -- precise ",
    "manipulation bends it, and then composition is mistaken for ",
    "policy. More bandwidth-sensitive than RD because a DERIVATIVE ",
    "is being estimated."
  )
}

kinktreatmentbound <- rkd_estimate
bound_kink_te      <- rkd_estimate
boundkinkte        <- rkd_estimate

# house entry point: the package exports one morie_<module>
morie_bnskt2 <- rkd_estimate
