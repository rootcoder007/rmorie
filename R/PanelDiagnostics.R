.pd_groups <- function(unit) match(unit, unique(unit))

.pd_demean <- function(v, idx) v - (rowsum(v, idx, reorder = FALSE) / tabulate(idx))[idx]

.pd_ols <- function(X, y) {
  beta <- as.vector(solve(crossprod(X), crossprod(X, y)))
  fit <- as.vector(X %*% beta)
  list(beta = beta, res = y - fit, fit = fit)
}

#' Panel-data diagnostics
#'
#' \code{PanelWithin}: within transformation. \code{PanelResiduals}: pooled,
#' within or unit-by-unit OLS residuals. \code{PanelVarianceComponents}:
#' quadratic-form (Wallace-Hussain or Amemiya) error-component variances for a
#' balanced panel. \code{CrossSectionDependence}: Pesaran CD, Breusch-Pagan LM,
#' scaled and bias-corrected scaled LM tests and mean (absolute) residual
#' correlations. \code{UnobservedEffectsTest}: Wooldridge's test for unobserved
#' effects. \code{BaltagiLiTest}: Baltagi-Li LM test for AR(1)/MA(1) errors in
#' the random-effects model (maximum likelihood by the Breusch iteration).
#' \code{PanelSerialTest}: Breusch-Godfrey/Wooldridge test on within
#' residuals. \code{ConleyVcov}: Conley spatial HAC covariance of OLS
#' coefficients (uniform or Bartlett kernel on haversine distances, or the
#' \code{fixest} approximations). Rows must be sorted by unit and time.
#' Identical to the Python arm \code{morie.fn.paneldiag}.
#'
#' @param x Vector or matrix to demean.
#' @param unit Unit identifiers.
#' @param time Period identifiers.
#' @param y Response vector.
#' @param X Regressor matrix (without intercept).
#' @param model \code{"pooling"}, \code{"within"} or \code{"heterogeneous"}.
#' @param resid Residual vector.
#' @param df_correction Degrees of freedom subtracted from \code{N(T-1)}.
#' @param test \code{"cd"}, \code{"lm"}, \code{"sclm"}, \code{"bcsclm"},
#'   \code{"rho"} or \code{"absrho"}.
#' @param w Optional proximity matrix restricting the unit pairs.
#' @param alternative \code{"twosided"} or \code{"onesided"}.
#' @param order Serial-correlation order (default: shortest unit length).
#' @param type \code{"Chisq"} or \code{"F"}.
#' @param lat,lon Coordinates in decimal degrees.
#' @param cutoff Distance cutoff in km.
#' @param kernel \code{"uniform"} or \code{"bartlett"}.
#' @param distance \code{"haversine"}, \code{"fixest_triangular"} or
#'   \code{"fixest_spherical"}.
#' @param intercept Add an intercept column to \code{X}.
#' @param adjust Multiply by \code{n / (n - k)}.
#' @return A vector, matrix or list of statistics.
#' @references Pesaran, M. H. (2004). General diagnostic tests for cross
#'   section dependence in panels. CESifo Working Paper 1229.
#'
#'   Baltagi, B. H. and Li, Q. (1995). Testing AR(1) against MA(1)
#'   disturbances in an error component model. Journal of Econometrics 68,
#'   133-151.
#'
#'   Wooldridge, J. M. (2010). Econometric Analysis of Cross Section and Panel
#'   Data, 2nd edn. MIT Press.
#'
#'   Conley, T. G. (1999). GMM estimation with cross sectional dependence.
#'   Journal of Econometrics 92, 1-45.
#' @examples
#' PanelWithin(c(1, 3, 2, 6), c("a", "a", "b", "b"))
#' e <- c(1, -1, 2, 1, -1, 3, -2, 1, 0)
#' CrossSectionDependence(e, rep(1:3, each = 3), rep(1:3, 3))$statistic
#' @export
PanelWithin <- function(x, unit) {
  idx <- .pd_groups(unit)
  if (is.matrix(x)) return(apply(x, 2, .pd_demean, idx = idx))
  .pd_demean(as.numeric(x), idx)
}

#' @rdname PanelWithin
#' @export
PanelResiduals <- function(y, X, unit, model = "pooling") {
  X <- as.matrix(X)
  idx <- .pd_groups(unit)
  if (model == "pooling") {
    f <- .pd_ols(cbind(1, X), y)
    return(list(residuals = f$res, coefficients = f$beta, model = model))
  }
  if (model == "within") {
    D <- apply(X, 2, .pd_demean, idx = idx)
    D <- D[, apply(abs(D), 2, max) > 0, drop = FALSE]
    f <- .pd_ols(D, .pd_demean(y, idx))
    return(list(residuals = f$res, coefficients = f$beta, model = model))
  }
  if (model == "heterogeneous") {
    res <- numeric(length(y))
    beta <- list()
    for (u in seq_len(max(idx))) {
      rows <- which(idx == u)
      f <- .pd_ols(cbind(1, X[rows, , drop = FALSE]), y[rows])
      res[rows] <- f$res
      beta[[u]] <- f$beta
    }
    return(list(residuals = res, coefficients = beta, model = model))
  }
  stop("model must be 'pooling', 'within' or 'heterogeneous'")
}

#' @rdname PanelWithin
#' @export
PanelVarianceComponents <- function(resid, unit, df_correction = 0) {
  idx <- .pd_groups(unit)
  cnt <- tabulate(idx)
  if (min(cnt) != max(cnt)) stop("PanelVarianceComponents needs a balanced panel")
  g <- length(cnt)
  tt <- cnt[1]
  m <- as.vector(rowsum(resid, idx, reorder = FALSE)) / cnt
  s2e <- sum((resid - m[idx])^2) / (g * (tt - 1) - df_correction)
  s21 <- tt * sum(m^2) / g
  list(sigma2_idios = s2e, sigma2_1 = s21, sigma2_id = (s21 - s2e) / tt,
       theta = 1 - sqrt(s2e / s21), n_units = g, n_periods = tt)
}

#' @rdname PanelWithin
#' @export
CrossSectionDependence <- function(resid, unit, time, test = "cd", w = NULL) {
  idx <- .pd_groups(unit)
  n <- max(idx)
  tt <- vector("list", n)
  ee <- vector("list", n)
  for (g in seq_len(n)) {
    tt[[g]] <- time[idx == g]
    ee[[g]] <- resid[idx == g]
  }
  tij <- numeric(0)
  rho <- numeric(0)
  tmax <- 0
  for (j in seq_len(n)) {
    for (i in seq_len(n)) {
      if (i <= j) next
      if (!is.null(w) && w[i, j] == 0) next
      common <- tt[[i]][tt[[i]] %in% tt[[j]]]
      nt <- length(common)
      tmax <- max(tmax, nt)
      if (nt <= 1) next
      a <- ee[[i]][match(common, tt[[i]])]
      b <- ee[[j]][match(common, tt[[j]])]
      a <- a - sum(a) / nt
      b <- b - sum(b) / nt
      tij <- c(tij, nt)
      rho <- c(rho, sum(a * b) / sqrt(sum(a^2) * sum(b^2)))
    }
  }
  m <- length(rho)
  if (m == 0) stop("no pair of units with more than one common period")
  p <- NULL
  if (test == "cd") {
    stat <- sqrt(1 / m) * sum(sqrt(tij) * rho)
    p <- 2 * stats::pnorm(-abs(stat))
  } else if (test == "lm") {
    stat <- sum(tij * rho^2)
    p <- stats::pchisq(stat, m, lower.tail = FALSE)
  } else if (test %in% c("sclm", "bcsclm")) {
    stat <- sqrt(1 / (2 * m)) * sum(tij * rho^2 - 1)
    if (test == "bcsclm") stat <- stat - n / (2 * (tmax - 1))
    p <- 2 * stats::pnorm(-abs(stat))
  } else if (test == "rho") {
    stat <- sum(rho) / m
  } else if (test == "absrho") {
    stat <- sum(abs(rho)) / m
  } else {
    stop("test must be one of cd, lm, sclm, bcsclm, rho, absrho")
  }
  list(statistic = stat, p_value = p, n_pairs = m, test = test)
}

#' @rdname PanelWithin
#' @export
UnobservedEffectsTest <- function(resid, unit) {
  idx <- .pd_groups(unit)
  s <- vapply(seq_len(max(idx)), function(g) {
    v <- resid[idx == g]
    o <- outer(v, v)
    sum(o[upper.tri(o)])
  }, numeric(1))
  z <- sum(s) / sqrt(sum(s^2))
  list(statistic = z, p_value = 2 * stats::pnorm(-abs(z)), S = s)
}

.pd_re_ml <- function(y, X, idx, tt, tol = 1e-14, maxit = 10000) {
  cnt <- tabulate(idx)
  phi2 <- 1
  for (it in seq_len(maxit)) {
    theta <- 1 - sqrt(phi2)
    my <- as.vector(rowsum(y, idx, reorder = FALSE)) / cnt
    mx <- rowsum(X, idx, reorder = FALSE) / cnt
    f <- .pd_ols(X - theta * mx[idx, , drop = FALSE], y - theta * my[idx])
    d <- as.vector(y - X %*% f$beta)
    md <- as.vector(rowsum(d, idx, reorder = FALSE)) / cnt
    q <- sum((d - md[idx])^2)
    pq <- tt * sum(md^2)
    new <- min(1, q / ((tt - 1) * pq))
    done <- abs(new - phi2) <= tol * phi2
    phi2 <- new
    if (done) break
  }
  list(beta = f$beta, d = d, phi2 = phi2)
}

#' @rdname PanelWithin
#' @export
BaltagiLiTest <- function(y, X, unit, alternative = "twosided") {
  X <- cbind(1, as.matrix(X))
  idx <- .pd_groups(unit)
  cnt <- tabulate(idx)
  if (min(cnt) != max(cnt)) stop("BaltagiLiTest needs a balanced panel")
  n <- length(cnt)
  tt <- cnt[1]
  fit <- .pd_re_ml(y, X, idx, tt)
  ui <- lapply(seq_len(n), function(g) fit$d[idx == g])
  s2e <- sum(vapply(ui, function(r) sum((r - sum(r) / tt)^2), numeric(1))) / (n * (tt - 1))
  s21 <- sum(vapply(ui, function(r) tt * (sum(r) / tt)^2, numeric(1))) / n
  A <- matrix(1 / (tt * s21), tt, tt) + (diag(tt) - matrix(1 / tt, tt, tt)) / s2e
  G <- matrix(0, tt, tt)
  G[abs(row(G) - col(G)) == 1] <- 1
  S <- A %*% G %*% A
  star2 <- sum(vapply(ui, function(r) sum(r * (S %*% r)), numeric(1)))
  D <- (n * (tt - 1) / tt) * (s21 - s2e) / s21 + s2e / 2 * star2
  a <- (s2e - s21) / (tt * s21)
  j_rr <- n * (2 * a^2 * (tt - 1)^2 + 2 * a * (2 * tt - 3) + (tt - 1))
  j12 <- n * (tt - 1) * s2e / s21^2
  j13 <- n * (tt - 1) / tt * s2e * (1 / s21^2 - 1 / s2e^2)
  j22 <- n * tt^2 / (2 * s21^2)
  j23 <- n * tt / (2 * s21^2)
  j33 <- (n / 2) * (1 / s21^2 + (tt - 1) / s2e^2)
  det <- j_rr * (j22 * j33 - j23 * j23) - j12 * (j12 * j33 - j23 * j13) + j13 * (j12 * j23 - j22 * j13)
  J11 <- n^2 * tt^2 * (tt - 1) / (det * 4 * s21^2 * s2e^2)
  if (alternative == "onesided") {
    stat <- D * sqrt(J11)
    p <- stats::pnorm(stat, lower.tail = FALSE)
  } else if (alternative == "twosided") {
    stat <- D^2 * J11
    p <- stats::pchisq(stat, 1, lower.tail = FALSE)
  } else {
    stop("alternative must be 'twosided' or 'onesided'")
  }
  list(statistic = stat, p_value = p, alternative = alternative, coefficients = fit$beta,
       sigma2_e = s2e, sigma2_1 = s21, phi2 = fit$phi2, D = D, J11 = J11)
}

#' @rdname PanelWithin
#' @export
PanelSerialTest <- function(y, X, unit, order = NULL, type = "Chisq") {
  X <- as.matrix(X)
  idx <- .pd_groups(unit)
  if (is.null(order)) order <- min(tabulate(idx))
  D <- apply(X, 2, .pd_demean, idx = idx)
  D <- D[, apply(abs(D), 2, max) > 0, drop = FALSE]
  n <- length(y)
  e <- .pd_ols(D, .pd_demean(y, idx))$res
  Z <- vapply(seq_len(order), function(L) c(rep(0, L), e[seq_len(n - L)]), numeric(n))
  aux <- .pd_ols(cbind(D, Z), e)
  k <- ncol(D)
  if (type == "Chisq") {
    stat <- n * sum(aux$fit^2) / sum(e^2)
    return(list(statistic = stat, p_value = stats::pchisq(stat, order, lower.tail = FALSE),
                df = order, order = order))
  }
  if (type == "F") {
    s0 <- sum(e^2)
    s1 <- sum(aux$res^2)
    stat <- ((s0 - s1) / order) / (s1 / (n - k - order))
    return(list(statistic = stat, p_value = stats::pf(stat, order, n - k - order, lower.tail = FALSE),
                df = c(order, n - k - order), order = order))
  }
  stop("type must be 'Chisq' or 'F'")
}

.pd_fx_pair <- function(la1, lo1, la2, lo2, cutoff, distance) {
  if (abs(la2 - la1) > cutoff / 111 * 3.14159 / 180) return(FALSE)
  dlon <- abs(lo2 - lo1)
  if (dlon >= 3.14159) dlon <- 6.28318 - dlon
  cm <- cos((la1 + la2) / 2)
  if (dlon > cutoff / 111 * 3.14159 / 180 / cm) return(FALSE)
  if (distance == "fixest_spherical") {
    a <- sin((la2 - la1) / 2)^2 + cos(la1) * cos(la2) * sin((lo2 - lo1) / 2)^2
    return(12752 * asin(min(1, sqrt(a))) <= cutoff)
  }
  (la2 - la1)^2 + (cm * dlon)^2 <= (cutoff * 3.14159 / 180 / 111)^2
}

#' @rdname PanelWithin
#' @export
ConleyVcov <- function(X, resid, lat, lon, cutoff, kernel = "uniform", distance = "haversine",
                       intercept = TRUE, adjust = TRUE) {
  X <- as.matrix(X)
  if (intercept) X <- cbind(1, X)
  la <- lat * pi / 180
  lo <- lon * pi / 180
  n <- nrow(X)
  k <- ncol(X)
  s <- X * resid
  meat <- crossprod(s)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (distance == "haversine") {
        a <- sin((la[j] - la[i]) / 2)^2 + cos(la[i]) * cos(la[j]) * sin((lo[j] - lo[i]) / 2)^2
        d <- 2 * 6371 * asin(min(1, sqrt(a)))
        if (kernel == "uniform") {
          wt <- as.numeric(d <= cutoff)
        } else if (kernel == "bartlett") {
          wt <- max(1 - d / cutoff, 0)
        } else {
          stop("kernel must be 'uniform' or 'bartlett'")
        }
      } else if (distance %in% c("fixest_triangular", "fixest_spherical")) {
        if (la[i] <= la[j]) {
          wt <- as.numeric(.pd_fx_pair(la[i], lo[i], la[j], lo[j], cutoff, distance))
        } else {
          wt <- as.numeric(.pd_fx_pair(la[j], lo[j], la[i], lo[i], cutoff, distance))
        }
      } else {
        stop("distance must be haversine, fixest_triangular or fixest_spherical")
      }
      if (wt != 0) meat <- meat + wt * (outer(s[i, ], s[j, ]) + outer(s[j, ], s[i, ]))
    }
  }
  B <- solve(crossprod(X))
  V <- B %*% meat %*% B
  if (adjust) V <- V * n / (n - k)
  dv <- diag(V)
  list(vcov = V, se = ifelse(dv >= 0, sqrt(abs(dv)), NaN), cutoff = cutoff)
}
