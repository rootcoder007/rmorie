# causdidwd_native.R
#
# Wooldridge, J. M. (2025) "Two-way fixed effects, the two-way Mundlak
# regression, and difference-in-differences estimators", *Empirical
# Economics* 69, 2545-2587, doi:10.1007/s00181-025-02807-z.
#
# The paper's point is narrower and more useful than the slogan "TWFE
# is broken": TWFE is broken when applied to a model that is too
# restrictive, and there is nothing wrong with it applied to a
# sufficiently flexible one.  Sec. 3 proves the TWFE == two-way
# Mundlak (TWM) algebraic equivalence: pooled OLS with unit-specific
# time averages and period-specific cross-sectional averages
# reproduces the two-way fixed effects estimates **exactly**.  Sec. 4
# develops the two-step imputation estimator (fit on untreated obs,
# impute untreated potential outcome everywhere, average the residual
# in each (g, t) cell).  Sec. 5 proves imputation, pooled OLS on
# cohort dummies, random effects and TWFE are numerically identical
# on the flexible model.  Sec. 7 aggregates the cohort-time ATTs.
#
# The regression arm (`etwfe`) saturates the regression in treatment
# cohort by calendar period, so each coefficient **is** the
# ATT(g, t); nothing is averaged, so no negative weights arise.
#
# Native R implementation mirroring Python morie.fn.causdidwd exactly:
# same construction of the design (unit and period dummies, with the
# reference cell dropped from each set), same ridged least squares
# via .s03lstsq, same payload keys.

.EPS <- 1e-12

.causdidwd_panel <- function(Y, unit, period) {
  yv <- as.numeric(unlist(Y, use.names = FALSE))
  uv <- as.character(unlist(unit, use.names = FALSE))
  tv <- as.character(unlist(period, use.names = FALSE))
  n <- length(yv)
  if (!(length(uv) == length(tv) && length(uv) == n))
    stop(sprintf(paste("causdidwd: Y, unit and period must agree in",
                        "length (%d, %d, %d)"),
                 n, length(uv), length(tv)))
  if (n < 4L)
    stop(sprintf("causdidwd: need at least 4 observations, got %d", n))
  list(y = yv, u = uv, t = tv, n = n)
}

.causdidwd_unique_sorted <- function(v) {
  out <- character(0)
  seen <- character(0)
  for (x in v) {
    if (!(x %in% seen)) {
      seen <- c(seen, x)
      out <- c(out, x)
    }
  }
  out
}

.causdidwd_with_intercept <- function(rows) {
  cbind(1, rows)
}

#' Pooled OLS with a full set of unit and period dummies
#'
#' The reference cell is dropped from each set so the design has
#' full rank.  Returns the coefficients on \code{X} only -- the
#' fixed effects are nuisance.
#'
#' @param Y N-vector of outcomes.
#' @param unit,period N-vectors of panel identifiers.
#' @param X N-by-p matrix of covariates.
#' @return A list with \code{coef} (length-p numeric, no intercept),
#'   \code{full} (numeric vector of all fitted coefficients
#'   including the intercept, in order: intercept, X, unit dummies,
#'   period dummies), \code{n_units}, \code{n_periods},
#'   \code{n_columns}, \code{method}.
#' @references Wooldridge (2025) Sec. 3.
#' @export
morie_two_way_fixed_effects <- function(Y, unit, period, X) {
  p <- .causdidwd_panel(Y, unit, period)
  Xm <- .s03mat(X)
  if (nrow(Xm) != p$n)
    stop(sprintf("causdidwd: X has %d rows for %d observations",
                 nrow(Xm), p$n))
  us <- .causdidwd_unique_sorted(p$u)
  ts <- .causdidwd_unique_sorted(p$t)
  if (length(us) < 2L || length(ts) < 2L)
    stop(sprintf(paste("causdidwd: need at least 2 units and 2",
                       "periods, got %d and %d"),
                 length(us), length(ts)))
  ui <- match(p$u, us)            # 1-based
  ti <- match(p$t, ts)
  px <- ncol(Xm)
  rows <- matrix(0, p$n, px + length(us) - 1L + length(ts) - 1L)
  for (i in seq_len(p$n)) {
    d <- numeric(length(us) - 1L)
    if (ui[i] > 1L) d[ui[i] - 1L] <- 1
    f <- numeric(length(ts) - 1L)
    if (ti[i] > 1L) f[ti[i] - 1L] <- 1
    rows[i, ] <- c(Xm[i, ], d, f)
  }
  beta <- .s03lstsq(.causdidwd_with_intercept(rows), p$y, 1e-10)
  list(coef      = beta[seq_len(px) + 1L],
       full      = beta,
       n_units   = length(us),
       n_periods = length(ts),
       n_columns = ncol(rows) + 1L,
       method    = "two-way fixed effects by dummy variables")
}

#' Pooled OLS with unit-specific and period-specific averages
#'
#' Sec. 3: adding the unit-specific time averages
#' \eqn{\bar X_i}{Xbar_i} and the period-specific cross-sectional
#' averages \eqn{\bar X_t}{Xbar_t} to a pooled OLS regression
#' reproduces the two-way fixed effects estimates **exactly**.  The
#' design is far smaller: three blocks of width \eqn{p}{p} rather
#' than \eqn{N + T}{N+T} dummies.
#'
#' @inheritParams morie_two_way_fixed_effects
#' @return A list with \code{coef} (no intercept), \code{full}
#'   (intercept, X, unit-avg, period-avg), \code{n_columns},
#'   \code{method}, \code{identical_to}.
#' @references Wooldridge (2025) Sec. 3.
#' @export
morie_two_way_mundlak <- function(Y, unit, period, X) {
  p <- .causdidwd_panel(Y, unit, period)
  Xm <- .s03mat(X)
  if (nrow(Xm) != p$n)
    stop(sprintf("causdidwd: X has %d rows for %d observations",
                 nrow(Xm), p$n))
  px <- ncol(Xm)
  map_u <- list()
  map_t <- list()
  for (i in seq_len(p$n)) {
    if (is.null(map_u[[p$u[i]]])) map_u[[p$u[i]]] <- integer(0)
    map_u[[p$u[i]]] <- c(map_u[[p$u[i]]], i)
    if (is.null(map_t[[p$t[i]]])) map_t[[p$t[i]]] <- integer(0)
    map_t[[p$t[i]]] <- c(map_t[[p$t[i]]], i)
  }
  ubar <- vector("list", length(map_u)); names(ubar) <- names(map_u)
  for (g in names(map_u)) {
    idx <- map_u[[g]]
    s <- numeric(px)
    for (i in idx) for (j in seq_len(px)) s[j] <- s[j] + Xm[i, j]
    ubar[[g]] <- s / length(idx)
  }
  tbar <- vector("list", length(map_t)); names(tbar) <- names(map_t)
  for (g in names(map_t)) {
    idx <- map_t[[g]]
    s <- numeric(px)
    for (i in idx) for (j in seq_len(px)) s[j] <- s[j] + Xm[i, j]
    tbar[[g]] <- s / length(idx)
  }
  rows <- matrix(0, p$n, 3L * px)
  for (i in seq_len(p$n))
    rows[i, ] <- c(Xm[i, ], ubar[[p$u[i]]], tbar[[p$t[i]]])
  beta <- .s03lstsq(.causdidwd_with_intercept(rows), p$y, 1e-10)
  list(coef         = beta[seq_len(px) + 1L],
       full         = beta,
       n_columns    = 1L + 3L * px,
       method       = paste("two-way Mundlak: pooled OLS with",
                            "unit-specific time averages and",
                            "period-specific cross-sectional averages;",
                            "Wooldridge (2025) Sec. 3"),
       identical_to = "two-way fixed effects")
}

.causdidwd_cohorts <- function(first_treated, period) {
  ts <- .causdidwd_unique_sorted(period)
  order <- seq_along(ts) - 1L        # 0-based
  G <- character(length(first_treated))
  for (i in seq_along(first_treated)) {
    v <- first_treated[[i]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) {
      G[i] <- NA_character_
      next
    }
    s <- as.character(v)
    if (!(s %in% ts))
      stop(sprintf("causdidwd: adoption period %s is not a period in the data",
                   sQuote(s)))
    G[i] <- s
  }
  if (all(is.na(G))) stop("causdidwd: no unit is ever treated")
  list(G = G, ts = ts, order = order)
}

#' The saturated cohort-by-period regression of Sec. 5
#'
#' Every \eqn{(g, t)}{(g, t)} cell with \eqn{t >= g}{t >= g} gets
#' its own coefficient, so the fitted coefficients **are** the
#' \eqn{ATT(g, t)}{ATT(g,t)}.  Nothing is averaged, so nothing can
#' be averaged with a negative weight.
#'
#' @param first_treated N-vector of adoption periods (\code{NULL} for
#'   never-treated units).
#' @param X Optional N-by-p matrix of time-varying covariates.
#' @return A list with \code{estimate} (mean of \code{att}),
#'   \code{att} (named numeric keyed by "(g,t)" strings),
#'   \code{cells}, \code{n_cells}, \code{coef}, \code{cohorts},
#'   \code{periods}, \code{n}, \code{method}, \code{note}.
#' @references Wooldridge (2025) Sec. 5.
#' @export
morie_etwfe <- function(Y, unit, period, first_treated, X = NULL) {
  p <- .causdidwd_panel(Y, unit, period)
  if (length(first_treated) != p$n)
    stop(sprintf("causdidwd: %d adoption periods for %d observations",
                 length(first_treated), p$n))
  ch <- .causdidwd_cohorts(first_treated, p$t)
  G <- ch$G; ts <- ch$ts; ord <- ch$order
  # cells: sorted set of (G[i], t[i]) for treated-post observations
  cell_keys <- character(0)
  for (i in seq_len(p$n)) {
    if (is.na(G[i])) next
    gi <- ord[match(G[i], ts)]
    ti <- ord[match(p$t[i], ts)]
    if (ti >= gi) {
      key <- paste0(G[i], ",", p$t[i])
      if (!(key %in% cell_keys)) cell_keys <- c(cell_keys, key)
    }
  }
  if (length(cell_keys) == 0L)
    stop("causdidwd: no post-treatment cell exists")
  us <- .causdidwd_unique_sorted(p$u)
  ui <- match(p$u, us)
  ti <- match(p$t, ts)
  Xm <- if (is.null(X)) matrix(0, p$n, 0) else .s03mat(X)
  if (nrow(Xm) != p$n)
    stop(sprintf("causdidwd: X has %d rows for %d observations",
                 nrow(Xm), p$n))
  px <- ncol(Xm)
  ncells <- length(cell_keys)
  ncols  <- ncells + px + length(us) - 1L + length(ts) - 1L
  rows   <- matrix(0, p$n, ncols)
  for (i in seq_len(p$n)) {
    cell_col <- numeric(ncells)
    if (!is.na(G[i])) {
      key <- paste0(G[i], ",", p$t[i])
      pos <- match(key, cell_keys)
      if (!is.na(pos)) cell_col[pos] <- 1
    }
    d <- numeric(length(us) - 1L)
    if (ui[i] > 1L) d[ui[i] - 1L] <- 1
    f <- numeric(length(ts) - 1L)
    if (ti[i] > 1L) f[ti[i] - 1L] <- 1
    rows[i, ] <- c(cell_col, Xm[i, ], d, f)
  }
  beta <- .s03lstsq(.causdidwd_with_intercept(rows), p$y, 1e-10)
  att <- numeric(ncells)
  names(att) <- cell_keys
  for (j in seq_len(ncells)) att[j] <- beta[j + 1L]
  list(estimate = mean(att),
       att      = att,
       cells    = cell_keys,
       n_cells  = ncells,
       coef     = beta,
       cohorts  = sort(unique(G[!is.na(G)])),
       periods  = ts,
       n        = p$n,
       method   = paste("extended two-way fixed effects: saturated in",
                        "cohort x period; Wooldridge (2025) Sec. 5"),
       note     = paste("each coefficient IS an ATT(g,t); nothing is",
                        "averaged, so no negative weights arise"))
}

#' The two-step imputation estimator of Sec. 4
#'
#' Fit the two-way model on **untreated** observations only, impute
#' the untreated potential outcome everywhere, and average the
#' residual within each \eqn{(g, t)}{(g, t)} cell.  Sec. 5 proves
#' this is numerically identical to \code{morie_etwfe}.
#'
#' @inheritParams morie_etwfe
#' @return A list with \code{estimate}, \code{att}, \code{n_cells},
#'   \code{n_untreated_used}, \code{coef}, \code{method},
#'   \code{identical_to}.
#' @references Wooldridge (2025) Sec. 4 (procedure) and Sec. 5
#'   (equivalence).
#' @export
morie_imputation <- function(Y, unit, period, first_treated, X = NULL) {
  p <- .causdidwd_panel(Y, unit, period)
  if (length(first_treated) != p$n)
    stop(sprintf("causdidwd: %d adoption periods for %d observations",
                 length(first_treated), p$n))
  ch <- .causdidwd_cohorts(first_treated, p$t)
  G <- ch$G; ts <- ch$ts; ord <- ch$order
  treated <- rep(FALSE, p$n)
  for (i in seq_len(p$n)) {
    if (is.na(G[i])) next
    if (ord[match(p$t[i], ts)] >= ord[match(G[i], ts)]) treated[i] <- TRUE
  }
  untreated <- which(!treated)
  if (length(untreated) < 2L)
    stop("causdidwd: too few untreated observations to fit the baseline model")
  us <- .causdidwd_unique_sorted(p$u)
  ui <- match(p$u, us)
  ti <- match(p$t, ts)
  Xm <- if (is.null(X)) matrix(0, p$n, 0) else .s03mat(X)
  if (nrow(Xm) != p$n)
    stop(sprintf("causdidwd: X has %d rows for %d observations",
                 nrow(Xm), p$n))
  px <- ncol(Xm)
  build_row <- function(i) {
    d <- numeric(length(us) - 1L)
    if (ui[i] > 1L) d[ui[i] - 1L] <- 1
    f <- numeric(length(ts) - 1L)
    if (ti[i] > 1L) f[ti[i] - 1L] <- 1
    c(Xm[i, ], d, f)
  }
  Ru <- matrix(0, length(untreated), px + length(us) - 1L + length(ts) - 1L)
  for (k in seq_along(untreated)) {
    i <- untreated[k]
    Ru[k, ] <- build_row(i)
  }
  yu <- p$y[untreated]
  beta <- .s03lstsq(.causdidwd_with_intercept(Ru), yu, 1e-10)
  # accumulate residuals by (G[i], t[i]) for treated obs
  cell_sums <- list()
  cell_cnt  <- list()
  for (i in seq_len(p$n)) {
    if (!treated[i]) next
    ri <- c(1, build_row(i))
    yhat <- 0
    for (j in seq_along(beta)) yhat <- yhat + ri[j] * beta[j]
    key <- paste0(G[i], ",", p$t[i])
    if (is.null(cell_sums[[key]])) {
      cell_sums[[key]] <- p$y[i] - yhat
      cell_cnt[[key]]  <- 1L
    } else {
      cell_sums[[key]] <- cell_sums[[key]] + p$y[i] - yhat
      cell_cnt[[key]]  <- cell_cnt[[key]] + 1L
    }
  }
  keys <- names(cell_sums)
  att <- numeric(length(keys))
  names(att) <- keys
  for (k in seq_along(keys)) att[k] <- cell_sums[[keys[k]]] / cell_cnt[[keys[k]]]
  list(estimate         = mean(att),
       att              = att,
       n_cells          = length(att),
       n_untreated_used = length(untreated),
       coef             = beta,
       method           = paste("two-step imputation on untreated",
                                "observations; Wooldridge (2025) Sec. 4"),
       identical_to     = "etwfe, by Sec. 5")
}

#' Collapse the ATT(g,t) to one number or an event-time profile
#'
#' With Sec. 7 weights made visible.  The \code{simple} scheme takes
#' a (possibly user-supplied) weighted mean of the cell estimates.
#' The \code{event} scheme averages within each calendar period
#' \eqn{t}{t}.  The \code{cohort} scheme averages within each
#' treatment cohort \eqn{g}{g}.
#'
#' @param result Either an \code{etwfe} / \code{imputation} output
#'   (with an \code{att} element) or a bare \code{att}-like named
#'   numeric.
#' @param scheme One of \code{"simple"}, \code{"event"},
#'   \code{"cohort"}.
#' @param weights Optional named numeric of weights (one per cell)
#'   for the \code{"simple"} scheme.
#' @return For \code{simple}: a list with \code{estimate},
#'   \code{weights}, \code{scheme}.  For \code{event} / \code{cohort}:
#'   a list with \code{profile}, \code{estimate}, \code{scheme}.
#' @references Wooldridge (2025) Sec. 7.
#' @export
morie_aggregate <- function(result, scheme = "simple", weights = NULL) {
  if (!(scheme %in% c("simple", "event", "cohort")))
    stop(sprintf(paste("causdidwd: scheme must be simple, event or",
                       "cohort, got %s"), sQuote(scheme)))
  if (is.list(result) && !is.null(result$att)) {
    att <- result$att
  } else {
    att <- result
  }
  if (length(att) == 0L) stop("causdidwd: nothing to aggregate")
  if (scheme == "simple") {
    if (is.null(weights)) {
      w <- setNames(rep(1.0 / length(att), length(att)), names(att))
    } else {
      w <- as.numeric(weights)
      names(w) <- names(att)
    }
    tot <- sum(w)
    if (abs(tot) <= .EPS) stop("causdidwd: the weights sum to zero")
    return(list(estimate = sum(att * w) / tot,
                weights  = w,
                scheme   = "simple"))
  }
  prof <- list()
  for (nm in names(att)) {
    parts <- strsplit(nm, ",", fixed = TRUE)[[1]]
    if (length(parts) != 2L) next
    g <- parts[1]; t <- parts[2]
    key <- if (scheme == "cohort") g else t
    if (is.null(prof[[key]])) prof[[key]] <- numeric(0)
    prof[[key]] <- c(prof[[key]], att[[nm]])
  }
  prof <- lapply(prof, mean)
  list(estimate = mean(unlist(prof, use.names = FALSE)),
       profile  = setNames(unlist(prof, use.names = FALSE), names(prof)),
       scheme   = scheme)
}

# Module-name entry point.  Mirrors morie.fn.causdidwd.etwfe and
# the alias morie.fn.causdidwd.causal_did_wooldridge_eta.
#' @rdname morie_etwfe
#' @export
morie_causdidwd <- function(Y, unit, period, first_treated, X = NULL) {
  morie_etwfe(Y, unit, period, first_treated, X = X)
}
