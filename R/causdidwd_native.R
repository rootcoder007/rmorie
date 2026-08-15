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

.causdidwd_cohorts <- function(first_treated, period) {
  ts <- sort(unique(as.character(period)))
  ord <- setNames(seq_along(ts) - 1L, ts)
  n <- length(first_treated)
  G <- character(n)
  for (i in seq_len(n)) {
    v <- first_treated[[i]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) {
      G[i] <- NA_character_
    } else {
      s <- as.character(v)
      if (!(s %in% names(ord))) {
        stop(sprintf("causdidwd: adoption period %s is not a period in the data", s))
      }
      G[i] <- s
    }
  }
  if (!any(!is.na(G)))
    stop("causdidwd: no unit is ever treated")
  list(G = G, ts = ts, order = ord)
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
  ui <- match(p$u, us)
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
    key_u <- p$u[i]
    key_t <- p$t[i]
    if (is.null(map_u[[key_u]])) map_u[[key_u]] <- integer(0)
    map_u[[key_u]] <- c(map_u[[key_u]], i)
    if (is.null(map_t[[key_t]])) map_t[[key_t]] <- integer(0)
    map_t[[key_t]] <- c(map_t[[key_t]], i)
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
  list(coef        = beta[seq_len(px) + 1L],
       full        = beta,
       n_columns   = 1L + 3L * px,
       method      = "two-way Mundlak: pooled OLS with unit-specific time averages and period-specific cross-sectional averages; Wooldridge (2025) Sec. 3",
       identical_to = "two-way fixed effects")
}

#' The saturated cohort-by-period regression of Sec. 5
#'
#' Every \eqn{(g, t)}{(g, t)} cell with \eqn{t \ge g}{t >= g} gets
#' its own coefficient, so the fitted coefficients **are** the
#' \eqn{ATT(g, t)}{ATT(g, t)}.  Nothing is averaged, so nothing can
#' be averaged with a negative weight.
#'
#' @inheritParams morie_two_way_fixed_effects
#' @param first_treated N-vector of adoption periods (the period in
#'   which each unit is first treated; \code{NULL} or \code{NA} for
#'   never-treated).
#' @param X Optional N-by-p matrix of covariates.
#' @return A named list mirroring the RichResult payload: \code{estimate},
#'   \code{att} (named numeric vector keyed by \code{"<g>|<t>"}),
#'   \code{cells} (list of \code{c(g, t)} pairs), \code{n_cells},
#'   \code{coef}, \code{cohorts}, \code{periods}, \code{n},
#'   \code{method}, \code{note}.
#' @references Wooldridge (2025) Sec. 5.
#' @export
morie_etwfe <- function(Y, unit, period, first_treated, X = NULL) {
  p <- .causdidwd_panel(Y, unit, period)
  if (length(first_treated) != p$n)
    stop(sprintf("causdidwd: %d adoption periods for %d observations",
                 length(first_treated), p$n))
  ch <- .causdidwd_cohorts(first_treated, p$t)
  G <- ch$G
  ts <- ch$ts
  ord <- ch$order

  cell_keys <- character(0)
  for (i in seq_len(p$n)) {
    if (!is.na(G[i]) && ord[p$t[i]] >= ord[G[i]]) {
      key <- paste(G[i], p$t[i], sep = "\r")
      if (!(key %in% cell_keys))
        cell_keys <- c(cell_keys, key)
    }
  }
  if (length(cell_keys) == 0L)
    stop("causdidwd: no post-treatment cell exists")
  cell_keys <- sort(cell_keys)

  n_cells <- length(cell_keys)
  us <- .causdidwd_unique_sorted(p$u)
  ui <- match(p$u, us)
  ti <- match(p$t, ts)

  if (is.null(X)) {
    Xm <- matrix(0, p$n, 0L)
  } else {
    Xm <- .s03mat(X)
    if (nrow(Xm) != p$n)
      stop(sprintf("causdidwd: X has %d rows for %d observations",
                   nrow(Xm), p$n))
  }
  px <- ncol(Xm)

  n_cols <- n_cells + px + (length(us) - 1L) + (length(ts) - 1L)
  design <- matrix(0, p$n, n_cols)
  for (i in seq_len(p$n)) {
    if (!is.na(G[i])) {
      key <- paste(G[i], p$t[i], sep = "\r")
      ci <- match(key, cell_keys)
      if (!is.na(ci))
        design[i, ci] <- 1
    }
    if (px > 0L)
      design[i, n_cells + seq_len(px)] <- Xm[i, ]
    if (ui[i] > 1L)
      design[i, n_cells + px + ui[i] - 1L] <- 1
    if (ti[i] > 1L)
      design[i, n_cells + px + length(us) - 1L + ti[i] - 1L] <- 1
  }
  beta <- .s03lstsq(cbind(1, design), p$y, 1e-10)

  att_values <- beta[seq_len(n_cells) + 1L]
  att <- setNames(att_values, cell_keys)
  cells_list <- lapply(cell_keys,
                       function(k) strsplit(k, "\r", fixed = TRUE)[[1]])

  list(
    estimate = mean(att_values),
    att      = att,
    cells    = cells_list,
    n_cells  = n_cells,
    coef     = beta,
    cohorts  = sort(unique(G[!is.na(G)])),
    periods  = ts,
    n        = p$n,
    method   = "extended two-way fixed effects: saturated in cohort x period; Wooldridge (2025) Sec. 5",
    note     = "each coefficient IS an ATT(g,t); nothing is averaged, so no negative weights arise"
  )
}

#' The two-step imputation estimator of Sec. 4
#'
#' Fit the two-way model on **untreated** observations only, impute
#' the untreated potential outcome everywhere, and average the
#' residual within each \eqn{(g, t)}{(g, t)} cell.  Sec. 5 proves
#' this is numerically identical to \code{morie_etwfe}.
#'
#' @inheritParams morie_etwfe
#' @return A named list mirroring the RichResult payload: \code{estimate},
#'   \code{att}, \code{n_cells}, \code{n_untreated_used}, \code{coef},
#'   \code{method}, \code{identical_to}.
#' @references Wooldridge (2025) Sec. 4.
#' @export
morie_imputation <- function(Y, unit, period, first_treated, X = NULL) {
  p <- .causdidwd_panel(Y, unit, period)
  if (length(first_treated) != p$n)
    stop(sprintf("causdidwd: %d adoption periods for %d observations",
                 length(first_treated), p$n))
  ch <- .causdidwd_cohorts(first_treated, p$t)
  G <- ch$G
  ts <- ch$ts
  ord <- ch$order

  treated <- !is.na(G) & ord[p$t] >= ord[G]
  untreated_idx <- which(!treated)
  if (length(untreated_idx) < 2L)
    stop("causdidwd: too few untreated observations to fit the baseline model")

  us <- .causdidwd_unique_sorted(p$u)
  ui <- match(p$u, us)
  ti <- match(p$t, ts)

  if (is.null(X)) {
    Xm <- matrix(0, p$n, 0L)
  } else {
    Xm <- .s03mat(X)
    if (nrow(Xm) != p$n)
      stop(sprintf("causdidwd: X has %d rows for %d observations",
                   nrow(Xm), p$n))
  }
  px <- ncol(Xm)

  n_cols <- px + (length(us) - 1L) + (length(ts) - 1L)
  R <- matrix(0, length(untreated_idx), n_cols)
  for (k in seq_along(untreated_idx)) {
    i <- untreated_idx[k]
    off <- 0L
    if (px > 0L) {
      R[k, seq_len(px)] <- Xm[i, ]
      off <- px
    }
    if (ui[i] > 1L)
      R[k, off + ui[i] - 1L] <- 1
    off <- off + length(us) - 1L
    if (ti[i] > 1L)
      R[k, off + ti[i] - 1L] <- 1
  }
  y_untreated <- p$y[untreated_idx]
  beta <- .s03lstsq(cbind(1, R), y_untreated, 1e-10)

  cells <- list()
  for (i in seq_len(p$n)) {
    if (!treated[i]) next
    row_i <- numeric(length(beta))
    row_i[1L] <- 1
    off <- 1L
    if (px > 0L) {
      row_i[off + seq_len(px)] <- Xm[i, ]
      off <- off + px
    }
    if (ui[i] > 1L)
      row_i[off + ui[i] - 1L] <- 1
    off <- off + length(us) - 1L
    if (ti[i] > 1L)
      row_i[off + ti[i] - 1L] <- 1
    yhat <- sum(row_i * beta)
    key <- paste(G[i], p$t[i], sep = "\r")
    if (is.null(cells[[key]]))
      cells[[key]] <- numeric(0)
    cells[[key]] <- c(cells[[key]], p$y[i] - yhat)
  }

  att_keys <- names(cells)
  att_values <- vapply(cells, mean, numeric(1))
  att <- setNames(att_values, att_keys)

  list(
    estimate         = mean(att_values),
    att              = att,
    n_cells          = length(att),
    n_untreated_used = length(untreated_idx),
    coef             = beta,
    method           = "two-step imputation on untreated observations; Wooldridge (2025) Sec. 4",
    identical_to     = "etwfe, by Sec. 5"
  )
}

#' Collapse the ATT(g, t) to one number or a profile (Sec. 7)
#'
#' @param result Output of \code{morie_etwfe} or
#'   \code{morie_imputation}.
#' @param scheme One of \code{"simple"} (uniform weights),
#'   \code{"event"} (profile by \code{(g, t)}), or \code{"cohort"}
#'   (profile by calendar period \code{t}).
#' @param weights Optional named numeric vector of weights for the
#'   \code{"simple"} scheme; defaults to uniform.
#' @return A named list: for \code{"simple"} \code{estimate},
#'   \code{weights}, \code{scheme}; for \code{"event"} or
#'   \code{"cohort"} \code{profile}, \code{scheme}, \code{estimate}.
#' @references Wooldridge (2025) Sec. 7.
#' @export
morie_aggregate <- function(result, scheme = "simple", weights = NULL) {
  if (!(scheme %in% c("simple", "event", "cohort")))
    stop(sprintf("causdidwd: scheme must be simple, event or cohort, got %s",
                 scheme))
  att <- result$att
  if (is.null(att) || length(att) == 0L)
    stop("causdidwd: nothing to aggregate")

  if (scheme == "simple") {
    if (is.null(weights)) {
      w <- setNames(rep(1.0 / length(att), length(att)), names(att))
    } else {
      w <- weights
    }
    tot <- sum(w)
    if (abs(tot) <= .EPS)
      stop("causdidwd: the weights sum to zero")
    est <- sum(att * w) / tot
    return(list(estimate = est, weights = w, scheme = "simple"))
  }

  keys <- names(att)
  if (scheme == "cohort") {
    split_keys <- strsplit(keys, "\r", fixed = TRUE)
    group_keys <- vapply(split_keys, function(x) x[2L], character(1))
  } else {
    group_keys <- keys
  }
  unique_groups <- unique(group_keys)
  prof <- vapply(unique_groups,
                 function(g) mean(att[group_keys == g]),
                 numeric(1))
  est <- mean(prof)
  list(profile = prof, scheme = scheme, estimate = est)
}

morie_cheatsheet <- function() {
  paste("causdidwd: ETWFE. TWFE == two-way MUNDLAK -- pooled OLS ",
        "with unit-specific time averages AND period-specific ",
        "cross-sectional averages gives the identical ",
        "coefficients, in 3p columns rather than N+T dummies. ",
        "TWFE is not broken; a RESTRICTIVE model is. Saturate in ",
        "cohort x period and each coefficient IS an ATT(g,t) -- ",
        "nothing is averaged, so no negative weights. Imputation ",
        "on untreated observations gives the same numbers.",
        sep = "")
}

# Compact alias per ledger/NAMING.md
morie_etwfedid <- morie_etwfe

# Public names resolved by fn/_lazy_map.json
morie_causal_did_wooldridge_eta <- morie_etwfe

# Module entry point
morie_causdidwd <- morie_etwfe

#' @rdname morie_two_way_fixed_effects
#' @export
morie_causdidwd <- morie_two_way_fixed_effects

#' @rdname morie_two_way_fixed_effects
#' @export
morie_causdidwd <- morie_two_way_fixed_effects

#' @rdname morie_two_way_fixed_effects
#' @export
morie_causdidwd <- morie_two_way_fixed_effects

#' @rdname morie_two_way_fixed_effects
#' @export
morie_causdidwd <- morie_two_way_fixed_effects
