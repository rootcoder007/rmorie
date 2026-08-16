# causdiddc_native.R
#
# de Chaisemartin, C. & D'Haultfoeuille, X. (2020). "Two-Way Fixed
# Effects Estimators with Heterogeneous Treatment Effects", American
# Economic Review 110(9), 2964-2996.
#
# The paper's result is a warning about a regression everyone runs. The
# two-way fixed effects coefficient is NOT an average treatment effect;
# it is a weighted sum of the cell-level effects with weights from the
# residual of D on the two sets of fixed effects -- and those weights
# can be NEGATIVE.  A cell with a negative weight enters the estimate
# with its effect sign flipped, so beta_fe can be negative when every
# single cell effect is positive.  DID_M compares, for each pair of
# consecutive periods, the cells that switch treatment against the
# cells whose treatment does not change.  It is unbiased for the
# average effect among switchers under common trends whatever the
# heterogeneity.
#
# Native R implementation mirroring Python morie.fn.causdiddc exactly:
# same residualisation by alternating projections for the Theorem 1
# weights, same DID_M switcher-vs-stayer construction, same payload
# keys.

#' .causdiddc_panel
#'
#' A step of the causdiddc_native implementation. Called by \code{.causdiddc_did_m}, \code{.causdiddc_twfe}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Coerced to numeric by the body, with \code{as.numeric}.
#' @param D Coerced to numeric by the body, with \code{as.numeric}.
#' @param group Coerced to vector by the body, with \code{as.vector}.
#' @param period Coerced to vector by the body, with \code{as.vector}.
#' @return A list with \code{Y}, \code{D}, \code{g}, \code{t}, \code{n}.
#' @export
.causdiddc_panel <- function(Y, D, group, period) {
  Yv <- as.numeric(Y)
  Dv <- as.numeric(D)
  g  <- as.vector(group)
  t  <- as.vector(period)
  n  <- length(Yv)
  if (!(length(Dv) == length(g) && length(g) == length(t) && length(t) == n))
    stop("causdiddc: Y, D, group and period must have equal length")
  if (n < 4L)
    stop("causdiddc: need at least four observations")
  for (j in seq_along(Dv)) {
    if (!(Dv[j] == 0 || Dv[j] == 1))
      stop("causdiddc: D must be binary 0/1")
  }
  for (j in seq_along(Yv)) {
    if (!is.finite(Yv[j]))
      stop("causdiddc: Y contains a non-finite value")
  }
  list(Y = Yv, D = Dv, g = g, t = t, n = n)
}

# Collapse to (group, period) cells: mean Y, count, treatment.
#' Collapse to (group, period) cells: mean Y, count, treatment
#'
#' A step of the causdiddc_native implementation. Called by \code{.causdiddc_did_m}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y A vector; indexed elementwise.
#' @param D A vector; indexed elementwise.
#' @param g Passed to \code{paste0}.
#' @param t Passed to \code{paste0}.
#' @return The value of \code{out}, as built in the body.
#' @export
.causdiddc_cells <- function(Y, D, g, t) {
  keys <- paste0(g, "\r", t)        # ad-hoc separator that never appears
  uk   <- unique(keys)
  out  <- new.env(hash = TRUE, parent = emptyenv())
  for (k in uk) {
    idx <- which(keys == k)
    subD <- D[idx]
    if (max(subD) != min(subD))
      stop(sprintf("causdiddc: treatment varies within the (group, period) cell %s",
                   k))
    out[[k]] <- list(
      mean = sum(Y[idx]) / length(idx),
      n    = length(idx),
      d    = subD[1L])
  }
  out
}

# The w_{g,t} of Theorem 1, from the residual of D on the two-way
# fixed effects.  Alternating projections, iterated to convergence --
# the within-transformation solver, not the matrix-form Frisch-Waugh.
#' The w_{g,t} of Theorem 1, from the residual of D on the two-way
#'
#' fixed effects.  Alternating projections, iterated to convergence --
#' the within-transformation solver, not the matrix-form Frisch-Waugh.
#'
#' @param D Coerced to numeric by the body, with \code{as.numeric}.
#' @param group Coerced to vector by the body, with \code{as.vector}.
#' @param period Coerced to vector by the body, with \code{as.vector}.
#' @param weights Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return A list with \code{weights}, \code{residual}.
#' @export
.causdiddc_twfe_weights <- function(D, group, period, weights = NULL) {
  Dv <- as.numeric(D)
  g  <- as.vector(group)
  t  <- as.vector(period)
  n  <- length(Dv)
  if (is.null(weights)) weights <- rep(1.0, n)
  weights <- as.numeric(weights)
  gs <- sort(unique(g))
  ts <- sort(unique(t))
  gi <- match(g, gs)
  ti <- match(t, ts)
  G  <- length(gs)
  Tp <- length(ts)
  r  <- Dv
  for (iter in seq_len(500L)) {
    # strip group means
    ga <- numeric(G); gw <- numeric(G)
    for (i in seq_len(n)) {
      k <- gi[i]
      ga[k] <- ga[k] + r[i] * weights[i]
      gw[k] <- gw[k] + weights[i]
    }
    for (i in seq_len(n)) {
      k <- gi[i]
      r[i] <- r[i] - ga[k] / gw[k]
    }
    # strip period means
    ta <- numeric(Tp); tw <- numeric(Tp)
    for (i in seq_len(n)) {
      k <- ti[i]
      ta[k] <- ta[k] + r[i] * weights[i]
      tw[k] <- tw[k] + weights[i]
    }
    for (i in seq_len(n)) {
      k <- ti[i]
      r[i] <- r[i] - ta[k] / tw[k]
    }
    # convergence check: within-group residual mass
    gr <- numeric(G)
    for (i in seq_len(n)) gr[gi[i]] <- gr[gi[i]] + r[i] * weights[i]
    if (max(abs(gr)) < 1e-13) break
  }
  denom <- 0.0
  for (i in seq_len(n)) denom <- denom + weights[i] * Dv[i] * r[i]
  if (abs(denom) < 1e-14)
    stop(paste("causdiddc: the treatment has no variation left after the",
               "fixed effects; beta_fe is not identified"))
  cells <- new.env(hash = TRUE, parent = emptyenv())
  keys  <- paste0(g, "\r", t)
  for (i in seq_len(n)) {
    if (Dv[i] == 1) {
      k <- keys[i]
      cur <- if (exists(k, envir = cells, inherits = FALSE)) get(k, envir = cells) else 0
      assign(k, cur + weights[i] * r[i] / denom, envir = cells)
    }
  }
  # translate to a named list so the user sees keys without the separator
  out <- list()
  if (length(ls(cells, all.names = TRUE)) > 0L) {
    nm <- ls(cells, all.names = TRUE)
    for (k in nm) {
      parts <- strsplit(k, "\r", fixed = TRUE)[[1L]]
      out[[paste0("(", parts[1L], ",", parts[2L], ")")]] <- get(k, envir = cells)
    }
  }
  list(weights = out, residual = r)
}

# The two-way fixed effects coefficient and its decomposition.
#' The two-way fixed effects coefficient and its decomposition
#'
#' A step of the causdiddc_native implementation. Called by \code{morie_causdiddc}, \code{morie_causdiddc_twfe}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.causdiddc_panel}.
#' @param D Passed to \code{.causdiddc_panel}.
#' @param group Passed to \code{.causdiddc_panel}.
#' @param period Passed to \code{.causdiddc_panel}.
#' @return A list with \code{estimate}, \code{beta_fe}, \code{weights}, \code{n_negative}, \code{negative_mass}, \code{weight_sum}, \code{n_treated_cells}, \code{n}, \code{method}, \code{note}.
#' @export
.causdiddc_twfe <- function(Y, D, group, period) {
  p <- .causdiddc_panel(Y, D, group, period)
  w  <- .causdiddc_twfe_weights(p$D, p$g, p$t)
  n  <- p$n
  denom <- 0.0
  num   <- 0.0
  for (i in seq_len(n)) {
    denom <- denom + p$D[i] * w$residual[i]
    num   <- num   + p$Y[i] * w$residual[i]
  }
  beta <- num / denom
  neg  <- w$weights[vapply(w$weights, function(v) v < 0, logical(1))]
  list(estimate        = beta,
       beta_fe         = beta,
       weights         = w$weights,
       n_negative      = length(neg),
       negative_mass   = sum(vapply(neg, abs, numeric(1))),
       weight_sum      = sum(vapply(w$weights, function(v) v, numeric(1))),
       n_treated_cells = length(w$weights),
       n               = n,
       method = paste("two-way fixed effects (de Chaisemartin &",
                      "D'Haultfoeuille 2020, Theorem 1)"),
       note = paste("beta_fe is a weighted sum of cell effects whose",
                    "weights sum to 1 but may be negative; n_negative and",
                    "negative_mass say how much of the estimate runs",
                    "backwards. Compare against did_m"))
}

# The paper's DID_M estimator: switchers vs stayers, period by period.
#' The paper\'s DID_M estimator: switchers vs stayers, period by period
#'
#' A step of the causdiddc_native implementation. Called by \code{morie_causdiddc}, \code{morie_causdiddc_did_m}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y Passed to \code{.causdiddc_panel}.
#' @param D Passed to \code{.causdiddc_panel}.
#' @param group Passed to \code{.causdiddc_panel}.
#' @param period Passed to \code{.causdiddc_panel}.
#' @return A list with \code{estimate}, \code{did_m}, \code{switches}, \code{n_switches}, \code{n_switching_obs}, \code{n}, \code{method}, \code{note}.
#' @export
.causdiddc_did_m <- function(Y, D, group, period) {
  p <- .causdiddc_panel(Y, D, group, period)
  cells <- .causdiddc_cells(p$Y, p$D, p$g, p$t)
  periods <- sort(unique(p$t))
  num <- 0.0; den <- 0.0
  parts <- list()
  if (length(periods) >= 2L) {
    for (k in seq_len(length(periods) - 1L)) {
      t0 <- periods[k]
      t1 <- periods[k + 1L]
      stayers_up <- list(); stayers_dn <- list()
      for (gg in unique(p$g)) {
        a <- cells[[paste0(gg, "\r", t0)]]
        b <- cells[[paste0(gg, "\r", t1)]]
        if (is.null(a) || is.null(b)) next
        if (a$d == b$d) {
          entry <- list(delta = b$mean - a$mean, n = b$n)
          if (a$d == 1) stayers_up[[length(stayers_up) + 1L]] <- entry
          else          stayers_dn[[length(stayers_dn) + 1L]] <- entry
        }
      }
      for (gg in sort(unique(p$g))) {
        a <- cells[[paste0(gg, "\r", t0)]]
        b <- cells[[paste0(gg, "\r", t1)]]
        if (is.null(a) || is.null(b) || a$d == b$d) next
        ctrl <- if (b$d == 1) stayers_dn else stayers_up
        if (length(ctrl) == 0L) next
        cw   <- sum(vapply(ctrl, function(e) e$n, numeric(1)))
        trend <- sum(vapply(ctrl,
                            function(e) e$delta * e$n, numeric(1))) / cw
        eff   <- (b$mean - a$mean) - trend
        if (b$d == 0) eff <- -eff     # a switch OUT of treatment
        num <- num + eff * b$n
        den <- den + b$n
        parts[[length(parts) + 1L]] <- list(
          group = gg, from = t0, to = t1, effect = eff, n = b$n,
          direction = if (b$d == 1) "in" else "out")
      }
    }
  }
  if (den == 0)
    stop(paste("causdiddc: no cell switches treatment between",
               "consecutive periods, so DID_M is not defined"))
  list(estimate        = num / den,
       did_m           = num / den,
       switches        = parts,
       n_switches      = length(parts),
       n_switching_obs = den,
       n               = p$n,
       method = paste("DID_M (de Chaisemartin & D'Haultfoeuille 2020):",
                      "switchers against stayers, period by period"),
       note = paste("unbiased for the average effect among switchers under",
                    "common trends, with no homogeneity assumption"))
}

#' de Chaisemartin-D'Haultfoeuille (2020) TWFE vs DID_M
#'
#' The two-way fixed effects coefficient \code{beta_fe} and the
#' paper's \code{DID_M} alternative, side by side, plus the Theorem 1
#' weights that show the gap.  \code{beta_fe} can take the wrong sign
#' under heterogeneity; \code{DID_M} is unbiased for the average
#' effect among switchers under common trends.
#'
#' @param Y Outcome, long format.
#' @param D Binary 0/1 treatment, long format.  Treated status must
#'   be constant within every (group, period) cell.
#' @param group,period Panel identifiers.
#' @return A list mirroring the Python RichResult payload:
#'   \code{estimate} (DID_M), \code{beta_fe}, \code{did_m},
#'   \code{weights}, \code{n_negative}, \code{negative_mass},
#'   \code{weight_sum}, \code{n_switches}, \code{gap}, \code{n},
#'   \code{method}, \code{note}.
#' @references de Chaisemartin, C. & D'Haultfoeuille, X. (2020).
#'   Two-Way Fixed Effects Estimators with Heterogeneous Treatment
#'   Effects. American Economic Review 110(9), 2964-2996.
#' @export
morie_causdiddc <- function(Y, D, group, period) {
  fe <- .causdiddc_twfe(Y, D, group, period)
  dm <- tryCatch(.causdiddc_did_m(Y, D, group, period),
                 error = function(e) {
                   if (grepl("DID_M is not defined", conditionMessage(e)))
                     list(estimate = NA_real_, n_switches = 0L)
                   else stop(e)
                 })
  list(estimate      = dm$estimate,
       beta_fe       = fe$beta_fe,
       did_m         = dm$estimate,
       weights       = fe$weights,
       n_negative    = fe$n_negative,
       negative_mass = fe$negative_mass,
       weight_sum    = fe$weight_sum,
       n_switches    = dm$n_switches,
       gap           = fe$beta_fe - dm$estimate,
       n             = fe$n,
       method = paste("TWFE against DID_M (de Chaisemartin &",
                      "D'Haultfoeuille 2020)"),
       note = paste("estimate is DID_M, the one that survives",
                    "heterogeneity; beta_fe is what a two-way fixed",
                    "effects regression would report, and gap is how far",
                    "apart they are"))
}

#' The w_{g,t} of Theorem 1
#'
#' The weights of de Chaisemartin & D'Haultfoeuille (2020) Theorem 1,
#' from the residual of the treatment indicator on the two sets of
#' fixed effects, scaled to sum to one over the treated cells.  They
#' can be NEGATIVE -- the regression everyone runs is not an average
#' treatment effect.
#'
#' @inheritParams morie_causdiddc
#' @param weights Optional observation weights.
#' @return A list with \code{weights} (named numeric keyed by
#'   "(group,period)" strings) and \code{residual} (the within-
#'   transformed treatment).
#' @references de Chaisemartin, C. & D'Haultfoeuille (2020), Theorem 1.
#' @export
morie_causdiddc_weights <- function(D, group, period, weights = NULL) {
  w <- .causdiddc_twfe_weights(D, group, period, weights = weights)
  list(weights = w$weights, residual = w$residual)
}

#' The two-way fixed effects coefficient
#'
#' The TWFE coefficient of de Chaisemartin & D'Haultfoeuille (2020)
#' Theorem 1 and its decomposition: the cell weights, the count and
#' mass of the negative weights, and the treated-cell count.
#'
#' @inheritParams morie_causdiddc
#' @return A list with \code{estimate}, \code{beta_fe}, \code{weights},
#'   \code{n_negative}, \code{negative_mass}, \code{weight_sum},
#'   \code{n_treated_cells}, \code{n}, \code{method}, \code{note}.
#' @references de Chaisemartin, C. & D'Haultfoeuille (2020).
#' @export
morie_causdiddc_twfe <- function(Y, D, group, period) {
  .causdiddc_twfe(Y, D, group, period)
}

#' The DID_M estimator
#'
#' The \code{DID_M} estimator of de Chaisemartin & D'Haultfoeuille
#' (2020): for each pair of consecutive periods, compare the outcome
#' change of cells that SWITCH treatment against the change of cells
#' whose treatment stays put, then average over switches, weighting by
#' the number of switching observations.  Unbiased for the average
#' effect among switchers under common trends whatever the
#' heterogeneity.
#'
#' @inheritParams morie_causdiddc
#' @return A list with \code{estimate}, \code{did_m}, \code{switches},
#'   \code{n_switches}, \code{n_switching_obs}, \code{n},
#'   \code{method}, \code{note}.
#' @references de Chaisemartin, C. & D'Haultfoeuille (2020).
#' @export
morie_causdiddc_did_m <- function(Y, D, group, period) {
  .causdiddc_did_m(Y, D, group, period)
}

# Public alias matching the Python module entry point.
causdiddc <- morie_causdiddc
twfe      <- morie_causdiddc_twfe
did_m     <- morie_causdiddc_did_m
causal_did_de_chaisemartin <- morie_causdiddc_weights
