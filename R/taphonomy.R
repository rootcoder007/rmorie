# SPDX-License-Identifier: AGPL-3.0-or-later
#' Taphonomic preservation as a causal-inference problem
#'
#' A thin domain layer that recasts the question "is this body's preservation
#' natural or anomalous?" as a treatment-effect estimate over MORIE's existing
#' causal estimators. It adds no new statistics: it documents the taphonomy
#' variable set and dispatches to \code{\link{morie_estimate_irm}} /
#' \code{\link{morie_estimate_cate}}, then attaches an E-value
#' (\code{\link{morie_e_value}}) that quantifies how strong an \emph{unmeasured}
#' cause would have to be to explain away the natural preservation effect --
#' i.e. it turns a claim of "incorruptibility" into a bounded statistical one.
#'
#' \strong{This module ships no data.} Forensic-taphonomy comparanda (documented
#' lime/desiccation burials with preservation outcomes) and any non-invasive
#' readings of a specific case (CT/micro-CT density, pXRF elemental signatures,
#' hyperspectral surface composition) must be supplied by the caller from real
#' sources. \code{\link{morie_taphonomy_schema}} returns a typed, zero-row frame
#' describing the expected columns; it never fabricates rows.
#'
#' \strong{What it can and cannot do.} With comparanda it estimates the average
#' preservation effect attributable to burial \emph{processing} (e.g. quicklime
#' desiccation) and reports the E-value residual. It cannot falsify a miracle:
#' a small natural-preservation probability yields a \emph{delta}, not a
#' disproof (Chernozhukov et al. 2018 give the effect; VanderWeele & Ding 2017
#' bound the unmeasured-confounding needed to nullify it).
#'
#' @name taphonomy
#' @references
#' Chernozhukov V, et al. (2018). Double/debiased machine learning.
#'   \emph{The Econometrics Journal} 21(1), C1--C68. \doi{10.1111/ectj.12097}
#'
#' VanderWeele TJ, Ding P (2017). Sensitivity analysis in observational
#'   research: introducing the E-value. \emph{Annals of Internal Medicine}
#'   167(4), 268--274. \doi{10.7326/M16-2607}
NULL

# Null-coalescing helper (package idiom: defined per-file, see did.R / iv.R).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Canonical taphonomy variable set. Grouped by causal role. Kept internal so
# the schema and the estimator agree on one source of truth.
.taphonomy_vars <- function() {
  list(
    # Anthropogenic "processing" — candidate treatment(s).
    treatment = c(
      lime_treatment = "integer"   # 1 = interred with quicklime, 0 = not
    ),
    # Environmental + handling covariates (confounders / effect modifiers).
    covariates = c(
      temp_c            = "numeric",  # mean interment temperature (deg C)
      humidity_pct      = "numeric",  # mean relative humidity (%)
      arid              = "integer",  # 1 = cool/arid microclimate
      casket_sealed     = "integer",  # 1 = sealed casket / low air exchange
      reinterment_count = "integer",  # times exhumed / moved
      exposure_days     = "numeric",  # days of pre-burial exposure
      decades_elapsed   = "numeric"   # time since death (decades)
    ),
    # Non-invasive measurement channels (outcome proxies / auxiliary covariates).
    measurements = c(
      ct_density_hu       = "numeric",  # CT/micro-CT mean tissue density (HU)
      ct_void_fraction    = "numeric",  # internal void fraction (0-1)
      pxrf_ca_ppm         = "numeric",  # pXRF residual surface calcium (ppm)
      hyperspectral_resin = "integer"   # 1 = applied resin/wax detected
    ),
    # Preservation outcome.
    outcome = c(
      preservation_score = "numeric"  # 0 (fully decayed) .. 1 (intact)
    )
  )
}

#' Taphonomy data schema (zero-row template)
#'
#' Returns an empty, typed \code{data.frame} documenting the columns
#' \code{\link{morie_taphonomy_preservation_delta}} expects. No rows are
#' fabricated -- the caller fills it with real comparanda.
#'
#' @return A zero-row \code{data.frame}; each column has the correct type and a
#'   \code{"role"} attribute (\code{"treatment"}, \code{"covariate"},
#'   \code{"measurement"}, or \code{"outcome"}).
#' @examples
#' str(morie_taphonomy_schema())
#' @export
morie_taphonomy_schema <- function() {
  v <- .taphonomy_vars()
  spec <- c(v$treatment, v$covariates, v$measurements, v$outcome)
  roles <- c(
    rep("treatment",   length(v$treatment)),
    rep("covariate",   length(v$covariates)),
    rep("measurement", length(v$measurements)),
    rep("outcome",     length(v$outcome))
  )
  cols <- Map(function(type) {
    switch(type, integer = integer(0), numeric = numeric(0), character(0))
  }, spec)
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  attr(df, "role") <- stats::setNames(roles, names(spec))
  df
}

# Convert a standardized mean difference d = ATE / sd(outcome) to an approximate
# risk ratio for the E-value, per VanderWeele & Ding (2017): RR ~= exp(0.91 * d).
# E-values are defined on ratio scales; this is the standard continuous-outcome
# bridge. Returns a value >= 1 (E-value machinery expects RR on the ">1" side).
.taphonomy_smd_to_rr <- function(d) {
  rr <- exp(0.91 * d)
  if (rr < 1) rr <- 1 / rr
  rr
}

#' Estimate the natural preservation "delta" and its E-value
#'
#' Recasts a taphonomy question as a treatment-effect estimate: how much of the
#' observed preservation is attributable to burial \emph{processing} (the
#' \code{treatment}, e.g. quicklime desiccation), holding environment and
#' handling fixed, and how strong an unmeasured cause (a "miracle") would need to
#' be to explain the rest. A thin dispatch over
#' \code{\link{morie_estimate_irm}} (DoubleML IRM, cross-fit ATE + SE; default)
#' or \code{\link{morie_estimate_cate}} (meta-learner, per-unit heterogeneous
#' effects) plus \code{\link{morie_e_value}}. Adds no new statistics.
#'
#' @param data A \code{data.frame} of real comparanda (see
#'   \code{\link{morie_taphonomy_schema}}). Must be non-empty.
#' @param treatment Binary treatment column (default \code{"lime_treatment"}).
#' @param outcome Continuous preservation outcome (default
#'   \code{"preservation_score"}).
#' @param covariates Character vector of confounder/measurement columns. Defaults
#'   to every schema covariate + measurement present in \code{data}.
#' @param estimator \code{"irm"} (DoubleML cross-fit ATE with an
#'   orthogonal SE; default) or \code{"cate"} (meta-learner per-unit effects,
#'   summarised to the mean delta, with the full per-unit vector in
#'   \code{cate_per_unit} and its dispersion in \code{cate_sd}).
#' @param se_method Inference for the \code{"cate"} path only:
#'   \code{"none"} (default) reports the point estimate + \code{cate_sd}
#'   dispersion with no SE/CI/p-value; \code{"bootstrap"} resamples rows and
#'   refits the CATE procedure \code{n_boot} times to give a \emph{valid} SE,
#'   percentile CI, and p-value for the mean effect. (\code{sd(tau)/sqrt(n)} is
#'   deliberately not offered: the per-unit effects are correlated fitted
#'   predictions, so it understates the true SE -- use \code{cate_sd} for
#'   heterogeneity or bootstrap for inference.) Ignored for \code{"irm"}, which
#'   carries its own orthogonal SE.
#' @param n_boot Bootstrap resamples when \code{se_method = "bootstrap"}
#'   (default 199).
#' @param boot_seed RNG seed for the bootstrap (default 42).
#' @param ... Passed to the chosen estimator (e.g. \code{n_folds} for IRM,
#'   \code{meta_learner} for CATE).
#' @return A named \code{list} (RichResult-style; all estimates are
#'   \code{double}): \code{value} (the preservation delta = ATE / mean CATE),
#'   \code{se}, \code{p_value} (Wald; \code{NA} on the CATE path -- no valid SE),
#'   \code{ci_lower}, \code{ci_upper}, \code{n} (integer count),
#'   \code{e_value}, \code{e_value_ci}, \code{estimator}, \code{cate_per_unit}
#'   and \code{cate_sd} (NULL unless \code{estimator = "cate"}), \code{warnings},
#'   and a plain-language \code{interpretation}.
#' @seealso \code{\link{morie_estimate_irm}}, \code{\link{morie_estimate_cate}},
#'   \code{\link{morie_e_value}}, \code{\link{morie_causal_impact}}
#' @examples
#' \donttest{
#' if (requireNamespace("DoubleML", quietly = TRUE)) {
#'   df <- morie_taphonomy_schema()        # fill with REAL comparanda first
#'   # morie_taphonomy_preservation_delta(df)
#' }
#' }
#' @export
morie_taphonomy_preservation_delta <- function(data,
                                               treatment = "lime_treatment",
                                               outcome = "preservation_score",
                                               covariates = NULL,
                                               estimator = c("irm", "cate"),
                                               se_method = c("none", "bootstrap"),
                                               n_boot = 199L,
                                               boot_seed = 42L,
                                               ...) {
  estimator <- match.arg(estimator)
  se_method <- match.arg(se_method)
  if (!is.data.frame(data)) stop("`data` must be a data.frame", call. = FALSE)
  if (nrow(data) == 0L) {
    stop("`data` is empty. Fill morie_taphonomy_schema() with real comparanda; ",
         "this module never fabricates burial rows.", call. = FALSE)
  }
  for (col in c(treatment, outcome)) {
    if (!col %in% names(data)) {
      stop(sprintf("column '%s' not found in `data`", col), call. = FALSE)
    }
  }
  if (is.null(covariates)) {
    v <- .taphonomy_vars()
    wanted <- names(c(v$covariates, v$measurements))
    covariates <- intersect(wanted, names(data))
  }
  covariates <- setdiff(covariates, c(treatment, outcome))
  if (length(covariates) == 0L) {
    stop("no covariates available; supply environment/handling/measurement ",
         "columns (see morie_taphonomy_schema())", call. = FALSE)
  }

  warnings <- character(0)
  tt <- data[[treatment]]
  if (length(unique(stats::na.omit(tt))) < 2L) {
    warnings <- c(warnings, "treatment has no contrast (all treated or all ",
                  "control) -- the effect is not identified.")
  }

  # Normalise each estimator's output to a common summary (ate/se/ci/n/method).
  # IRM returns a list (cross-fit ATE + orthogonal SE); CATE returns a bare
  # per-unit numeric vector, summarised here to its mean + dispersion-based SE.
  cate_per_unit <- NULL
  cate_sd <- NULL
  if (estimator == "irm") {
    est <- morie_estimate_irm(data, treatment, outcome, covariates, ...)
    ate <- as.numeric(est$ate)
    se  <- as.numeric(est$se %||% NA_real_)
    ci_lower <- as.numeric(est$ci_lower %||% NA_real_)
    ci_upper <- as.numeric(est$ci_upper %||% NA_real_)
    n <- as.integer(est$n %||% nrow(data))
    method <- est$method %||% "IRM (DoubleML)"
  } else {
    tau <- as.numeric(morie_estimate_cate(data, treatment, outcome, covariates, ...))
    tau <- tau[is.finite(tau)]
    if (length(tau) == 0L) stop("CATE estimation returned no finite effects",
                                call. = FALSE)
    cate_per_unit <- tau
    n <- length(tau)
    ate <- mean(tau)
    cate_sd <- if (n > 1L) stats::sd(tau) else NA_real_  # effect heterogeneity
    method <- "CATE (meta-learner, mean of per-unit effects)"
    if (se_method == "bootstrap") {
      # Valid inference: resample rows and refit the whole CATE procedure, so
      # the SE reflects sampling + estimation uncertainty of the mean effect
      # (not the correlated per-unit dispersion).
      set.seed(boot_seed)
      nb <- as.integer(n_boot)
      boot <- vapply(seq_len(nb), function(b) {
        idx <- sample.int(nrow(data), replace = TRUE)
        tb <- as.numeric(morie_estimate_cate(
          data[idx, , drop = FALSE], treatment, outcome, covariates, ...))
        mean(tb[is.finite(tb)])
      }, numeric(1))
      boot <- boot[is.finite(boot)]
      se <- stats::sd(boot)
      qs <- stats::quantile(boot, c(0.025, 0.975), names = FALSE)
      ci_lower <- qs[1]
      ci_upper <- qs[2]
      method <- sprintf("CATE (meta-learner, mean; %d-boot SE/CI)", length(boot))
    } else {
      # Point summary only. sd(tau)/sqrt(n) is NOT reported as an SE: the
      # per-unit effects are correlated fitted predictions, so it would
      # understate the true uncertainty. Use se_method="bootstrap" for a valid
      # SE, or cate_sd for heterogeneity.
      se <- NA_real_
      ci_lower <- NA_real_
      ci_upper <- NA_real_
      warnings <- c(warnings, sprintf(
        paste0("CATE point summary: no SE reported (per-unit effects are ",
               "correlated fitted predictions; cate_sd=%.3f is heterogeneity, ",
               "range [%.3f, %.3f]). Pass se_method='bootstrap' for a valid SE ",
               "+ CI + p-value, or use estimator='irm'."),
        cate_sd, min(tau), max(tau)))
    }
  }

  # Wald p-value from a valid SE only. IRM's SE is a cross-fit orthogonal SE
  # (test is exact-normal in the limit); the CATE path has se = NA (its per-unit
  # spread is heterogeneity, not sampling error) so no p-value is claimed.
  p_value <- if (is.finite(se) && se > 0 && is.finite(ate)) {
    2 * stats::pnorm(-abs(ate / se))
  } else {
    NA_real_
  }

  # E-value on the standardized effect (continuous-outcome bridge).
  out_sd <- stats::sd(data[[outcome]], na.rm = TRUE)
  ev <- list(morie_e_value = NA_real_, e_value_ci = NA_real_)
  if (is.finite(out_sd) && out_sd > 0 && is.finite(ate)) {
    d  <- ate / out_sd
    rr <- .taphonomy_smd_to_rr(d)
    rr_lo <- if (is.finite(se) && se > 0) {
      .taphonomy_smd_to_rr((abs(ate) - 1.959964 * se) / out_sd)
    } else NULL
    if (!is.null(rr_lo) && rr_lo < 1) rr_lo <- 1  # CI crosses null
    ev <- morie_e_value(rr, rr_lower = rr_lo)
  } else {
    warnings <- c(warnings, "outcome has zero/undefined SD; E-value skipped.")
  }

  interp <- sprintf(
    paste0(
      "Preservation delta (ATE of %s on %s) = %.3f [%.3f, %.3f], n=%d, via %s. ",
      "An unmeasured cause would need an E-value of %s (association with both ",
      "treatment and outcome, on the risk-ratio scale) to fully explain it ",
      "away. This bounds -- it does not falsify -- any 'incorruptibility' claim: ",
      "a natural mechanism (e.g. quicklime desiccation) of this strength is ",
      "sufficient, but sufficiency is not proof of exclusivity."
    ),
    treatment, outcome, ate, ci_lower, ci_upper, n, method,
    if (is.finite(ev$morie_e_value)) sprintf("%.2f", ev$morie_e_value) else "NA"
  )

  list(
    value          = ate,
    se             = se,
    p_value        = p_value,
    ci_lower       = ci_lower,
    ci_upper       = ci_upper,
    n              = n,
    e_value        = ev$morie_e_value,
    e_value_ci     = ev$e_value_ci,
    estimator      = method,
    cate_per_unit  = cate_per_unit,
    cate_sd        = cate_sd,
    warnings       = warnings,
    interpretation = interp
  )
}


# ===========================================================================
# Stochastic decay modelling -- absorbing Markov chain (DTMC)
# ===========================================================================

#' Build a taphonomic decay Markov chain (DTMC)
#'
#' A discrete-time absorbing Markov chain over decomposition stages. From each
#' transient stage a body either \emph{progresses} one step toward the terminal
#' \code{"skeletal"} state (ordinary decay) or \emph{diverts} to the terminal
#' \code{"mummified"} state (preserved). A \code{preservation} factor -- the
#' desiccant/bacteriocidal effect of quicklime, aridity, sealing -- shifts mass
#' from progression toward diversion, so a high-lime interment routes the body
#' to \code{"mummified"} rather than \code{"skeletal"}. Compare the natural
#' (\code{preservation = 0}) chain against a treated one to quantify how much
#' the burial practice changes the fate distribution.
#'
#' @param preservation Preservation factor in \code{[0, 1]}: 0 = ordinary
#'   decay, higher = stronger diversion toward mummification.
#' @param decay_rate Base per-step progression probability in \code{(0, 1]}
#'   (default 0.5).
#' @param mummify_rate Base per-step diversion-to-mummified probability in
#'   \code{[0, 1]} (default 0.5); scaled by \code{preservation}.
#' @param states Character vector of transient decomposition stages (default
#'   \code{c("fresh", "bloat", "active", "advanced")}).
#' @return A named \code{list}: \code{P} (row-stochastic transition matrix,
#'   \code{double}), \code{states}, \code{transient}, \code{absorbing}
#'   (\code{c("skeletal", "mummified")}), and \code{preservation}.
#' @seealso \code{\link{morie_taphonomy_decay_absorption}},
#'   \code{\link{morie_taphonomy_decay_simulate}},
#'   \code{\link{morie_taphonomy_decay_delta}}
#' @examples
#' ch <- morie_taphonomy_decay_chain(preservation = 0.7)
#' round(ch$P, 3)
#' @export
morie_taphonomy_decay_chain <- function(preservation = 0,
                                        decay_rate = 0.5,
                                        mummify_rate = 0.5,
                                        states = c("fresh", "bloat", "active",
                                                   "advanced")) {
  if (!is.numeric(preservation) || preservation < 0 || preservation > 1) {
    stop("`preservation` must be in [0, 1]", call. = FALSE)
  }
  if (decay_rate <= 0 || decay_rate > 1 || mummify_rate < 0 || mummify_rate > 1) {
    stop("`decay_rate` in (0,1] and `mummify_rate` in [0,1] required",
         call. = FALSE)
  }
  transient <- as.character(states)
  if (length(transient) < 1L || anyDuplicated(transient)) {
    stop("`states` must be >= 1 unique transient stage(s)", call. = FALSE)
  }
  absorbing <- c("skeletal", "mummified")
  all_states <- c(transient, absorbing)
  m <- length(all_states)
  k <- length(transient)
  P <- matrix(0, m, m, dimnames = list(all_states, all_states))

  for (i in seq_len(k)) {
    prog <- decay_rate * (1 - preservation)  # advance one decay stage
    mum  <- mummify_rate * preservation      # divert to mummified
    tot  <- prog + mum
    if (tot > 1) {                            # renormalise if leaving prob > 1
      prog <- prog / tot
      mum  <- mum / tot
      tot  <- 1
    }
    stay <- 1 - tot                           # geometric dwell in current stage
    nxt  <- if (i < k) transient[i + 1L] else "skeletal"
    P[i, nxt] <- P[i, nxt] + prog
    P[i, "mummified"] <- P[i, "mummified"] + mum
    P[i, transient[i]] <- P[i, transient[i]] + stay
  }
  P["skeletal", "skeletal"] <- 1
  P["mummified", "mummified"] <- 1

  list(P = P, states = all_states, transient = transient,
       absorbing = absorbing, preservation = preservation)
}

#' Absorption analysis of a taphonomic decay chain
#'
#' Uses the fundamental matrix \eqn{N = (I - Q)^{-1}} of the absorbing chain to
#' compute, for a body entering at \code{start}, the probability of each
#' terminal fate (\code{"skeletal"} vs \code{"mummified"}) and the expected
#' number of steps to absorption (Grinstead & Snell, Ch. 11).
#'
#' @param chain A chain from \code{\link{morie_taphonomy_decay_chain}}.
#' @param start Transient stage the body enters at (default the first).
#' @return A named \code{list}: \code{absorption} (named probabilities over the
#'   absorbing states, summing to 1, \code{double}), \code{expected_steps}
#'   (\code{double}), \code{fundamental} (matrix \code{N}), and \code{B} (full
#'   transient-by-absorbing probability matrix).
#' @references Grinstead CM, Snell JL (1997). \emph{Introduction to
#'   Probability} (2nd ed.), Ch. 11 (Absorbing Markov Chains). AMS.
#' @examples
#' morie_taphonomy_decay_absorption(morie_taphonomy_decay_chain(0.7))$absorption
#' @export
morie_taphonomy_decay_absorption <- function(chain,
                                             start = chain$transient[1]) {
  tr <- chain$transient
  ab <- chain$absorbing
  if (!start %in% tr) {
    stop(sprintf("`start` must be a transient state (%s)",
                 paste(tr, collapse = ", ")), call. = FALSE)
  }
  Q <- chain$P[tr, tr, drop = FALSE]
  R <- chain$P[tr, ab, drop = FALSE]
  N <- solve(diag(length(tr)) - Q)      # fundamental matrix
  B <- N %*% R                           # absorption probabilities
  list(
    absorption     = B[start, ],
    expected_steps = as.numeric(rowSums(N)[start]),
    fundamental    = N,
    B              = B
  )
}

#' Simulate one decay path through a taphonomic Markov chain
#'
#' Draws a single realised trajectory from death through the transient stages
#' until an absorbing fate is reached (or \code{n_steps} elapses).
#'
#' @param chain A chain from \code{\link{morie_taphonomy_decay_chain}}.
#' @param start Starting transient stage (default the first).
#' @param n_steps Maximum steps to simulate (default 100).
#' @param seed RNG seed (default 42) for reproducibility.
#' @return A character vector: the sequence of visited states, ending at an
#'   absorbing state if reached.
#' @examples
#' morie_taphonomy_decay_simulate(morie_taphonomy_decay_chain(0.7), seed = 1)
#' @export
morie_taphonomy_decay_simulate <- function(chain, start = chain$transient[1],
                                           n_steps = 100L, seed = 42L) {
  if (!start %in% chain$transient) {
    stop("`start` must be a transient state", call. = FALSE)
  }
  set.seed(seed)
  s <- start
  path <- character(n_steps + 1L)
  path[1] <- s
  used <- 1L
  for (i in seq_len(n_steps)) {
    s <- sample(chain$states, 1L, prob = chain$P[s, ])
    path[i + 1L] <- s
    used <- i + 1L
    if (s %in% chain$absorbing) break
  }
  path[seq_len(used)]
}

#' Natural-vs-treated fate delta for a taphonomic decay chain
#'
#' The Markov-chain analogue of the preservation "delta": the change in the
#' probability of ending \code{"mummified"} when a preservation factor (e.g.
#' quicklime) is applied, relative to the natural \code{preservation = 0}
#' baseline. A large positive delta means the burial practice -- not chance --
#' drives the preserved outcome.
#'
#' @param preservation Preservation factor in \code{(0, 1]} for the treated
#'   chain.
#' @param start Starting transient stage (default the first).
#' @param ... Passed to \code{\link{morie_taphonomy_decay_chain}}
#'   (\code{decay_rate}, \code{mummify_rate}, \code{states}).
#' @return A named \code{list} (all \code{double}): \code{p_mummified_natural},
#'   \code{p_mummified_treated}, \code{delta}, and a plain-language
#'   \code{interpretation}.
#' @examples
#' morie_taphonomy_decay_delta(0.7)$delta
#' @export
morie_taphonomy_decay_delta <- function(preservation, start = NULL, ...) {
  if (!is.numeric(preservation) || preservation <= 0 || preservation > 1) {
    stop("`preservation` must be in (0, 1] for a contrast", call. = FALSE)
  }
  nat_chain <- morie_taphonomy_decay_chain(preservation = 0, ...)
  trt_chain <- morie_taphonomy_decay_chain(preservation = preservation, ...)
  if (is.null(start)) start <- nat_chain$transient[1]
  p_nat <- morie_taphonomy_decay_absorption(nat_chain, start)$absorption[["mummified"]]
  p_trt <- morie_taphonomy_decay_absorption(trt_chain, start)$absorption[["mummified"]]
  delta <- p_trt - p_nat
  list(
    p_mummified_natural = p_nat,
    p_mummified_treated = p_trt,
    delta = delta,
    interpretation = sprintf(
      paste0("P(mummified) rises from %.3f (natural, no preservation) to %.3f ",
             "under preservation=%.2f -- a fate delta of %+.3f. The preserved ",
             "outcome is driven by the burial practice, not baseline decay ",
             "dynamics."),
      p_nat, p_trt, preservation, delta)
  )
}


# ===========================================================================
# Forensic likelihood-ratio framework
# ===========================================================================

# ENFSI (2015) verbal-equivalent scale for a likelihood ratio supporting H1.
.taphonomy_lr_verbal <- function(lr) {
  if (!is.finite(lr)) return("extremely strong support (LR effectively infinite)")
  x <- if (lr >= 1) lr else 1 / lr
  side <- if (lr >= 1) "H1" else "H2"
  band <- if (x <= 1) {
    "no support either way"
  } else if (x <= 10) {
    "weak support"
  } else if (x <= 100) {
    "moderate support"
  } else if (x <= 1000) {
    "moderately strong support"
  } else if (x <= 10000) {
    "strong support"
  } else if (x <= 1e6) {
    "very strong support"
  } else {
    "extremely strong support"
  }
  if (band == "no support either way") band else paste(band, "for", side)
}

#' Gaussian log-likelihood of forensic evidence under a model
#'
#' Sum of independent normal log-densities for a vector of measured evidence
#' (e.g. pXRF residual calcium, CT tissue density) under a model with expected
#' \code{mean} and \code{sd}. Building block for
#' \code{\link{morie_taphonomy_likelihood_ratio}}: evaluate the evidence under
#' each competing hypothesis' model, then take the ratio.
#'
#' @param evidence Numeric vector of measured values.
#' @param mean Numeric model mean(s), recycled to \code{length(evidence)}.
#' @param sd Numeric model standard deviation(s) (> 0), recycled.
#' @return A single \code{double}: the total log-likelihood.
#' @examples
#' morie_taphonomy_evidence_loglik(c(1200, 1310), mean = 1250, sd = 80)
#' @export
morie_taphonomy_evidence_loglik <- function(evidence, mean, sd) {
  evidence <- as.numeric(evidence)
  if (length(evidence) == 0L) stop("`evidence` is empty", call. = FALSE)
  if (any(!is.finite(evidence))) stop("`evidence` has non-finite values",
                                      call. = FALSE)
  if (any(sd <= 0)) stop("`sd` must be > 0", call. = FALSE)
  sum(stats::dnorm(evidence, mean = mean, sd = sd, log = TRUE))
}

#' Forensic likelihood ratio for two competing hypotheses
#'
#' Given the log-likelihood of the evidence under a natural/target hypothesis
#' (\code{loglik_h1}) and an alternative (\code{loglik_h2}), returns the
#' likelihood ratio \eqn{LR = P(E \mid H_1) / P(E \mid H_2)} with its base-10
#' logarithm and the ENFSI (2015) verbal equivalent. Computed in log space for
#' numerical stability. This is the standard way to state forensic evidence:
#' the LR reports how much the evidence favours \eqn{H_1} over \eqn{H_2}; it is
#' \emph{not} the posterior odds and does not "prove" either hypothesis.
#'
#' @param loglik_h1,loglik_h2 Log-likelihoods of the evidence under \eqn{H_1}
#'   and \eqn{H_2} (e.g. from \code{\link{morie_taphonomy_evidence_loglik}}).
#' @return A named \code{list} (\code{double} where numeric): \code{lr},
#'   \code{log10_lr}, \code{log_lr}, \code{verbal}, and \code{interpretation}.
#' @references ENFSI (2015). \emph{Guideline for Evaluative Reporting in
#'   Forensic Science}. European Network of Forensic Science Institutes.
#'   Aitken CGG, Taroni F (2004). \emph{Statistics and the Evaluation of
#'   Evidence for Forensic Scientists} (2nd ed.). Wiley.
#' @examples
#' morie_taphonomy_likelihood_ratio(loglik_h1 = -3.1, loglik_h2 = -12.7)$verbal
#' @export
morie_taphonomy_likelihood_ratio <- function(loglik_h1, loglik_h2) {
  loglik_h1 <- as.numeric(loglik_h1)
  loglik_h2 <- as.numeric(loglik_h2)
  log_lr <- loglik_h1 - loglik_h2
  lr <- exp(log_lr)
  log10_lr <- log_lr / log(10)
  verbal <- .taphonomy_lr_verbal(lr)
  list(
    lr = lr,
    log10_lr = log10_lr,
    log_lr = log_lr,
    verbal = verbal,
    interpretation = sprintf(
      paste0("LR = %.4g (log10 = %.3f): the evidence is %s. The observed state ",
             "is %s more probable under H1 (natural preservation model) than ",
             "under H2. This quantifies support; it is not proof and not a ",
             "posterior probability."),
      lr, log10_lr, verbal,
      if (is.finite(lr) && lr >= 1) sprintf("%.4g times", lr)
      else if (is.finite(lr)) sprintf("%.4g times less", 1 / lr)
      else "infinitely")
  )
}

#' Preservation likelihood ratio from measured evidence
#'
#' Convenience wrapper: evaluate measured non-invasive evidence under a natural
#' preservation model (\eqn{H_1}) and an alternative model (\eqn{H_2}) and
#' return the forensic likelihood ratio. Each model is a list with numeric
#' \code{mean} and \code{sd} (recycled over the evidence vector).
#'
#' @param evidence Numeric vector of measured values (e.g. pXRF calcium).
#' @param natural List \code{list(mean=, sd=)} for the natural-preservation
#'   hypothesis \eqn{H_1}.
#' @param alternative List \code{list(mean=, sd=)} for the alternative
#'   hypothesis \eqn{H_2}.
#' @return The list from \code{\link{morie_taphonomy_likelihood_ratio}}, with
#'   \code{loglik_h1} and \code{loglik_h2} attached.
#' @examples
#' morie_taphonomy_preservation_lr(
#'   evidence = c(1200, 1310, 1180),
#'   natural = list(mean = 1250, sd = 90),      # lime-processed signature
#'   alternative = list(mean = 300, sd = 120)   # untreated/decayed signature
#' )$verbal
#' @export
morie_taphonomy_preservation_lr <- function(evidence, natural, alternative) {
  for (m in list(natural, alternative)) {
    if (!all(c("mean", "sd") %in% names(m))) {
      stop("`natural` and `alternative` must each be list(mean=, sd=)",
           call. = FALSE)
    }
  }
  ll1 <- morie_taphonomy_evidence_loglik(evidence, natural$mean, natural$sd)
  ll2 <- morie_taphonomy_evidence_loglik(evidence, alternative$mean,
                                         alternative$sd)
  out <- morie_taphonomy_likelihood_ratio(ll1, ll2)
  out$loglik_h1 <- ll1
  out$loglik_h2 <- ll2
  out
}


# ===========================================================================
# Bayesian preservation model (conjugate + empirical-Bayes hierarchy)
# ===========================================================================

#' Bayesian hierarchical preservation model
#'
#' A conjugate Gaussian-linear Bayesian model for a continuous preservation
#' outcome, with \strong{informative Normal priors} on the coefficients -- so
#' domain knowledge (e.g. quicklime's desiccant effect) enters as a prior and
#' the data updates it to a posterior. The coefficient posterior is closed-form
#' (no MCMC): with prior \eqn{\beta \sim N(m_0, \mathrm{diag}(s_0^2))} and noise
#' variance \eqn{\sigma^2} estimated from the OLS residuals (empirical Bayes),
#' \deqn{\Sigma = (X^\top X / \sigma^2 + \Lambda_0)^{-1}, \quad
#'       \mu = \Sigma (X^\top y / \sigma^2 + \Lambda_0 m_0).}
#'
#' When \code{group} is supplied, a second level is added: group intercepts are
#' partially pooled toward the grand mean by empirical-Bayes (normal-normal)
#' shrinkage \eqn{\lambda_j = \tau^2 / (\tau^2 + \sigma^2 / n_j)}, giving a
#' genuine two-level hierarchical model.
#'
#' For full hierarchical inference by HMC/NUTS, fit \pkg{rstanarm}
#' (\code{stan_glmer}) or \pkg{brms} instead; this function is the
#' dependency-free conjugate core.
#'
#' @param data A non-empty \code{data.frame}.
#' @param outcome Continuous outcome column (default \code{"preservation_score"}).
#' @param covariates Character vector of predictors. Defaults to the schema
#'   covariates + measurements present in \code{data}.
#' @param group Optional column giving a grouping factor (e.g. burial context)
#'   for partial-pooled random intercepts.
#' @param priors Optional named list mapping a coefficient name to
#'   \code{list(mean=, sd=)} -- its informative Normal prior. Unlisted
#'   coefficients get a diffuse prior (\code{sd = prior_sd_default}). Use e.g.
#'   \code{list(lime_treatment = list(mean = 0.3, sd = 0.1))}.
#' @param prior_sd_default Diffuse prior sd for unlisted coefficients
#'   (default 10).
#' @param backend \code{"conjugate"} (default; closed-form posterior, no deps)
#'   or \code{"cmdstanr"} (full-Bayes HMC/NUTS via \pkg{cmdstanr} + a built
#'   CmdStan -- the same model, sampled). The Stan path additionally returns the
#'   \code{stanfit} object.
#' @param chains,iter,seed HMC settings for \code{backend = "cmdstanr"}
#'   (chains, warmup = sampling iterations per chain, RNG seed).
#' @return A named \code{list} (all estimates \code{double}): \code{coefficients}
#'   (\code{data.frame}: term, post_mean, post_sd, ci_lower, ci_upper,
#'   prob_positive), \code{sigma}, \code{group_effects} (NULL unless
#'   \code{group}), \code{fitted} (posterior-predictive mean per row), \code{n},
#'   and a plain-language \code{interpretation}.
#' @references Gelman A, et al. (2013). \emph{Bayesian Data Analysis} (3rd ed.),
#'   Ch. 5 (hierarchical models) & Ch. 14 (conjugate regression). CRC.
#' @examples
#' \donttest{
#' df <- data.frame(preservation_score = rnorm(20),
#'                  lime_treatment = rbinom(20, 1, 0.5))
#' morie_taphonomy_bhm(df, covariates = "lime_treatment",
#'   priors = list(lime_treatment = list(mean = 0.3, sd = 0.1)))$coefficients
#' }
#' @export
morie_taphonomy_bhm <- function(data,
                                outcome = "preservation_score",
                                covariates = NULL,
                                group = NULL,
                                priors = NULL,
                                prior_sd_default = 10,
                                backend = c("conjugate", "cmdstanr"),
                                chains = 4L, iter = 1000L, seed = 42L) {
  backend <- match.arg(backend)
  if (!is.data.frame(data)) stop("`data` must be a data.frame", call. = FALSE)
  if (nrow(data) == 0L) stop("`data` is empty", call. = FALSE)
  if (!outcome %in% names(data)) {
    stop(sprintf("column '%s' not found", outcome), call. = FALSE)
  }
  if (is.null(covariates)) {
    v <- .taphonomy_vars()
    covariates <- intersect(names(c(v$covariates, v$measurements)), names(data))
  }
  covariates <- setdiff(covariates, c(outcome, group))
  if (length(covariates) == 0L) stop("no covariates available", call. = FALSE)

  frame <- stats::na.omit(data[, c(outcome, covariates, group), drop = FALSE])
  y <- as.numeric(frame[[outcome]])
  n <- length(y)
  # numeric design matrix with intercept
  Xc <- lapply(covariates, function(c) {
    col <- frame[[c]]
    if (is.numeric(col)) col else as.numeric(as.factor(col))
  })
  X <- cbind(1, do.call(cbind, Xc))
  terms <- c("(Intercept)", covariates)
  colnames(X) <- terms
  p <- ncol(X)

  # Prior mean m0 and precision Lambda0 = diag(1/s0^2).
  m0 <- numeric(p)
  s0 <- rep(prior_sd_default, p)
  names(m0) <- names(s0) <- terms
  for (nm in names(priors)) {
    if (nm %in% terms) {
      m0[nm] <- priors[[nm]]$mean
      s0[nm] <- priors[[nm]]$sd
    }
  }
  Lambda0 <- diag(1 / s0^2, p, p)

  gfac <- if (!is.null(group)) as.factor(frame[[group]]) else NULL

  # HMC/NUTS backend: same informative-prior hierarchical model, fit by Stan
  # (Chernozhukov-free full Bayes) instead of the conjugate closed form.
  if (backend == "cmdstanr") {
    return(.morie_bhm_cmdstanr(X, y, terms, m0, s0, gfac, group, chains, iter,
                               seed))
  }

  # Empirical-Bayes noise variance from the OLS fit.
  ols <- stats::lm.fit(X, y)
  dof <- max(1L, n - p)
  sigma2 <- sum(ols$residuals^2) / dof
  if (!is.finite(sigma2) || sigma2 <= 0) sigma2 <- stats::var(y)

  # Closed-form Gaussian posterior.
  Sigma <- solve(crossprod(X) / sigma2 + Lambda0)
  mu <- as.numeric(Sigma %*% (crossprod(X, y) / sigma2 + Lambda0 %*% m0))
  post_sd <- sqrt(diag(Sigma))
  z <- 1.959964
  coefs <- data.frame(
    term         = terms,
    post_mean    = mu,
    post_sd      = post_sd,
    ci_lower     = mu - z * post_sd,
    ci_upper     = mu + z * post_sd,
    prob_positive = stats::pnorm(mu / post_sd),
    row.names    = NULL,
    stringsAsFactors = FALSE
  )
  fitted <- as.numeric(X %*% mu)

  # Optional second level: empirical-Bayes partial-pooled random intercepts.
  group_effects <- NULL
  if (!is.null(group)) {
    g <- as.factor(frame[[group]])
    resid <- y - fitted
    gm <- tapply(resid, g, mean)
    nj <- tapply(resid, g, length)
    within <- sigma2
    tau2 <- max(0, stats::var(gm) - mean(within / nj))  # method-of-moments
    lambda <- tau2 / (tau2 + within / nj)                # shrinkage per group
    group_effects <- data.frame(
      group    = names(gm),
      raw_mean = as.numeric(gm),
      shrinkage = as.numeric(lambda),
      pooled_intercept = as.numeric(lambda * gm),  # shrunk toward 0 (grand mean)
      n = as.integer(nj),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }

  lime_row <- coefs[coefs$term %in% c("lime_treatment", covariates[1]), ][1, ]
  list(
    coefficients   = coefs,
    sigma          = sqrt(sigma2),
    group_effects  = group_effects,
    fitted         = fitted,
    n              = n,
    backend        = "conjugate (closed form)",
    interpretation = sprintf(
      paste0("Bayesian preservation model (n=%d, conjugate Gaussian, EB noise ",
             "sd=%.3f). Posterior effect of '%s' = %.3f [%.3f, %.3f], ",
             "P(effect>0)=%.3f. Priors update to posteriors: an informative ",
             "lime prior encodes the desiccant belief, the data revises it.%s"),
      n, sqrt(sigma2), lime_row$term, lime_row$post_mean,
      lime_row$ci_lower, lime_row$ci_upper, lime_row$prob_positive,
      if (is.null(group)) "" else
        sprintf(" %d group intercepts partially pooled.", nrow(group_effects)))
  )
}

# Stan program for the HMC/NUTS backend: the same informative-prior Gaussian
# hierarchical model as the conjugate core (non-centred group intercepts).
.MORIE_TAPHONOMY_BHM_STAN <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  vector[K] prior_mean;
  vector<lower=0>[K] prior_sd;
  int<lower=0> J;
  array[N] int<lower=0> g;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
  vector[J] z;
  real<lower=0> tau;
}
model {
  beta ~ normal(prior_mean, prior_sd);
  sigma ~ exponential(1);
  tau ~ exponential(1);
  z ~ normal(0, 1);
  vector[N] mu = X * beta;
  if (J > 0)
    for (n in 1:N) mu[n] += z[g[n]] * tau;
  y ~ normal(mu, sigma);
}
generated quantities {
  vector[J] group_intercept;
  for (j in 1:J) group_intercept[j] = z[j] * tau;
}
"

# HMC/NUTS fit via cmdstanr, returning the same structure as the conjugate path.
.morie_bhm_cmdstanr <- function(X, y, terms, m0, s0, gfac, group, chains, iter,
                                seed) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("backend = 'cmdstanr' needs the 'cmdstanr' package and a built ",
         "CmdStan. Install: install.packages('cmdstanr', repos = c(",
         "'https://stan-dev.r-universe.dev', getOption('repos'))); ",
         "cmdstanr::install_cmdstan().", call. = FALSE)
  }
  n <- nrow(X)
  J <- if (is.null(gfac)) 0L else nlevels(gfac)
  g <- if (is.null(gfac)) rep(0L, n) else as.integer(gfac)
  standata <- list(N = n, K = ncol(X), X = X, y = as.numeric(y),
                   prior_mean = as.numeric(m0), prior_sd = as.numeric(s0),
                   J = J, g = g)
  mod <- cmdstanr::cmdstan_model(
    cmdstanr::write_stan_file(.MORIE_TAPHONOMY_BHM_STAN))
  fit <- mod$sample(data = standata, chains = chains,
                    parallel_chains = chains, iter_warmup = iter,
                    iter_sampling = iter, seed = seed, refresh = 0,
                    show_messages = FALSE, show_exceptions = FALSE)
  q <- function(x, prob) stats::quantile(x, prob, names = FALSE)
  bs <- fit$summary("beta", mean = mean, sd = stats::sd,
                    ci_lower = \(x) q(x, 0.025), ci_upper = \(x) q(x, 0.975),
                    prob_positive = \(x) mean(x > 0))
  coefs <- data.frame(term = terms, post_mean = bs$mean, post_sd = bs$sd,
                      ci_lower = bs$ci_lower, ci_upper = bs$ci_upper,
                      prob_positive = bs$prob_positive,
                      row.names = NULL, stringsAsFactors = FALSE)
  sigma <- fit$summary("sigma", "mean")$mean
  group_effects <- NULL
  if (J > 0L) {
    gi <- fit$summary("group_intercept", mean = mean, sd = stats::sd)
    group_effects <- data.frame(group = levels(gfac),
                                pooled_intercept = gi$mean, post_sd = gi$sd,
                                n = as.integer(table(gfac)),
                                row.names = NULL, stringsAsFactors = FALSE)
  }
  fitted <- as.numeric(X %*% coefs$post_mean)
  if (J > 0L) fitted <- fitted + group_effects$pooled_intercept[g]
  lime_row <- coefs[coefs$term %in% c("lime_treatment", terms[2]), ][1, ]
  list(
    coefficients = coefs, sigma = sigma, group_effects = group_effects,
    fitted = fitted, n = n, backend = "cmdstanr (NUTS)", stanfit = fit,
    interpretation = sprintf(
      paste0("Bayesian preservation model (n=%d, HMC/NUTS via cmdstanr, %d ",
             "chains x %d draws). Posterior effect of '%s' = %.3f [%.3f, %.3f], ",
             "P(effect>0)=%.3f. Full-Bayes posterior (no conjugacy ",
             "approximation).%s"),
      n, chains, iter, lime_row$term, lime_row$post_mean, lime_row$ci_lower,
      lime_row$ci_upper, lime_row$prob_positive,
      if (J == 0L) "" else sprintf(" %d partially-pooled group intercepts.", J))
  )
}


# ===========================================================================
# Synthetic pXRF (compositional) generation + log-ratio transforms
# ===========================================================================

#' Simulate synthetic pXRF compositional data (Dirichlet)
#'
#' \strong{Synthetic data for testing/calibration ONLY -- never a substitute for
#' real comparanda, and never write the output to \code{inst/extdata}.} Portable
#' X-ray fluorescence yields elemental concentrations that are strictly
#' non-negative and closed (a composition on the simplex), so Gaussian noise is
#' the wrong model; this samples from a \strong{Dirichlet} distribution instead.
#' A control profile (natural soil/bone matrix) and a treatment profile
#' (quicklime -- heavily skewed to calcium) let you exercise the causal /
#' compositional pipeline before real scans exist.
#'
#' Dirichlet variates are drawn via normalised Gamma (\eqn{X_i \sim
#' \mathrm{Gamma}(\alpha_i, 1)}, \eqn{Y = X / \sum X}) so no extra package is
#' needed.
#'
#' @param n Number of samples (rows).
#' @param condition \code{"control"} (natural) or \code{"treatment"} (lime).
#' @param elements Character vector of element names (default Ca, P, Fe, Sr,
#'   Pb, Zn).
#' @param alpha Optional Dirichlet concentration vector (one per element).
#'   Defaults encode the control vs lime profiles; required if you pass custom
#'   \code{elements}.
#' @param seed Optional RNG seed.
#' @param as_ppm If \code{TRUE}, scale proportions to parts-per-million summing
#'   to \code{total_ppm}; else return proportions summing to 1.
#' @param total_ppm Total for the ppm scaling (default 1e6).
#' @return A \code{data.frame}: one column per element, plus \code{condition}
#'   and \code{lime_treatment} (1 = treatment). Attributes \code{elements},
#'   \code{alpha}, and \code{synthetic = TRUE} are attached.
#' @seealso \code{\link{morie_taphonomy_clr}}, \code{\link{morie_taphonomy_ilr}}
#' @examples
#' head(morie_taphonomy_simulate_pxrf(5, "treatment", seed = 1))
#' @export
morie_taphonomy_simulate_pxrf <- function(n,
                                          condition = c("control", "treatment"),
                                          elements = c("Ca", "P", "Fe", "Sr",
                                                       "Pb", "Zn"),
                                          alpha = NULL,
                                          seed = NULL,
                                          as_ppm = FALSE,
                                          total_ppm = 1e6) {
  condition <- match.arg(condition)
  if (!is.null(seed)) set.seed(seed)
  if (is.null(alpha)) {
    alpha <- if (condition == "control") {
      c(30, 15, 5, 1, 0.5, 0.5)          # natural soil/bone matrix
    } else {
      c(85, 5, 2, 0.5, 0.2, 0.2)         # quicklime: heavy calcium spike
    }
    if (length(elements) != length(alpha)) {
      stop("supply `alpha` when using non-default `elements`", call. = FALSE)
    }
  }
  if (length(alpha) != length(elements)) {
    stop("length(alpha) must equal length(elements)", call. = FALSE)
  }
  if (any(alpha <= 0)) stop("`alpha` must be > 0", call. = FALSE)
  D <- length(elements)
  g <- matrix(stats::rgamma(n * D, shape = rep(alpha, each = n), rate = 1),
              nrow = n, ncol = D)
  comp <- g / rowSums(g)
  if (as_ppm) comp <- comp * total_ppm
  out <- as.data.frame(comp)
  names(out) <- elements
  out$condition <- condition
  out$lime_treatment <- as.integer(condition == "treatment")
  attr(out, "elements") <- elements
  attr(out, "alpha") <- alpha
  attr(out, "synthetic") <- TRUE
  out
}

# Close a composition matrix to the simplex, guarding zeros with a pseudocount.
.taphonomy_close <- function(x, pseudocount) {
  X <- as.matrix(x)
  if (!is.numeric(X)) stop("`x` must be a numeric composition", call. = FALSE)
  if (any(X < 0)) stop("compositions must be non-negative", call. = FALSE)
  X <- X + pseudocount
  X / rowSums(X)
}

#' Centred log-ratio (CLR) transform of compositional data
#'
#' Maps closed compositions off the simplex into real space via
#' \eqn{\mathrm{clr}(x) = \log x - \overline{\log x}} (row-wise). Removes the
#' closure so downstream Gaussian/causal models are not fed singular covariance.
#' CLR is rank-deficient (columns sum to zero); for regression/DML inputs prefer
#' \code{\link{morie_taphonomy_ilr}}.
#'
#' @param x Numeric matrix/data.frame of compositions (rows = samples).
#' @param pseudocount Small value added before the log to guard zeros
#'   (default 1e-6).
#' @return A numeric matrix (same shape) of CLR coordinates.
#' @references Aitchison J (1986). \emph{The Statistical Analysis of
#'   Compositional Data}. Chapman & Hall.
#' @examples
#' morie_taphonomy_clr(morie_taphonomy_simulate_pxrf(3, seed = 1)[, 1:6])
#' @export
morie_taphonomy_clr <- function(x, pseudocount = 1e-6) {
  X <- .taphonomy_close(x, pseudocount)
  L <- log(X)
  clr <- L - rowMeans(L)
  colnames(clr) <- colnames(X)
  clr
}

#' Isometric log-ratio (ILR) transform of compositional data
#'
#' Maps a \eqn{D}-part composition to \eqn{D-1} unconstrained, orthonormal
#' coordinates -- unlike CLR these are \strong{full rank}, so they can go
#' straight into \code{\link{morie_taphonomy_preservation_delta}} /
#' \code{\link{morie_taphonomy_bhm}} without singular design matrices. Uses the
#' Egozcue et al. (2003) pivot-coordinate basis
#' \deqn{\mathrm{ilr}_i(x) = \sqrt{\tfrac{i}{i+1}}\,
#'   \log\frac{(\prod_{k\le i} x_k)^{1/i}}{x_{i+1}},}
#' computed in closed form so R and Python return identical values.
#'
#' @param x Numeric matrix/data.frame of compositions (>= 2 parts).
#' @param pseudocount Zero-guarding pseudocount (default 1e-6).
#' @return A numeric matrix with \code{ncol(x) - 1} columns (\code{ilr1}...).
#' @references Egozcue JJ, et al. (2003). Isometric logratio transformations
#'   for compositional data analysis. \emph{Mathematical Geology} 35(3),
#'   279--300. \doi{10.1023/A:1023818214614}
#' @examples
#' morie_taphonomy_ilr(morie_taphonomy_simulate_pxrf(3, seed = 1)[, 1:6])
#' @export
morie_taphonomy_ilr <- function(x, pseudocount = 1e-6) {
  X <- .taphonomy_close(x, pseudocount)
  D <- ncol(X)
  if (D < 2L) stop("need >= 2 parts for ILR", call. = FALSE)
  L <- log(X)
  ilr <- vapply(seq_len(D - 1L), function(i) {
    sqrt(i / (i + 1)) * (rowMeans(L[, seq_len(i), drop = FALSE]) - L[, i + 1L])
  }, numeric(nrow(X)))
  if (is.null(dim(ilr))) ilr <- matrix(ilr, nrow = nrow(X))
  colnames(ilr) <- paste0("ilr", seq_len(D - 1L))
  ilr
}


# ===========================================================================
# Open-data ingestion: real comparanda for calibration
# ===========================================================================

# Default USGS National Geochemical Database (soil) bulk CSV (verified
# 2026-07-03; 54 MB zip -> 482 MB CSV; subject to federal-portal reorg).
.MORIE_USGS_NGDBSOIL_URL <- "https://mrdata.usgs.gov/ngdb/soil/ngdbsoil-csv.zip"

# Read the CSV out of a downloaded ngdbsoil zip WITHOUT extracting the full
# 482 MB (base R `unz()` streams the member). Split out for testing.
.morie_read_usgs_soil_zip <- function(zip_path, nrows = NULL) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  csv <- grep("\\.csv$", members, value = TRUE, ignore.case = TRUE)[1]
  if (is.na(csv)) stop("no CSV member found in ", basename(zip_path),
                       call. = FALSE)
  # read.csv opens AND closes the unz() connection it is handed; do not close
  # it again here (double-close errors on an already-closed connection).
  utils::read.csv(unz(zip_path, csv),
                  nrows = if (is.null(nrows)) -1L else as.integer(nrows),
                  check.names = FALSE, stringsAsFactors = FALSE)
}

#' Fetch USGS National Geochemical Database soil geochemistry
#'
#' Downloads the U.S. Geological Survey National Geochemical Database (soil)
#' bulk CSV -- \strong{real, open} elemental concentrations (Ca, Fe, ...) that
#' are the compositional analogue of pXRF bone/relic spectra. Use it to
#' calibrate the compositional pipeline (\code{\link{morie_taphonomy_clr}} /
#' \code{\link{morie_taphonomy_ilr}} / DML) on genuine open data before real
#' scans exist. Dependency-free: the CSV is read straight from the zip with base
#' R (no GDAL/\pkg{sf}); the WFS endpoint is GML-only and deliberately not used.
#'
#' The download is ~54 MB (482 MB uncompressed); it is cached in \code{dest} and
#' reused unless \code{refresh = TRUE}. Network + size mean this never runs in
#' examples/tests.
#'
#' @param dest Cache directory for the zip (default \code{tempdir()}; never
#'   \code{~}).
#' @param nrows Rows to read (default 1000 for a quick slice; \code{NULL} = all
#'   1.5M+).
#' @param url Source URL (default the USGS NGDB soil zip).
#' @param refresh Re-download even if the cached zip exists.
#' @return A \code{data.frame} of soil samples with elemental-concentration
#'   columns; \code{attr(., "source")} records the URL.
#' @source \url{https://mrdata.usgs.gov/ngdb/soil/}
#' @seealso \code{\link{morie_taphonomy_clr}}, \code{\link{morie_taphonomy_ilr}}
#' @examples
#' \donttest{
#' # ~54 MB download:
#' # soil <- morie_taphonomy_fetch_usgs_soil(nrows = 500)
#' }
#' @export
morie_taphonomy_fetch_usgs_soil <- function(dest = tempdir(),
                                            nrows = 1000L,
                                            url = .MORIE_USGS_NGDBSOIL_URL,
                                            refresh = FALSE) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  zip_path <- file.path(dest, basename(url))
  if (refresh || !file.exists(zip_path)) {
    utils::download.file(url, zip_path, mode = "wb", quiet = TRUE)
  }
  df <- .morie_read_usgs_soil_zip(zip_path, nrows)
  attr(df, "source") <- url
  df
}

#' STO-2022 taphonomic-observation schema (PMI nuisance variables)
#'
#' A typed, zero-row template of the taphonomic-observation and environmental
#' variables used for post-mortem-interval (PMI) work, aligned with the Standard
#' for Taphonomic Observations in Support of the PMI (2022) -- the same variable
#' family the geoFOR project records. \strong{geoFOR itself is an interactive
#' web application with no open data API}, so this schema lets you structure your
#' own case observations into the high-dimensional nuisance set \eqn{X} for the
#' DML / Bayesian estimators (baseline expected decay given environment). No rows
#' are fabricated.
#'
#' @return A zero-row \code{data.frame}; each column carries a \code{"role"}
#'   attribute (\code{"observation"}, \code{"environment"}, or \code{"outcome"}).
#' @source geoFOR (\url{https://www.geoforapp.info}); Standard for Taphonomic
#'   Observations in Support of the PMI (2022).
#' @examples
#' str(morie_taphonomy_pmi_schema())
#' @export
morie_taphonomy_pmi_schema <- function() {
  spec <- c(
    decomp_stage        = "integer",  # ordinal TBS-style stage
    body_scoring_tbs    = "numeric",  # Total Body Score
    accumulated_deg_days = "numeric", # ADD (thermal history)
    temp_c              = "numeric",
    humidity_pct        = "numeric",
    precipitation_mm    = "numeric",
    burial_depth_cm     = "numeric",
    soil_ph             = "numeric",
    scavenger_activity  = "integer",  # 1 = present
    insect_activity     = "integer",  # 1 = present
    pmi_days            = "numeric"   # outcome
  )
  roles <- c(rep("observation", 2), rep("environment", 8), "outcome")
  cols <- Map(function(t) switch(t, integer = integer(0), numeric(0)), spec)
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  attr(df, "role") <- stats::setNames(roles, names(spec))
  df
}


# ===========================================================================
# MorphoSource client -- 3D bioarchaeology media (user-supplied API key)
# ===========================================================================
#
# Endpoints + auth verified 2026-07-03 against MorphoSource's own client
# (github.com/Imageomics/pyMorphoSource): base https://www.morphosource.org/api
# (override MORPHOSOURCE_API_URL); search = GET /media|/physical-objects with
# q + search_field=all_fields + f.<facet> + per_page/page; download = POST
# /download/<id> with an Authorization: <key> header + a use-statement body,
# returning response.media.download_url. The key is read from the caller's own
# environment -- never hard-coded, never in the URL/argv.

# Resolve the API base (env override -> default).
.morie_morphosource_api <- function() {
  base <- Sys.getenv("MORPHOSOURCE_API_URL", unset = "")
  if (nzchar(base)) base else "https://www.morphosource.org/api"
}

# Resolve the user's API key: explicit arg -> MORPHOSOURCE_API_KEY env.
# `required = FALSE` for public search; TRUE for downloads.
.morie_morphosource_key <- function(api_key = NULL, required = TRUE) {
  key <- if (!is.null(api_key) && nzchar(api_key)) {
    api_key
  } else {
    Sys.getenv("MORPHOSOURCE_API_KEY", unset = "")
  }
  if (!nzchar(key)) {
    if (required) {
      stop("MorphoSource API key required. Pass `api_key=` or export ",
           "MORPHOSOURCE_API_KEY (get a token from your account at ",
           "https://www.morphosource.org). Never hard-code the key.",
           call. = FALSE)
    }
    return(NULL)
  }
  key
}

# Build the GET search query list (testable without network).
.morie_morphosource_search_params <- function(query = NULL, media_type = NULL,
                                              taxonomy_gbif = NULL,
                                              visibility = NULL, media_tag = NULL,
                                              per_page = 10L, page = 1L) {
  params <- list()
  if (!is.null(query) && nzchar(query)) {
    params[["q"]] <- query
    params[["search_field"]] <- "all_fields"
  }
  facets <- list(media_type = media_type, taxonomy_gbif = taxonomy_gbif,
                 publication_status = visibility, tag = media_tag)
  for (k in names(facets)) {
    if (!is.null(facets[[k]])) params[[paste0("f.", k)]] <- facets[[k]]
  }
  params[["per_page"]] <- as.integer(per_page)
  params[["page"]] <- as.integer(page)
  params
}

#' Search MorphoSource for 3D media or physical objects
#'
#' Queries the open MorphoSource repository (CT / micro-CT / 3D scans of
#' skeletal and occasionally mummified remains) -- real bioarchaeology comparanda
#' for the taphonomy pipeline. Public search needs no key; a key (for restricted
#' records) is read from \code{api_key} or the \code{MORPHOSOURCE_API_KEY}
#' environment variable, sent in an \code{Authorization} header (never the URL).
#'
#' @param query Free-text query (searches all fields).
#' @param type \code{"media"} (default) or \code{"physical-objects"}.
#' @param media_type,taxonomy_gbif,visibility,media_tag Optional facet filters.
#' @param per_page,page Pagination (default 10 / 1).
#' @param api_key Optional key; else \code{MORPHOSOURCE_API_KEY}. Never
#'   hard-code it.
#' @return A \code{list}: \code{items} (raw records), \code{n},
#'   \code{total_pages}, and \code{df} (a \code{data.frame} of \code{id} +
#'   \code{title} where present).
#' @source \url{https://www.morphosource.org}; API verified against
#'   \url{https://github.com/Imageomics/pyMorphoSource}.
#' @seealso \code{\link{morie_taphonomy_morphosource_fetch}}
#' @examples
#' \donttest{
#' # res <- morie_taphonomy_morphosource_search("Homo sapiens cranium")
#' }
#' @export
morie_taphonomy_morphosource_search <- function(query = NULL,
                                                type = c("media",
                                                         "physical-objects"),
                                                media_type = NULL,
                                                taxonomy_gbif = NULL,
                                                visibility = NULL,
                                                media_tag = NULL,
                                                per_page = 10L, page = 1L,
                                                api_key = NULL) {
  type <- match.arg(type)
  url <- paste0(.morie_morphosource_api(), "/", type)
  params <- .morie_morphosource_search_params(query, media_type, taxonomy_gbif,
                                              visibility, media_tag, per_page,
                                              page)
  key <- .morie_morphosource_key(api_key, required = FALSE)
  headers <- if (!is.null(key)) paste0("Authorization: ", key) else character()
  resp <- .morie_dataset_http_text_with_status(url, query = params,
                                               headers = headers)
  if (resp$status_code >= 400L) {
    stop("MorphoSource search failed (HTTP ", resp$status_code, ")",
         call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)$response
  items_name <- if (type == "media") "media" else "physical_objects"
  items <- parsed[[items_name]]
  if (is.null(items)) items <- list()
  ids <- vapply(items, function(x) as.character(x$id %||% NA), character(1))
  titles <- vapply(items, function(x) as.character(x$title %||% NA), character(1))
  list(
    items = items,
    n = length(items),
    total_pages = parsed$pages$total_pages %||% NA_integer_,
    df = data.frame(id = ids, title = titles, stringsAsFactors = FALSE)
  )
}

#' Download a MorphoSource media bundle
#'
#' Resolves and downloads a media bundle by id. MorphoSource enforces a
#' \strong{data-use agreement} on every download, so \code{use_statement} is
#' required and \code{agreements_accepted} is sent as \code{TRUE}; restricted
#' media additionally need per-item permission granted on the website. The key
#' comes from \code{api_key} or \code{MORPHOSOURCE_API_KEY} and travels only in
#' the \code{Authorization} header.
#'
#' @param media_id MorphoSource media id.
#' @param use_statement Required free-text statement of intended use.
#' @param use_categories Optional character vector of use categories.
#' @param use_category_other Optional free-text for the "other" category.
#' @param dest Directory to write the bundle (default \code{tempdir()}).
#' @param api_key Optional key; else \code{MORPHOSOURCE_API_KEY}.
#' @return Path to the downloaded \code{.zip} bundle.
#' @source \url{https://www.morphosource.org}
#' @seealso \code{\link{morie_taphonomy_morphosource_search}}
#' @examples
#' \donttest{
#' # morie_taphonomy_morphosource_fetch(
#' #   media_id = 000000, use_statement = "Non-commercial taphonomy research")
#' }
#' @export
morie_taphonomy_morphosource_fetch <- function(media_id, use_statement,
                                               use_categories = NULL,
                                               use_category_other = NULL,
                                               dest = tempdir(),
                                               api_key = NULL) {
  if (missing(use_statement) || !is.character(use_statement) ||
      !nzchar(use_statement)) {
    stop("MorphoSource requires a non-empty `use_statement` (data-use ",
         "agreement).", call. = FALSE)
  }
  key <- .morie_morphosource_key(api_key, required = TRUE)
  headers <- paste0("Authorization: ", key)
  url <- paste0(.morie_morphosource_api(), "/download/", media_id)
  body <- list(use_statement = use_statement, agreements_accepted = TRUE)
  if (!is.null(use_categories)) body$use_categories <- use_categories
  if (!is.null(use_category_other)) body$use_category_other <- use_category_other

  resp <- .morie_dataset_http_post_json_with_status(url, body = body,
                                                    headers = headers)
  if (resp$status_code == 403L) {
    stop("Restricted media: request download permission at ",
         "https://www.morphosource.org for media id ", media_id, call. = FALSE)
  }
  if (resp$status_code >= 400L) {
    stop("MorphoSource download request failed (HTTP ", resp$status_code, ")",
         call. = FALSE)
  }
  dl_url <- jsonlite::fromJSON(resp$body,
                               simplifyVector = FALSE)$response$media$download_url
  if (is.null(dl_url) || !nzchar(dl_url)) {
    stop("MorphoSource returned no download_url", call. = FALSE)
  }
  bytes <- .morie_dataset_http_bytes(dl_url, headers = headers)
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  path <- file.path(dest, paste0("morphosource_", media_id, ".zip"))
  writeBin(bytes, path)
  path
}
