# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multiple testing correction and multiplicity-adjusted inference
#'
#' R port of \code{morie.multiple_testing}. Provides p-value adjustment
#' methods controlling the family-wise error rate (FWER) and the false
#' discovery rate (FDR), simultaneous-inference helpers, p-value
#' combination procedures, and gatekeeping / hierarchical testing.
#'
#' Every adjustment routine returns a \code{morie_rich_result} list
#' carrying \code{original}, \code{adjusted}, \code{rejected},
#' \code{method}, \code{alpha}, \code{n_rejected}, \code{n_tests}, and
#' an \code{interpretation} paragraph. P-value combination procedures
#' return a list with the test statistic, combined p-value, and
#' interpretation.
#'
#' FWER and FDR methods delegate to \code{stats::p.adjust} whenever an
#' exact equivalent is available (Bonferroni, Holm, Hochberg, Hommel,
#' Benjamini-Hochberg, Benjamini-Yekutieli).
#'
#' @name morie_multiple_testing
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

.mt_result <- function(title, call, summary_lines = list(),
                       warnings = character(0),
                       interpretation = "",
                       ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    warnings = warnings,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_multiple_testing_result", "morie_rich_result", "list")
  out
}

.mt_adjusted <- function(method, p, alpha, adjusted, labels = NULL,
                         note = NULL) {
  p <- as.numeric(p)
  m <- length(p)
  adjusted <- pmin(pmax(adjusted, 0), 1)
  rejected <- adjusted <= alpha
  n_rej <- as.integer(sum(rejected, na.rm = TRUE))

  reject_text <- if (n_rej == 0L) {
    sprintf("No hypotheses are rejected at alpha=%.4f.", alpha)
  } else if (n_rej == 1L) {
    sprintf("1 of %d hypotheses is rejected at alpha=%.4f.", m, alpha)
  } else {
    sprintf("%d of %d hypotheses are rejected at alpha=%.4f.", n_rej, m, alpha)
  }
  interp <- if (is.null(note)) reject_text else paste(reject_text, note)

  .mt_result(
    title = sprintf("Adjusted p-values (%s)", method),
    call = sprintf("method=%s, alpha=%.4f, n=%d", method, alpha, m),
    summary_lines = list(
      Method = method,
      alpha = alpha,
      `Tests (n)` = m,
      Rejected = n_rej,
      `Min adjusted p` = if (m > 0L) min(adjusted, na.rm = TRUE) else NA_real_,
      `Max adjusted p` = if (m > 0L) max(adjusted, na.rm = TRUE) else NA_real_
    ),
    interpretation = interp,
    original = p,
    adjusted = adjusted,
    rejected = rejected,
    method = method,
    alpha = alpha,
    n_rejected = n_rej,
    n_tests = as.integer(m),
    labels = labels
  )
}

.mt_check_p <- function(p) {
  p <- as.numeric(p)
  if (length(p) == 0L) {
    stop("p_values is empty")
  }
  if (any(!is.finite(p))) {
    stop("p_values contain non-finite entries")
  }
  if (any(p < 0 - 1e-12) || any(p > 1 + 1e-12)) {
    stop("p_values must lie in [0, 1]")
  }
  pmin(pmax(p, 0), 1)
}


# ---------------------------------------------------------------------------
# FWER-controlling procedures
# ---------------------------------------------------------------------------

#' Bonferroni FWER correction
#'
#' Wraps \code{stats::p.adjust(method = "bonferroni")}.
#'
#' @param p_values Numeric vector of raw p-values.
#' @param alpha Significance level.
#' @param labels Optional character vector of test labels.
#' @return A \code{morie_rich_result} list (see \code{morie_multiple_testing}).
#' @export
bonferroni <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "bonferroni")
  .mt_adjusted("bonferroni", p, alpha, adj, labels)
}

#' Sidak FWER correction
#'
#' Slightly less conservative than Bonferroni under independence.
#'
#' @inheritParams bonferroni
#' @export
sidak <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  adj <- 1.0 - (1.0 - p) ^ m
  .mt_adjusted("sidak", p, alpha, adj, labels)
}

#' Holm step-down FWER procedure
#'
#' Wraps \code{stats::p.adjust(method = "holm")}; uniformly more
#' powerful than Bonferroni.
#'
#' @inheritParams bonferroni
#' @export
holm <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "holm")
  .mt_adjusted("holm", p, alpha, adj, labels)
}

#' Hochberg step-up FWER procedure
#'
#' Wraps \code{stats::p.adjust(method = "hochberg")}.
#'
#' @inheritParams bonferroni
#' @export
hochberg <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "hochberg")
  .mt_adjusted("hochberg", p, alpha, adj, labels)
}

#' Hommel FWER procedure
#'
#' Wraps \code{stats::p.adjust(method = "hommel")}.
#'
#' @inheritParams bonferroni
#' @export
hommel <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "hommel")
  .mt_adjusted("hommel", p, alpha, adj, labels)
}

#' Holm-Sidak step-down procedure
#'
#' @inheritParams bonferroni
#' @export
holm_sidak <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  ord <- order(p)
  sp <- p[ord]
  adj_sorted <- 1.0 - (1.0 - sp) ^ seq(m, 1)
  # Enforce monotonicity (step-down)
  for (i in seq_len(m)[-1]) {
    adj_sorted[i] <- max(adj_sorted[i], adj_sorted[i - 1])
  }
  adj <- numeric(m)
  adj[ord] <- adj_sorted
  .mt_adjusted("holm_sidak", p, alpha, adj, labels)
}


# ---------------------------------------------------------------------------
# FDR-controlling procedures
# ---------------------------------------------------------------------------

#' Benjamini-Hochberg FDR control
#'
#' Wraps \code{stats::p.adjust(method = "BH")}.
#'
#' @inheritParams bonferroni
#' @export
benjamini_hochberg <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "BH")
  .mt_adjusted("benjamini_hochberg", p, alpha, adj, labels)
}

#' @rdname benjamini_hochberg
#' @export
bh <- benjamini_hochberg

#' Benjamini-Yekutieli FDR control under arbitrary dependence
#'
#' Wraps \code{stats::p.adjust(method = "BY")}.
#'
#' @inheritParams bonferroni
#' @export
benjamini_yekutieli <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  adj <- stats::p.adjust(p, method = "BY")
  .mt_adjusted("benjamini_yekutieli", p, alpha, adj, labels)
}

#' @rdname benjamini_yekutieli
#' @export
by_fdr <- benjamini_yekutieli

#' Storey q-value procedure (adaptive FDR)
#'
#' Estimates the proportion of true null hypotheses (pi0) and
#' tightens the BH thresholds by that factor.
#'
#' @inheritParams bonferroni
#' @param lambda_param Tuning parameter in (0, 1) for the pi0 estimator.
#' @export
storey_q <- function(p_values, alpha = 0.05, lambda_param = 0.5,
                     labels = NULL) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  pi0 <- sum(p > lambda_param) / (m * (1.0 - lambda_param))
  pi0 <- min(pi0, 1.0)

  ord <- order(p)
  sp <- p[ord]
  q_sorted <- sp * pi0 * m / seq_len(m)
  for (i in seq(m - 1L, 1L, by = -1L)) {
    q_sorted[i] <- min(q_sorted[i], q_sorted[i + 1L])
  }
  q_sorted <- pmin(q_sorted, 1.0)
  adj <- numeric(m)
  adj[ord] <- q_sorted

  note <- sprintf("Storey pi0 estimate is %.3f (lambda=%.2f).",
                  pi0, lambda_param)
  out <- .mt_adjusted(sprintf("storey_q(pi0=%.3f)", pi0),
                      p, alpha, adj, labels, note = note)
  out$pi0 <- pi0
  out$lambda_param <- lambda_param
  out
}


# ---------------------------------------------------------------------------
# Combining p-values
# ---------------------------------------------------------------------------

.mt_combine_result <- function(method, stat, p_comb, interp,
                                extra = list()) {
  out <- list(
    title = sprintf("Combined p-value (%s)", method),
    call = sprintf("method=%s", method),
    summary_lines = c(
      list(
        Method = method,
        Statistic = stat,
        `Combined p` = p_comb
      ),
      extra
    ),
    warnings = character(0),
    interpretation = interp,
    method = method,
    statistic = stat,
    p_value = p_comb
  )
  class(out) <- c("morie_multiple_testing_result", "morie_rich_result", "list")
  out
}

#' Fisher's method for combining independent p-values
#' @inheritParams bonferroni
#' @export
fisher_combined <- function(p_values) {
  p <- .mt_check_p(p_values)
  p <- pmax(p, 1e-300)
  chi2 <- -2.0 * sum(log(p))
  p_comb <- stats::pchisq(chi2, df = 2L * length(p), lower.tail = FALSE)
  interp <- sprintf(
    "Fisher's combination of %d p-values yields chi-square=%.4f on %d df with combined p=%.4g.",
    length(p), chi2, 2L * length(p), p_comb
  )
  .mt_combine_result("fisher", chi2, p_comb, interp,
                      list(`df` = 2L * length(p)))
}

#' Stouffer's z-score method
#'
#' @inheritParams bonferroni
#' @param weights Optional non-negative weights (any scale).
#' @export
stouffer_combined <- function(p_values, weights = NULL) {
  p <- .mt_check_p(p_values)
  p <- pmin(pmax(p, 1e-300), 1 - 1e-15)
  z <- stats::qnorm(1 - p)
  if (is.null(weights)) {
    z_comb <- sum(z) / sqrt(length(z))
  } else {
    w <- as.numeric(weights)
    z_comb <- sum(w * z) / sqrt(sum(w * w))
  }
  p_comb <- stats::pnorm(z_comb, lower.tail = FALSE)
  interp <- sprintf(
    "Stouffer's combination of %d p-values gives Z=%.4f and combined p=%.4g.",
    length(p), z_comb, p_comb
  )
  .mt_combine_result("stouffer", z_comb, p_comb, interp)
}

#' Tippett's minimum-p method
#'
#' @inheritParams bonferroni
#' @export
tippett_combined <- function(p_values) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  mn <- min(p)
  p_comb <- 1.0 - (1.0 - mn) ^ m
  interp <- sprintf(
    "Tippett's minimum-p across %d tests is %.4g, giving combined p=%.4g.",
    m, mn, p_comb
  )
  .mt_combine_result("tippett", mn, p_comb, interp)
}

#' Simes test for the global null
#'
#' @inheritParams bonferroni
#' @export
simes_combined <- function(p_values) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  sp <- sort(p)
  simes_vals <- sp * m / seq_len(m)
  s <- min(simes_vals)
  interp <- sprintf(
    "Simes statistic on %d ordered p-values is %.4g (this is also the combined p-value).",
    m, s
  )
  .mt_combine_result("simes", s, s, interp)
}

#' Harmonic mean p-value
#'
#' For tests that may be dependent.
#'
#' @inheritParams bonferroni
#' @export
harmonic_mean_p <- function(p_values) {
  p <- .mt_check_p(p_values)
  p <- pmax(p, 1e-300)
  m <- length(p)
  hmp <- m / sum(1.0 / p)
  hmp
}

#' Cauchy combination test (Liu and Xie 2020)
#'
#' Robust to arbitrary correlation structure.
#'
#' @inheritParams bonferroni
#' @param weights Optional non-negative weights summing to 1.
#' @export
cauchy_combination <- function(p_values, weights = NULL) {
  p <- .mt_check_p(p_values)
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  m <- length(p)
  if (is.null(weights)) {
    w <- rep(1.0 / m, m)
  } else {
    w <- as.numeric(weights)
    w <- w / sum(w)
  }
  tvals <- tan((0.5 - p) * pi)
  t_stat <- sum(w * tvals)
  p_comb <- stats::pcauchy(t_stat, lower.tail = FALSE)
  interp <- sprintf(
    "Cauchy combination across %d weighted tests gives T=%.4f and combined p=%.4g.",
    m, t_stat, p_comb
  )
  .mt_combine_result("cauchy", t_stat, p_comb, interp)
}


# ---------------------------------------------------------------------------
# Gatekeeping / hierarchical testing
# ---------------------------------------------------------------------------

#' Fixed-sequence (predetermined order) testing
#'
#' Tests are evaluated in the given order and the procedure stops at
#' the first non-rejection; reached hypotheses need no multiplicity
#' adjustment.
#'
#' @inheritParams bonferroni
#' @export
fixed_sequence <- function(p_values, alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  rejected <- logical(m)
  for (i in seq_len(m)) {
    if (p[i] <= alpha) {
      rejected[i] <- TRUE
    } else {
      break
    }
  }
  first_fail <- which(!rejected)[1]
  note <- if (is.na(first_fail)) {
    "All hypotheses in the predetermined sequence were rejected."
  } else if (first_fail == 1L) {
    sprintf("The first hypothesis failed (p=%.4g > alpha=%.4f); no rejections.",
            p[1], alpha)
  } else {
    sprintf("Procedure stopped at position %d (p=%.4g > alpha=%.4f); first %d hypotheses rejected.",
            first_fail, p[first_fail], alpha, first_fail - 1L)
  }
  out <- .mt_adjusted("fixed_sequence", p, alpha, p, labels, note = note)
  out$rejected <- rejected
  out$n_rejected <- as.integer(sum(rejected))
  out
}

#' Fallback (fixed-sequence with alpha spending)
#'
#' @inheritParams bonferroni
#' @param weights Numeric vector of non-negative weights summing to 1.
#' @export
fallback_procedure <- function(p_values, weights, alpha = 0.05,
                                labels = NULL) {
  p <- .mt_check_p(p_values)
  w <- as.numeric(weights)
  if (length(w) != length(p)) {
    stop("weights must have the same length as p_values")
  }
  m <- length(p)
  allocated <- w * alpha
  rejected <- logical(m)
  carry_over <- 0.0
  for (i in seq_len(m)) {
    threshold <- allocated[i] + carry_over
    if (p[i] <= threshold) {
      rejected[i] <- TRUE
      carry_over <- threshold - p[i]
    } else {
      carry_over <- threshold
    }
  }
  note <- sprintf("Fallback alpha spending across %d steps; %d rejected.",
                  m, sum(rejected))
  out <- .mt_adjusted("fallback", p, alpha, p, labels, note = note)
  out$rejected <- rejected
  out$n_rejected <- as.integer(sum(rejected))
  out$weights <- w
  out
}

#' Hierarchical (serial gatekeeping) Bonferroni procedure
#'
#' Families are tested in order; if a family produces no rejections,
#' subsequent families are blocked from testing.
#'
#' @param p_values_by_family List of numeric vectors, one per family.
#' @param alpha Overall FWER level.
#' @param propagate_alpha Logical; currently keeps alpha constant
#'   across families (mirrors the Python reference).
#' @return A \code{morie_rich_result} list with one stage entry per
#'   family and an \code{overall_rejected} logical vector.
#' @export
hierarchical_bonferroni <- function(p_values_by_family, alpha = 0.05,
                                     propagate_alpha = TRUE) {
  if (!is.list(p_values_by_family)) {
    stop("p_values_by_family must be a list of numeric vectors")
  }
  n_families <- length(p_values_by_family)
  stages <- vector("list", 0L)
  all_rejected <- logical(0)
  remaining_alpha <- alpha
  gate_closed <- FALSE

  for (i in seq_len(n_families)) {
    family_p <- as.numeric(p_values_by_family[[i]])
    m <- length(family_p)
    if (gate_closed) {
      stages[[length(stages) + 1L]] <- list(
        family = i, n_tests = m,
        alpha_used = 0.0, n_rejected = 0L,
        adjusted_p = rep(1.0, m),
        rejected = logical(m)
      )
      all_rejected <- c(all_rejected, logical(m))
      next
    }
    adj <- bonferroni(family_p, alpha = remaining_alpha)
    stages[[length(stages) + 1L]] <- list(
      family = i, n_tests = m,
      alpha_used = remaining_alpha,
      n_rejected = adj$n_rejected,
      adjusted_p = adj$adjusted,
      rejected = adj$rejected
    )
    all_rejected <- c(all_rejected, adj$rejected)
    if (adj$n_rejected == 0L) {
      gate_closed <- TRUE
    }
  }

  total_rej <- sum(all_rejected)
  interp <- sprintf(
    "Serial gatekeeping across %d famil(ies) with alpha=%.4f yielded %d total rejection(s).%s",
    n_families, alpha, total_rej,
    if (gate_closed) " A downstream gate was closed by an empty family." else ""
  )

  out <- list(
    title = "Hierarchical Bonferroni Gatekeeping",
    call = sprintf("families=%d, alpha=%.4f", n_families, alpha),
    summary_lines = list(
      Families = n_families,
      alpha = alpha,
      `Total rejected` = as.integer(total_rej),
      `Gate closed` = gate_closed
    ),
    warnings = character(0),
    interpretation = interp,
    stages = stages,
    overall_rejected = all_rejected,
    method = "hierarchical_bonferroni",
    alpha = alpha
  )
  class(out) <- c("morie_multiple_testing_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

#' Estimate the proportion of true null hypotheses (pi0)
#'
#' @inheritParams bonferroni
#' @param method One of \code{"storey"}, \code{"bootstrap"}, or
#'   \code{"two_step"}.
#' @return A scalar pi0 estimate in `[0, 1]`.
#' @export
estimate_pi0 <- function(p_values, method = c("storey", "bootstrap", "two_step")) {
  method <- match.arg(method)
  p <- .mt_check_p(p_values)
  m <- length(p)
  lambdas <- seq(0.05, 0.90, by = 0.05)

  if (method == "storey") {
    estimates <- vapply(lambdas, function(lam) {
      sum(p > lam) / (m * (1 - lam))
    }, numeric(1))
    return(min(min(estimates), 1.0))
  }

  if (method == "bootstrap") {
    pi0_hat <- vapply(lambdas, function(lam) {
      sum(p > lam) / (m * (1 - lam))
    }, numeric(1))
    min_pi0 <- stats::quantile(pi0_hat, 0.10, names = FALSE)
    mse <- numeric(length(lambdas))
    for (b in seq_len(100L)) {
      p_boot <- sample(p, size = m, replace = TRUE)
      pi0_boot <- vapply(lambdas, function(lam) {
        sum(p_boot > lam) / (m * (1 - lam))
      }, numeric(1))
      mse <- mse + (pi0_boot - min_pi0) ^ 2
    }
    mse <- mse / 100
    return(min(pi0_hat[which.min(mse)], 1.0))
  }

  # two_step
  bh_res <- benjamini_hochberg(p, alpha = 0.05)
  r <- bh_res$n_rejected
  if (r == 0L) {
    return(1.0)
  }
  min((m - r) / m, 1.0)
}

#' Convenience dispatcher for p-value adjustment methods
#'
#' @inheritParams bonferroni
#' @param method One of \code{"bonferroni"}, \code{"sidak"},
#'   \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
#'   \code{"holm_sidak"}, \code{"bh"} / \code{"benjamini_hochberg"} /
#'   \code{"fdr"}, \code{"by"} / \code{"benjamini_yekutieli"},
#'   \code{"storey"}, or \code{"fwer"} (alias of holm).
#' @export
adjust_p_values <- function(p_values, method = "bh", alpha = 0.05,
                             labels = NULL) {
  method <- tolower(method)
  dispatch <- list(
    bonferroni = bonferroni,
    sidak = sidak,
    holm = holm,
    hochberg = hochberg,
    hommel = hommel,
    holm_sidak = holm_sidak,
    bh = benjamini_hochberg,
    benjamini_hochberg = benjamini_hochberg,
    fdr = benjamini_hochberg,
    by = benjamini_yekutieli,
    benjamini_yekutieli = benjamini_yekutieli,
    storey = storey_q,
    fwer = holm
  )
  fn <- dispatch[[method]]
  if (is.null(fn)) {
    stop(sprintf("Unknown method '%s'. Available: %s",
                 method, paste(names(dispatch), collapse = ", ")))
  }
  fn(p_values, alpha = alpha, labels = labels)
}

#' Effective number of independent tests from a correlation matrix
#'
#' @param correlation_matrix Square symmetric correlation matrix.
#' @param method One of \code{"galwey"} (Galwey 2009),
#'   \code{"li_ji"} (Li and Ji 2005), or \code{"nyholt"} (Nyholt 2004).
#' @return Effective number of tests (>= 1).
#' @export
n_effective_tests <- function(correlation_matrix,
                               method = c("galwey", "li_ji", "nyholt")) {
  method <- match.arg(method)
  if (is.null(correlation_matrix)) {
    stop("correlation_matrix is required")
  }
  R <- as.matrix(correlation_matrix)
  if (!isTRUE(nrow(R) == ncol(R))) {
    stop("correlation_matrix must be square")
  }
  evs <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  evs <- evs[evs > 0]
  m <- length(evs)
  if (m == 0L) {
    return(1.0)
  }

  if (method == "galwey") {
    m_eff <- (sum(sqrt(evs))) ^ 2 / sum(evs)
  } else if (method == "li_ji") {
    # Li & Ji (2005): f(x) = I(x>=1) + (x - floor(x)); summed over all
    # eigenvalues, INCLUDING the fractional part of evs >= 1.
    m_eff <- sum(evs >= 1) + sum(evs - floor(evs))
  } else {
    var_e <- stats::var(evs)
    m_eff <- 1 + (m - 1) * (1 - var_e / m)
  }
  max(1.0, m_eff)
}


# ---------------------------------------------------------------------------
# Print method (delegates to existing morie_rich_result printer)
# ---------------------------------------------------------------------------

#' @export
print.morie_multiple_testing_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\
\
", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}



# ---------------------------------------------------------------------------
# Local FDR and permutation-based FWER/FDR
# ---------------------------------------------------------------------------

#' Local false discovery rate via empirical-Bayes two-component mixture
#'
#' Estimates the local FDR for each test as
#' \eqn{lfdr_i = \\pi_0 \\, f_0(z_i) / f(z_i)}{lfdr_i = pi_0 \ f_0(z_i) / f(z_i)}, where
#' \eqn{z_i = \Phi^{-1}(1 - p_i/2)}{z_i = Phi^-1(1 - p_i/2)} are two-sided z-scores, \code{f_0} is the
#' standard-normal null density, \eqn{f} is a kernel density estimate of the
#' observed z-scores, and \eqn{\\pi_0}{pi_0} is the proportion of null hypotheses
#' estimated by the Storey-style cutoff at \eqn{p > 0.5}.
#'
#' @param p_values Numeric vector of raw p-values in \eqn{`[0, 1]`}.
#' @param pi0_method Pi-zero estimator. Accepted: \code{"bootstrap"}
#'   (alias for the Storey-style cutoff at 0.5; retained for API parity
#'   with the Python sibling).
#' @param labels Optional character vector of test labels.
#' @return A data frame with columns \code{p_value}, \code{z_score},
#'   \code{local_fdr}, and (if supplied) \code{label}. The data frame
#'   additionally carries class \code{morie_rich_result}.
#' @examples
#' set.seed(1)
#' p <- c(stats::runif(80), stats::pnorm(-abs(stats::rnorm(20, mean = 3))) * 2)
#' lfdr <- local_fdr(p)
#' head(lfdr)
#' @export
local_fdr <- function(p_values, pi0_method = "bootstrap", labels = NULL) {
  p <- .mt_check_p(p_values)
  m <- length(p)
  # Two-sided z-scores
  z <- stats::qnorm(1 - p / 2)

  # Pi0 estimate (Storey-style cutoff at 0.5; the pi0_method argument is
  # currently a passthrough for cross-language API parity).
  pi0_hat <- min(sum(p > 0.5) / (m * 0.5), 1.0)

  # KDE for observed z; degrade gracefully if all z are identical.
  f_z <- if (length(unique(z)) > 1L) {
    suppressWarnings(stats::density(z, from = min(z), to = max(z),
                                    n = max(512L, m)))
  } else {
    NULL
  }
  if (is.null(f_z)) {
    f_z_at <- rep(stats::dnorm(0), m)
  } else {
    f_z_at <- stats::approx(f_z$x, f_z$y, xout = z, rule = 2)$y
  }
  f0_z <- stats::dnorm(z)

  lfdr <- pi0_hat * f0_z / pmax(f_z_at, 1e-10)
  lfdr <- pmin(lfdr, 1.0)

  out <- data.frame(p_value = p, z_score = z, local_fdr = lfdr,
                    stringsAsFactors = FALSE)
  if (!is.null(labels)) {
    out$label <- labels
  }
  attr(out, "pi0_hat") <- pi0_hat
  attr(out, "pi0_method") <- pi0_method
  class(out) <- c("morie_local_fdr_result", "morie_rich_result",
                  class(out))
  out
}

#' Permutation-based FWER control via step-down max-T (Westfall--Young)
#'
#' Given observed test statistics and a matrix of test statistics under the
#' permutation null, computes step-down max-T adjusted p-values that
#' strongly control the family-wise error rate without requiring
#' independence across tests.
#'
#' @param test_stats Numeric vector of length \eqn{m} of observed test
#'   statistics.
#' @param null_stats Numeric matrix with \eqn{n_{perm}} rows and \eqn{m}
#'   columns containing test statistics computed on permuted data.
#' @param alternative One of \code{"two_sided"} (default), \code{"greater"},
#'   or \code{"less"}.
#' @param alpha Significance level for rejection (default 0.05).
#' @param labels Optional character vector of test labels.
#' @return A \code{morie_rich_result} list with \code{original},
#'   \code{adjusted}, \code{rejected}, \code{method}, \code{alpha},
#'   \code{n_rejected}, \code{n_tests}.
#' @examples
#' set.seed(1)
#' m <- 10; nperm <- 200
#' obs <- c(rnorm(m - 2), 4.0, 3.5)
#' null <- matrix(rnorm(nperm * m), nperm, m)
#' permutation_fwer(obs, null)
#' @export
permutation_fwer <- function(test_stats, null_stats,
                             alternative = c("two_sided", "greater", "less"),
                             alpha = 0.05, labels = NULL) {
  alternative <- match.arg(alternative)
  t_obs <- as.numeric(test_stats)
  t_null <- as.matrix(null_stats)
  m <- length(t_obs)
  if (ncol(t_null) != m) {
    stop("permutation_fwer: ncol(null_stats) must equal length(test_stats).")
  }

  if (alternative == "two_sided") {
    t_obs_a <- abs(t_obs)
    t_null_a <- abs(t_null)
  } else if (alternative == "greater") {
    t_obs_a <- t_obs
    t_null_a <- t_null
  } else {
    t_obs_a <- -t_obs
    t_null_a <- -t_null
  }

  ord <- order(-t_obs_a)   # descending
  t_sorted <- t_obs_a[ord]
  adjusted_sorted <- numeric(m)
  for (i in seq_len(m)) {
    remaining <- ord[i:m]
    if (length(remaining) == 1L) {
      max_null <- t_null_a[, remaining]
    } else {
      max_null <- apply(t_null_a[, remaining, drop = FALSE], 1, max)
    }
    adjusted_sorted[i] <- mean(max_null >= t_sorted[i])
  }
  # Enforce monotonicity along the sorted order
  if (m > 1L) {
    for (i in 2:m) {
      adjusted_sorted[i] <- max(adjusted_sorted[i], adjusted_sorted[i - 1])
    }
  }
  adjusted <- numeric(m)
  adjusted[ord] <- adjusted_sorted

  .mt_adjusted("permutation_maxT", t_obs_a, alpha, adjusted, labels,
               note = sprintf("Step-down max-T over %d permutations.",
                              nrow(t_null)))
}

#' Permutation-based FDR control via empirical null p-value distribution
#'
#' Estimates the false discovery rate at each candidate threshold using a
#' matrix of p-values computed under the permutation null, and selects the
#' largest threshold whose estimated FDR is at most \code{alpha}. Q-values
#' are assigned as the minimum estimated FDR across thresholds at least as
#' large as each observed p-value.
#'
#' @param test_stats Numeric vector of observed p-values (named after the
#'   Python sibling argument to keep cross-language parity).
#' @param null_stats Numeric matrix of permutation-null p-values with
#'   \eqn{n_{perm}} rows and \eqn{m} columns.
#' @param alpha Target FDR level (default 0.05).
#' @param labels Optional character vector of test labels.
#' @return A \code{morie_rich_result} list with \code{original} (raw
#'   p-values), \code{adjusted} (q-values), \code{rejected},
#'   \code{method}, \code{alpha}, \code{n_rejected}, \code{n_tests}.
#' @examples
#' set.seed(1)
#' m <- 20; nperm <- 200
#' p_obs <- c(stats::runif(m - 4), c(1e-4, 1e-3, 1e-3, 5e-3))
#' p_null <- matrix(stats::runif(nperm * m), nperm, m)
#' permutation_fdr(p_obs, p_null)
#' @export
permutation_fdr <- function(test_stats, null_stats,
                            alpha = 0.05, labels = NULL) {
  p <- .mt_check_p(test_stats)
  p_null <- as.matrix(null_stats)
  m <- length(p)
  if (ncol(p_null) != m) {
    stop("permutation_fdr: ncol(null_stats) must equal length(test_stats).")
  }

  thresholds <- sort(p)
  fdr_at_t <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    n_rej <- sum(p <= t)
    if (n_rej == 0L) {
      fdr_at_t[i] <- 0
    } else {
      expected_false <- mean(rowSums(p_null <= t))
      fdr_at_t[i] <- expected_false / n_rej
    }
  }

  valid <- fdr_at_t <= alpha
  t_star <- if (any(valid)) thresholds[max(which(valid))] else 0
  rejected <- p <= t_star

  q_values <- rep(1, m)
  for (i in seq_len(m)) {
    cand <- fdr_at_t[thresholds >= p[i]]
    if (length(cand) > 0L) q_values[i] <- min(cand)
  }
  q_values <- pmin(pmax(q_values, 0), 1)

  .mt_adjusted("permutation_fdr", p, alpha, q_values, labels,
               note = sprintf("Permutation-null FDR with %d permutations.",
                              nrow(p_null)))
}
