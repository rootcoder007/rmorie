# morie.fn -- function file (rootcoder007/morie)
# R arm of slvgrf (aipw_scores, toc_curve, rate, qini_coefficient,
# autoc, qini_curve, rate_test).
# Sources:
#   Yadlowsky, S., Fleming, S., Shah, N., Brunskill, E. & Wager, S.
#   (2025) "Evaluating Treatment Prioritization Rules via Rank-Weighted
#   Average Treatment Effects", JASA 120(549), 38-51,
#   doi:10.1080/01621459.2024.2393466. Definition 1 (prioritization
#   rule), Definition 2 (TOC), Definition 3 (RATE), Remark 1 (the
#   exact null), Sec. 2.2-2.3 (the AIPW-score estimator), Theorem 3
#   and Corollary 5 (asymptotic linearity and the half-sample
#   bootstrap), Sec. 4 and Fig. 2 (Qini vs AUTOC power).
#   Sverdrup, E., Wu, H., Athey, S. & Wager, S. (2025) "Qini Curves
#   for Multi-Armed Treatment Rules", JCGS 34(3), 948-960,
#   doi:10.1080/10618600.2024.2418820. The Qini curve under a cost
#   constraint and its multi-armed generalisation.
#   Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized random
#   forests", The Annals of Statistics 47(2), 1148-1178,
#   doi:10.1214/18-AOS1709. The forest whose CATE estimates are the
#   usual priority score here.

.SLVGRF_EPS <- 1e-12
.SLVGRF_WEIGHTS <- c("qini", "autoc", "uniform")

#' .slvgrf_check
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @return A list with \code{g}, \code{s}.
#' @export
.slvgrf_check <- function(scores, priority) {
  g <- as.numeric(scores)
  s <- as.numeric(priority)
  if (length(g) != length(s))
    stop(sprintf("slvgrf: %d scores but %d priority values",
                 length(g), length(s)))
  if (length(g) < 2L)
    stop(sprintf("slvgrf: need at least 2 units, got %d", length(g)))
  list(g = g, s = s)
}

#' aipw_scores
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param W See Usage.
#' @param mu1 See Usage.
#' @param mu0 See Usage.
#' @param e See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
aipw_scores <- function(Y, W, mu1, mu0, e) {
  y <- as.numeric(Y)
  w <- as.numeric(W)
  m1 <- as.numeric(mu1)
  m0 <- as.numeric(mu0)
  n <- length(y)
  if (length(e) == 1L) {
    ev <- rep(as.numeric(e), n)
  } else {
    ev <- as.numeric(e)
  }
  if (length(w) != n)
    stop(sprintf("slvgrf: W has %d entries for %d units", length(w), n))
  if (length(m1) != n)
    stop(sprintf("slvgrf: mu1 has %d entries for %d units",
                 length(m1), n))
  if (length(m0) != n)
    stop(sprintf("slvgrf: mu0 has %d entries for %d units",
                 length(m0), n))
  if (length(ev) != n)
    stop(sprintf("slvgrf: e has %d entries for %d units", length(ev), n))
  for (v in w)
    if (!(v == 0.0 || v == 1.0))
      stop(sprintf("slvgrf: W must be 0/1, got %r", v))
  for (v in ev)
    if (!(v > 0.0 && v < 1.0))
      stop(sprintf(paste0("slvgrf: the propensity must lie strictly ",
                          "in (0, 1); got %r -- overlap fails"), v))
  out <- m1 - m0 + w * (y - m1) / ev - (1.0 - w) * (y - m0) / (1.0 - ev)
  as.numeric(out)
}

#' toc_curve
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @return A list with \code{u}, \code{toc}, \code{ate}, \code{order}, \code{n}.
#' @export
toc_curve <- function(scores, priority) {
  chk <- .slvgrf_check(scores, priority)
  g <- chk$g; s <- chk$s
  n <- length(g)
  order <- order(-s, seq_len(n) - 1L)
  ate <- sum(g) / n
  run <- 0.0
  toc <- numeric(n)
  us <- numeric(n)
  for (j in seq_len(n)) {
    i <- order[j]
    run <- run + g[i]
    toc[j] <- run / j - ate
    us[j] <- j / as.numeric(n)
  }
  list(u = us, toc = toc, ate = ate, order = as.integer(order),
       n = as.integer(n))
}

#' rate
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @param weight Defaults to \code{"autoc"}.
#' @return A list with \code{estimate}, \code{weight}, \code{curve}, \code{n}.
#' @export
rate <- function(scores, priority, weight = "autoc") {
  if (!(weight %in% .SLVGRF_WEIGHTS))
    stop(sprintf("slvgrf: weight must be one of %s, got %r",
                 paste(.SLVGRF_WEIGHTS, collapse = ", "), weight))
  c <- toc_curve(scores, priority)
  n <- c$n
  if (weight == "qini") {
    val <- sum(c$u * c$toc) / n
  } else {
    val <- sum(c$toc) / n
  }
  list(estimate = val, weight = weight, curve = c, n = n)
}

#' autoc
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @return The value of \code{$}.
#' @export
autoc <- function(scores, priority) {
  rate(scores, priority, weight = "autoc")$estimate
}

#' qini_coefficient
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @return The value of \code{$}.
#' @export
qini_coefficient <- function(scores, priority) {
  rate(scores, priority, weight = "qini")$estimate
}

#' qini_curve
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @param cost Defaults to \code{NULL}.
#' @return A list with \code{spend}, \code{gain}, \code{ate}, \code{n}, \code{constrained}.
#' @export
qini_curve <- function(scores, priority, cost = NULL) {
  chk <- .slvgrf_check(scores, priority)
  g <- chk$g; s <- chk$s
  n <- length(g)
  order <- order(-s, seq_len(n) - 1L)
  if (is.null(cost)) {
    cv <- rep(1.0, n)
  } else {
    if (length(cost) == 1L) cv <- rep(as.numeric(cost), n)
    else cv <- as.numeric(cost)
    if (length(cv) != n)
      stop(sprintf("slvgrf: %d costs for %d units", length(cv), n))
    if (any(cv <= 0.0))
      stop("slvgrf: costs must be positive")
  }
  total <- sum(cv)
  run <- 0.0
  spent <- 0.0
  xs <- numeric(n)
  ys <- numeric(n)
  for (k in seq_len(n)) {
    i <- order[k]
    run <- run + g[i]
    spent <- spent + cv[i]
    xs[k] <- spent / total
    ys[k] <- run / n
  }
  list(spend = xs, gain = ys, ate = sum(g) / n, n = n,
       constrained = !is.null(cost))
}

#' rate_test
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param scores See Usage.
#' @param priority See Usage.
#' @param weight Defaults to \code{"autoc"}.
#' @param reps Defaults to \code{500}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{se}, \code{z}, \code{p_value}, \code{weight}, \code{reps}, \code{n}, \code{null}, \code{method}.
#' @export
rate_test <- function(scores, priority, weight = "autoc", reps = 500,
                      seed = 0) {
  chk <- .slvgrf_check(scores, priority)
  g <- chk$g; s <- chk$s
  n <- length(g)
  if (n < 8L)
    stop(sprintf("slvgrf: the half-sample bootstrap needs at least 8 units, got %d", n))
  theta <- rate(g, s, weight = weight)$estimate
  e <- .ghc_rng(seed)
  half <- as.integer(n / 2L)
  draws <- numeric(as.integer(reps))
  for (k in seq_len(as.integer(reps))) {
    u <- .ghc_unif(e, n)
    ord <- order(u, seq_len(n) - 1L)
    idx <- ord[seq_len(half)]
    draws[k] <- rate(g[idx], s[idx], weight = weight)$estimate
  }
  m <- mean(draws)
  v <- sum((draws - m) ^ 2) / (length(draws) - 1L)
  se <- sqrt(max(v, 0.0) / 2.0)
  z <- if (se > .SLVGRF_EPS) theta / se else 0.0
  p <- 2.0 * (1.0 - pnorm(abs(z)))
  list(estimate = theta, se = se, z = z, p_value = p,
       weight = weight, reps = as.integer(reps), n = n,
       null = paste0("the priority score is independent of the ",
                     "treatment effect (Remark 1), NOT that the ATE ",
                     "is zero"),
       method = paste0("RATE with half-sample bootstrap, Yadlowsky ",
                       "et al. (2025) Corollary 5"))
}

#' .slvgrf_cheatsheet
#'
#' Part of the slvgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.slvgrf_cheatsheet <- function() {
  paste0("slvgrf: score a PRIORITIZATION RULE, not a CATE fit. ",
         "TOC(u) = mean effect in the top u minus the ATE, so ",
         "TOC(1) = 0 exactly. RATE = int alpha(u) TOC(u) du; ",
         "alpha(u)=u is Qini, alpha(u)=1 is AUTOC. If the score is ",
         "independent of the effect, every RATE is exactly 0 -- so ",
         "this tests HETEROGENEITY, not the ATE. Qini has more ",
         "power when many units benefit, AUTOC when few do. ",
         "Estimate off AIPW scores; test by half-sample bootstrap.")
}

# ledger/NAMING.md compact aliases
slicedgrf <- rate
sliced_grf <- rate

morie_slvgrf <- list(aipw_scores = aipw_scores,
                     toc_curve = toc_curve,
                     rate = rate,
                     qini_coefficient = qini_coefficient,
                     autoc = autoc,
                     qini_curve = qini_curve,
                     rate_test = rate_test,
                     cheatsheet = .slvgrf_cheatsheet,
                     slicedgrf = slicedgrf,
                     sliced_grf = sliced_grf)
