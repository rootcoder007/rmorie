# SPDX-License-Identifier: AGPL-3.0-or-later

# ---------------------------------------------------------------------
# Causal inference: potential outcomes, back-door identification and
# adjustment, mediation (linear, binary, product-of-coefficients),
# doubly robust DiD, and additive-noise causal direction.
#
# Mirrors morie.fn.ate_d / bdrj / bdcrt / bkmed / abind / aiptdd /
# binmed / anmod.
# ---------------------------------------------------------------------

#' Average treatment effect from potential outcomes
#'
#' \eqn{ATE = E\[Y(1) - Y(0)\]}. The two arms may be the same units seen
#' under both conditions (\code{paired = TRUE}) or different units.
#' The distinction is explicit rather than inferred from matching
#' lengths, because the variance formulas differ sharply: pairing uses
#' \eqn{Var(Y(1) - Y(0))/n}, which accounts for the covariance between
#' arms, while independent arms use \eqn{s_1^2/n_1 + s_0^2/n_0}. With
#' strongly correlated arms the paired standard error is many times
#' smaller, so guessing would mis-state every interval.
#'
#' Mirrors \code{morie.fn.ate_d}.
#'
#' @param y1,y0 Potential outcomes under treatment and control.
#' @param paired Treat the arrays as the same units under both arms.
#' @param alpha Significance level for the interval.
#' @return Named list with \code{ate}, \code{se}, \code{ci_low},
#'   \code{ci_high}, \code{statistic}, \code{p_value}, \code{df},
#'   \code{n1}, \code{n0}, \code{paired}, \code{method}.
#' @references Rubin DB (1974). Estimating causal effects of treatments
#'   in randomized and nonrandomized studies. \emph{Journal of
#'   Educational Psychology}, 66(5), 688-701. Holland PW (1986).
#'   Statistics and causal inference. \emph{JASA}, 81(396), 945-960.
#' @examples
#' set.seed(1)
#' morie_ate_potential_outcomes(rnorm(50, 2), rnorm(50))$ate
#' @export
morie_ate_potential_outcomes <- function(y1, y0, paired = TRUE, alpha = 0.05) {
  a <- as.numeric(y1)
  b <- as.numeric(y0)
  if (!length(a) || !length(b)) stop("y1 and y0 must not be empty.", call. = FALSE)
  if (!all(is.finite(a)) || !all(is.finite(b))) stop("y1 and y0 must be finite.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1) stop("alpha must lie in (0, 1), got ", alpha, ".", call. = FALSE)
  if (paired && length(a) != length(b)) {
    stop("paired = TRUE needs the same units in both arms; got ", length(a),
      " and ", length(b), ". Pass paired = FALSE for independent groups.",
      call. = FALSE
    )
  }
  ate <- mean(a) - mean(b)
  if (paired) {
    d <- a - b
    if (length(d) < 2L) stop("Need at least 2 pairs for a standard error.", call. = FALSE)
    se <- stats::sd(d) / sqrt(length(d))
    df <- length(d) - 1
  } else {
    n1 <- length(a)
    n0 <- length(b)
    if (n1 < 2L || n0 < 2L) stop("Need at least 2 observations per arm.", call. = FALSE)
    v1 <- stats::var(a) / n1
    v0 <- stats::var(b) / n0
    se <- sqrt(v1 + v0)
    df <- (v1 + v0)^2 / (v1^2 / (n1 - 1) + v0^2 / (n0 - 1))
  }
  tstat <- if (se > 0) ate / se else NA_real_
  crit <- if (se > 0) stats::qt(1 - alpha / 2, df) else 0
  list(
    ate = ate, estimate = ate, se = se,
    ci_low = ate - crit * se, ci_high = ate + crit * se,
    statistic = tstat,
    p_value = if (se > 0) 2 * stats::pt(-abs(tstat), df) else NA_real_,
    df = df, mean_treated = mean(a), mean_control = mean(b),
    n1 = length(a), n0 = length(b), paired = paired,
    method = paste0(
      "ATE from potential outcomes, ",
      if (paired) "paired" else "independent arms"
    )
  )
}

#' Back-door adjustment formula
#'
#' \eqn{P(Y=y | do(X=x)) = \sum_z P(Y=y | X=x, Z=z) P(Z=z)}. Each
#' stratum is reweighted by how common it is in the whole population,
#' not among the treated; that reweighting is the entire content of the
#' formula.
#'
#' This is causal only when Z satisfies the back-door criterion, which
#' is a claim about the graph and cannot be checked from data. See
#' \code{\link{morie_backdoor_criterion}}. All variables are discrete.
#'
#' Mirrors \code{morie.fn.bdrj}.
#'
#' @param x,y Discrete treatment and outcome vectors.
#' @param z Adjustment set: a vector or a matrix of columns.
#' @param at Treatment value to intervene on; defaults to every level.
#' @return Named list with \code{distribution}, \code{strata},
#'   \code{p_z}, \code{n}, \code{incomplete_strata}, \code{method}.
#' @references Pearl J (2009). \emph{Causality}, 2nd edn, Thm 3.3.2.
#' @examples
#' morie_backdoor_adjustment(c(1, 1, 0, 0), c(1, 0, 1, 0), c(0, 0, 1, 1))$distribution
#' @export
morie_backdoor_adjustment <- function(x, y, z, at = NULL) {
  xa <- as.vector(x)
  ya <- as.vector(y)
  za <- if (is.null(dim(z))) matrix(z, ncol = 1L) else as.matrix(z)
  n <- length(xa)
  if (length(ya) != n || nrow(za) != n) {
    stop("x, y and z must share a length; got ", n, ", ", length(ya), ", ",
      nrow(za), ".",
      call. = FALSE
    )
  }
  if (n == 0L) stop("x, y and z must not be empty.", call. = FALSE)
  zlab <- apply(za, 1L, function(r) paste(r, collapse = "|"))
  lev <- sort(unique(zlab))
  pz <- as.numeric(table(factor(zlab, levels = lev))) / n
  sup_x <- sort(unique(xa))
  sup_y <- sort(unique(ya))
  targets <- if (is.null(at)) sup_x else at
  for (t in targets) {
    if (!any(xa == t)) {
      stop("at = ", t, " does not occur in x; the conditional is undefined.", call. = FALSE)
    }
  }
  dist <- list()
  incomplete <- list()
  for (t in targets) {
    acc <- stats::setNames(rep(0, length(sup_y)), as.character(sup_y))
    for (i in seq_along(lev)) {
      sel <- zlab == lev[i] & xa == t
      m <- sum(sel)
      if (m == 0L) {
        incomplete[[length(incomplete) + 1L]] <- c(as.character(t), lev[i])
        next
      }
      yz <- ya[sel]
      for (yv in sup_y) {
        acc[[as.character(yv)]] <- acc[[as.character(yv)]] + pz[i] * sum(yz == yv) / m
      }
    }
    dist[[as.character(t)]] <- acc
  }
  list(
    distribution = dist, strata = lev, p_z = pz, n = n,
    support_x = sup_x, support_y = sup_y,
    incomplete_strata = incomplete,
    method = "Back-door adjustment (Pearl 2009, Thm 3.3.2), discrete"
  )
}

# Internal graph helpers for the back-door criterion.
#' Internal graph helpers for the back-door criterion
#'
#' A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dag A matrix; passed to \code{as.matrix}.
#' @return A list with \code{children}, \code{parents}, \code{nodes}.
#' @export
.bd_parse <- function(dag) {
  if (is.list(dag) && !is.null(names(dag))) {
    edges <- do.call(rbind, lapply(names(dag), function(u) {
      if (!length(dag[[u]])) NULL else cbind(u, as.character(dag[[u]]))
    }))
  } else {
    edges <- as.matrix(dag)
  }
  nodes <- unique(as.vector(edges))
  ch <- stats::setNames(lapply(nodes, function(n) edges[edges[, 1] == n, 2]), nodes)
  pa <- stats::setNames(lapply(nodes, function(n) edges[edges[, 2] == n, 1]), nodes)
  list(children = ch, parents = pa, nodes = nodes)
}

#' .bd_desc
#'
#' A step of the causal_native implementation. Called by \code{.bd_blocked}, \code{morie_backdoor_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node See Usage.
#' @param ch A vector; indexed elementwise.
#' @return The value of \code{seen}, as built in the body.
#' @export
.bd_desc <- function(node, ch) {
  seen <- character(0)
  stack <- node
  while (length(stack)) {
    cur <- stack[1]
    stack <- stack[-1]
    for (c in ch[[cur]]) {
      if (!(c %in% seen)) {
        seen <- c(seen, c)
        stack <- c(stack, c)
      }
    }
  }
  seen
}

#' .bd_paths
#'
#' A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Carried through into a list the body builds.
#' @param y Passed to \code{==}.
#' @param ch A vector; indexed elementwise.
#' @param pa A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.bd_paths <- function(x, y, ch, pa) {
  out <- list()
  stack <- list(list(cur = x, path = x, dirs = character(0)))
  while (length(stack)) {
    s <- stack[[1]]
    stack <- stack[-1]
    if (s$cur == y) {
      out[[length(out) + 1L]] <- s
      next
    }
    for (nx in ch[[s$cur]]) {
      if (!(nx %in% s$path)) {
        stack <- c(stack, list(list(cur = nx, path = c(s$path, nx), dirs = c(s$dirs, "->"))))
      }
    }
    for (nx in pa[[s$cur]]) {
      if (!(nx %in% s$path)) {
        stack <- c(stack, list(list(cur = nx, path = c(s$path, nx), dirs = c(s$dirs, "<-"))))
      }
    }
  }
  out
}

#' .bd_blocked
#'
#' A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; its length is taken and its elements indexed.
#' @param d A vector; indexed elementwise.
#' @param Z Passed to \code{\%in\%}.
#' @param ch Passed to \code{.bd_desc}.
#' @return A logical value.
#' @export
.bd_blocked <- function(p, d, Z, ch) {
  if (length(p) < 3L) {
    return(FALSE)
  }
  for (i in 2:(length(p) - 1L)) {
    node <- p[i]
    collider <- d[i - 1L] == "->" && d[i] == "<-"
    if (collider) {
      if (!(node %in% Z) && !length(intersect(.bd_desc(node, ch), Z))) {
        return(TRUE)
      }
    } else if (node %in% Z) {
      return(TRUE)
    }
  }
  FALSE
}

#' Back-door criterion for an adjustment set
#'
#' Z satisfies the criterion when no member is a descendant of X and Z
#' blocks every back-door path from X to Y. Blocking is d-separation,
#' including the collider rule that makes it more than "adjust for
#' everything": a chain or fork is blocked when the middle node is in Z,
#' but a collider blocks the path \emph{unless} it or a descendant is in
#' Z, so conditioning on a collider opens a path that was closed.
#'
#' This is the check that \code{\link{morie_backdoor_adjustment}}
#' assumes and cannot perform, since the adjustment is arithmetic over
#' data while the criterion is a claim about the graph.
#'
#' Mirrors \code{morie.fn.bdcrt}.
#'
#' @param dag Named list of \code{node = children}, or a two-column edge
#'   matrix of (parent, child). Must be acyclic.
#' @param x,y Treatment and outcome node names.
#' @param z Candidate adjustment set; the empty set is valid.
#' @return Named list with \code{satisfied},
#'   \code{descendant_violations}, \code{open_paths},
#'   \code{n_backdoor}, \code{reason}, \code{method}.
#' @references Pearl J (2009). \emph{Causality}, 2nd edn, Def. 3.3.1.
#' @examples
#' morie_backdoor_criterion(list(Z = c("X", "Y"), X = "Y"), "X", "Y", "Z")$satisfied
#' @export
morie_backdoor_criterion <- function(dag, x, y, z = character(0)) {
  g <- .bd_parse(dag)
  Z <- as.character(z)
  for (nm in c(x, y)) {
    if (!(nm %in% g$nodes)) stop(nm, " is not a node of the graph.", call. = FALSE)
  }
  miss <- setdiff(Z, g$nodes)
  if (length(miss)) {
    stop("z contains nodes not in the graph: ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  if (x %in% Z || y %in% Z) stop("z must not contain x or y.", call. = FALSE)
  for (nd in g$nodes) {
    if (nd %in% .bd_desc(nd, g$children)) {
      stop("dag contains a cycle; the back-door criterion is defined for DAGs.", call. = FALSE)
    }
  }

  bad <- sort(intersect(Z, .bd_desc(x, g$children)))
  back <- Filter(
    function(s) length(s$dirs) && s$dirs[1] == "<-",
    .bd_paths(x, y, g$children, g$parents)
  )
  open <- Filter(function(s) !.bd_blocked(s$path, s$dirs, Z, g$children), back)
  ok <- !length(bad) && !length(open)
  reason <- if (ok) {
    "Z satisfies the back-door criterion; the effect is identified by adjustment."
  } else if (length(bad)) {
    paste0(
      "Z contains descendants of X: ", paste(bad, collapse = ", "),
      ". Adjusting for them blocks part of the effect itself."
    )
  } else {
    paste0(length(open), " back-door path(s) remain open, so confounding is not removed.")
  }
  list(
    satisfied = ok, descendant_violations = bad,
    open_paths = vapply(open, function(s) paste(s$path, collapse = " "), character(1)),
    n_backdoor = length(back), adjustment_set = sort(Z), reason = reason,
    method = "Back-door criterion (Pearl 2009, Def. 3.3.1) via d-separation"
  )
}

#' Baron-Kenny stepwise mediation
#'
#' Fits \eqn{Y = i_1 + cX}, \eqn{M = i_2 + aX} and
#' \eqn{Y = i_3 + c'X + bM}, and reports each of the four conditions
#' separately rather than collapsing to a verdict. Requiring a
#' significant total effect (step 1) is now regarded as too strong:
#' opposite-signed direct and indirect paths cancel, and that rule
#' discards exactly those cases.
#'
#' Mirrors \code{morie.fn.bkmed}.
#'
#' @param y,x,m Outcome, treatment and mediator vectors.
#' @param alpha Significance level for each step.
#' @return Named list with \code{c}, \code{a}, \code{b},
#'   \code{c_prime}, \code{indirect}, \code{proportion_mediated},
#'   \code{se}, \code{p}, \code{steps}, \code{mediation}, \code{n}.
#' @references Baron RM & Kenny DA (1986). The moderator-mediator
#'   variable distinction in social psychological research: conceptual,
#'   strategic, and statistical considerations. \emph{Journal of
#'   Personality and Social Psychology}, 51(6), 1173-1182.
#' @examples
#' set.seed(1)
#' x <- rnorm(200)
#' m <- 0.8 * x + rnorm(200)
#' morie_baron_kenny(0.3 * x + 0.6 * m + rnorm(200), x, m)$mediation
#' @export
morie_baron_kenny <- function(y, x, m, alpha = 0.05) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  m <- as.numeric(m)
  n <- length(y)
  if (length(x) != n || length(m) != n) {
    stop("y, x and m must be the same length; got ", n, ", ", length(x),
      ", ", length(m), ".",
      call. = FALSE
    )
  }
  if (n < 4L) stop("Need at least 4 observations for the three-step fit, got ", n, ".", call. = FALSE)
  if (!all(is.finite(c(y, x, m)))) stop("y, x and m must be finite.", call. = FALSE)

  s1 <- summary(stats::lm(y ~ x))$coefficients
  s2 <- summary(stats::lm(m ~ x))$coefficients
  s3 <- summary(stats::lm(y ~ x + m))$coefficients
  cc <- s1["x", 1]
  a <- s2["x", 1]
  cp <- s3["x", 1]
  b <- s3["m", 1]
  p_c <- s1["x", 4]
  p_a <- s2["x", 4]
  p_cp <- s3["x", 4]
  p_b <- s3["m", 4]

  steps <- list(
    step1_total_effect_significant = p_c < alpha,
    step2_x_predicts_m = p_a < alpha,
    step3_m_predicts_y_given_x = p_b < alpha,
    step4_direct_effect_shrinks = abs(cp) < abs(cc)
  )
  med <- if (steps$step2_x_predicts_m && steps$step3_m_predicts_y_given_x) {
    if (p_cp >= alpha) "complete" else if (steps$step4_direct_effect_shrinks) "partial" else "none"
  } else {
    "none"
  }

  list(
    c = cc, a = a, b = b, c_prime = cp, indirect = a * b,
    proportion_mediated = if (cc != 0) a * b / cc else NA_real_,
    se = list(c = s1["x", 2], a = s2["x", 2], b = s3["m", 2], c_prime = s3["x", 2]),
    p = list(c = p_c, a = p_a, b = p_b, c_prime = p_cp),
    steps = steps, mediation = med, n = n, alpha = alpha,
    method = "Baron & Kenny (1986) stepwise mediation"
  )
}

#' Product-of-coefficients indirect effect with the Sobel standard error
#'
#' \eqn{IE = ab} with \eqn{s_{ab} = \sqrt{b^2 s_a^2 + a^2 s_b^2}}.
#' Reported because it is what this quantity is classically paired with,
#' but the product of two normals is not normal and is skewed when both
#' coefficients are near zero, so the test is conservative exactly where
#' mediation is most in doubt; a bootstrap is the better instrument
#' there. Without both standard errors the test is skipped rather than
#' faked.
#'
#' Mirrors \code{morie.fn.abind}.
#'
#' @param a,b Path coefficients.
#' @param se_a,se_b Their standard errors; both needed for a test.
#' @param alpha Significance level.
#' @return Named list with \code{estimate}, \code{se},
#'   \code{statistic}, \code{p_value}, \code{ci_low}, \code{ci_high}.
#' @references Sobel ME (1982). Asymptotic confidence intervals for
#'   indirect effects in structural equation models. \emph{Sociological
#'   Methodology}, 13, 290-312.
#' @examples
#' morie_indirect_effect_sobel(0.5, 0.4, 0.1, 0.08)$p_value
#' @export
morie_indirect_effect_sobel <- function(a, b, se_a = NULL, se_b = NULL, alpha = 0.05) {
  if (!all(is.finite(c(a, b)))) stop("a and b must be finite.", call. = FALSE)
  if (alpha <= 0 || alpha >= 1) stop("alpha must lie in (0, 1), got ", alpha, ".", call. = FALSE)
  ie <- a * b
  if (is.null(se_a) || is.null(se_b)) {
    return(list(
      estimate = ie, se = NULL, statistic = NULL, p_value = NULL,
      ci_low = NULL, ci_high = NULL, a = a, b = b,
      method = "Indirect effect ab; no standard errors supplied, so no test"
    ))
  }
  if (any(se_a < 0) || any(se_b < 0)) stop("Standard errors must not be negative.", call. = FALSE)
  se <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
  z <- ifelse(se > 0, ie / se, NA_real_)
  crit <- stats::qnorm(1 - alpha / 2)
  list(
    estimate = ie, se = se, statistic = z,
    p_value = 2 * stats::pnorm(-abs(z)),
    ci_low = ie - crit * se, ci_high = ie + crit * se, a = a, b = b,
    method = "Product-of-coefficients indirect effect, Sobel standard error"
  )
}

#' Doubly robust difference-in-differences
#'
#' Combines a propensity score with an outcome regression fitted on the
#' comparison group, so the estimator stays consistent if \emph{either}
#' working model is correct, not necessarily both. Parallel trends is
#' still assumed, now conditional on the covariates.
#'
#' Extreme propensity scores are the practical failure mode, since the
#' odds weight explodes as the score approaches one; scores are trimmed
#' and the count reported rather than hidden. Standard errors come from
#' the influence function, so the weighting is accounted for.
#'
#' Mirrors \code{morie.fn.aiptdd}.
#'
#' @param y_pre,y_post Outcome before and after, same units in order.
#' @param d Binary treatment indicator.
#' @param x Covariate matrix; a vector is one column.
#' @param trim Upper bound on the fitted propensity score.
#' @param alpha Significance level.
#' @return Named list with \code{att}, \code{se}, \code{ci_low},
#'   \code{ci_high}, \code{statistic}, \code{p_value}, \code{n_trimmed},
#'   \code{ps_min}, \code{ps_max}, \code{method}.
#' @references Sant'Anna PHC & Zhao J (2020). Doubly robust
#'   difference-in-differences estimators. \emph{Journal of
#'   Econometrics}, 219(1), 101-122.
#' @examples
#' set.seed(1)
#' n <- 400
#' xx <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.8 * xx))
#' u <- rnorm(n, 0, 2)
#' morie_dr_did(u + rnorm(n), u + 0.9 * xx + 2 * d + rnorm(n), d, xx)$att
#' @export
morie_dr_did <- function(y_pre, y_post, d, x, trim = 0.995, alpha = 0.05) {
  pre <- as.numeric(y_pre)
  post <- as.numeric(y_post)
  dd <- as.numeric(d)
  X <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else as.matrix(x)
  n <- length(pre)
  if (length(post) != n || length(dd) != n || nrow(X) != n) {
    stop("y_pre, y_post, d and x must share a length; got ", n, ", ",
      length(post), ", ", length(dd), ", ", nrow(X), ".",
      call. = FALSE
    )
  }
  if (!all(dd %in% c(0, 1))) stop("d must be binary (0/1).", call. = FALSE)
  if (sum(dd) < 2 || sum(1 - dd) < 2) {
    stop("Need at least 2 units per arm; got ", sum(dd), " treated, ",
      sum(1 - dd), " control.",
      call. = FALSE
    )
  }
  if (!all(is.finite(c(pre, post))) || !all(is.finite(X))) {
    stop("y_pre, y_post and x must be finite.", call. = FALSE)
  }
  if (trim <= 0 || trim >= 1) stop("trim must lie in (0, 1), got ", trim, ".", call. = FALSE)

  dy <- post - pre
  ps <- stats::fitted(stats::glm(dd ~ X, family = stats::binomial()))
  n_trim <- sum(ps > trim)
  ps <- pmin(pmax(ps, 1e-6), trim)
  # Fit the outcome regression on the comparison group only, then predict
  # for every row -- lm.fit rather than lm(), so the design matrix is
  # explicit and prediction is a plain matrix product.
  fit0 <- stats::lm.fit(cbind(1, X[dd == 0, , drop = FALSE]), dy[dd == 0])
  m0 <- as.vector(cbind(1, X) %*% fit0$coefficients)

  resid <- dy - m0
  w1 <- dd
  w0 <- (1 - dd) * ps / (1 - ps)
  t1 <- sum(w1 * resid) / sum(w1)
  t0 <- sum(w0 * resid) / sum(w0)
  att <- t1 - t0
  inf <- (w1 * (resid - t1)) / mean(w1) - (w0 * (resid - t0)) / mean(w0)
  se <- stats::sd(inf) / sqrt(n)
  z <- if (se > 0) att / se else NA_real_
  crit <- stats::qnorm(1 - alpha / 2)

  list(
    att = att, estimate = att, se = se,
    ci_low = att - crit * se, ci_high = att + crit * se,
    statistic = z, p_value = if (se > 0) 2 * stats::pnorm(-abs(z)) else NA_real_,
    n_treated = sum(dd), n_control = sum(1 - dd), n_trimmed = n_trim,
    ps_min = min(ps), ps_max = max(ps),
    method = "AIPW / doubly robust DiD (Sant'Anna & Zhao 2020), panel"
  )
}

#' Binary-outcome causal mediation by inverse odds-ratio weighting
#'
#' Fits the exposure model \eqn{P(X=1 | M, C)}, weights the exposed by
#' the reciprocal of the exposure-mediator odds ratio, and refits the
#' total-effect outcome model with those weights. The weighted fit is
#' the natural direct effect; the difference from the unweighted total
#' is the natural indirect effect, both on the log-odds scale.
#'
#' This is not the linear machinery with a logit swapped in. Logistic
#' coefficients are not comparable across models -- each is scaled by
#' its own residual variance -- so the familiar \eqn{ab} and
#' \eqn{c - c'} decompositions do not hold on the logit scale. That
#' non-collapsibility is why \code{\link{morie_baron_kenny}} must not
#' simply be pointed at a binary outcome.
#'
#' No standard error is returned unless \code{B > 0}: the weights are
#' themselves estimated, so a model-based standard error understates the
#' uncertainty.
#'
#' Mirrors \code{morie.fn.binmed}.
#'
#' @param x Binary exposure.
#' @param m Mediator vector or matrix.
#' @param y Binary outcome.
#' @param covariates Optional baseline covariates.
#' @param B Bootstrap replicates for standard errors.
#' @param alpha Significance level for the bootstrap interval.
#' @return Named list with \code{total}, \code{direct}, \code{indirect},
#'   \code{or_total}, \code{or_direct}, \code{or_indirect}, \code{se},
#'   \code{ci_low}, \code{ci_high}, \code{n}, \code{B}, \code{method}.
#' @references Tchetgen Tchetgen EJ (2013). Inverse odds ratio-weighted
#'   estimation for causal mediation analysis. \emph{Statistics in
#'   Medicine}, 32(26), 4567-4580.
#' @examples
#' set.seed(1)
#' n <- 600
#' xx <- rbinom(n, 1, 0.5)
#' mm <- xx + rnorm(n)
#' yy <- rbinom(n, 1, plogis(-0.5 + 0.5 * xx + mm))
#' morie_binary_mediation(xx, mm, yy)$indirect
#' @export
morie_binary_mediation <- function(x, m, y, covariates = NULL, B = 0L, alpha = 0.05) {
  xx <- as.numeric(x)
  yy <- as.numeric(y)
  M <- if (is.null(dim(m))) matrix(as.numeric(m), ncol = 1L) else as.matrix(m)
  n <- length(xx)
  if (length(yy) != n || nrow(M) != n) {
    stop("x, m and y must share a length; got ", n, ", ", nrow(M), ", ",
      length(yy), ".",
      call. = FALSE
    )
  }
  if (!all(xx %in% c(0, 1))) stop("x must be binary (0/1).", call. = FALSE)
  if (!all(yy %in% c(0, 1))) {
    stop("y must be binary (0/1); this estimator is for a binary outcome.", call. = FALSE)
  }
  if (!all(is.finite(c(xx, yy))) || !all(is.finite(M))) {
    stop("x, m and y must be finite.", call. = FALSE)
  }
  if (sum(xx) < 2 || sum(1 - xx) < 2) {
    stop("Need at least 2 units per exposure arm; got ", sum(xx), " and ",
      sum(1 - xx), ".",
      call. = FALSE
    )
  }
  C <- if (is.null(covariates)) NULL else as.matrix(covariates)

  point <- function(idx) {
    xs <- xx[idx]
    ys <- yy[idx]
    ms <- M[idx, , drop = FALSE]
    cs <- if (is.null(C)) NULL else C[idx, , drop = FALSE]
    base <- if (is.null(cs)) ms else cbind(ms, cs)
    g <- stats::coef(suppressWarnings(stats::glm(xs ~ base, family = stats::binomial())))
    gm <- g[2:(1 + ncol(ms))]
    w <- rep(1, length(xs))
    ex <- xs == 1
    w[ex] <- exp(-as.vector(ms[ex, , drop = FALSE] %*% gm))
    Xt <- if (is.null(cs)) matrix(xs, ncol = 1L) else cbind(xs, cs)
    tot <- stats::coef(suppressWarnings(stats::glm(ys ~ Xt, family = stats::binomial())))[2]
    dir <- stats::coef(suppressWarnings(
      stats::glm(ys ~ Xt, family = stats::binomial(), weights = w)
    ))[2]
    c(total = unname(tot), direct = unname(dir))
  }

  pt <- point(seq_len(n))
  total <- pt[["total"]]
  direct <- pt[["direct"]]
  indirect <- total - direct
  se <- ci_lo <- ci_hi <- NULL
  B <- as.integer(B)
  if (B > 0L) {
    draws <- matrix(NA_real_, B, 3L)
    for (b in seq_len(B)) {
      idx <- sample.int(n, n, replace = TRUE)
      v <- tryCatch(point(idx), error = function(e) NULL)
      if (!is.null(v)) draws[b, ] <- c(v[["total"]], v[["direct"]], v[["total"]] - v[["direct"]])
    }
    good <- draws[stats::complete.cases(draws), , drop = FALSE]
    if (nrow(good) >= 2L) {
      nm <- c("total", "direct", "indirect")
      se <- stats::setNames(as.list(apply(good, 2L, stats::sd)), nm)
      ci_lo <- stats::setNames(as.list(apply(good, 2L, stats::quantile, probs = alpha / 2)), nm)
      ci_hi <- stats::setNames(as.list(apply(good, 2L, stats::quantile, probs = 1 - alpha / 2)), nm)
    }
  }
  list(
    total = total, direct = direct, indirect = indirect,
    or_total = exp(total), or_direct = exp(direct), or_indirect = exp(indirect),
    proportion_mediated = if (total != 0) indirect / total else NA_real_,
    se = se, ci_low = ci_lo, ci_high = ci_hi, n = n, B = B,
    method = "Inverse odds-ratio weighting (Tchetgen Tchetgen 2013), log-odds scale"
  )
}

#' Hilbert-Schmidt Independence Criterion
#'
#' \eqn{HSIC = tr(KHLH)/n^2} with RBF Gram matrices and the centring
#' matrix \eqn{H}. Zero exactly under independence, which is what an
#' additive-noise test needs: a nonlinear dependence can leave the
#' correlation at zero while the variables remain fully dependent.
#'
#' Mirrors \code{morie.fn.anmod}'s \code{hsic}.
#'
#' @param a,b Numeric vectors of equal length.
#' @return A single non-negative numeric.
#' @examples
#' set.seed(1)
#' morie_hsic(rnorm(100), rnorm(100))
#' @export
morie_hsic <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (length(a) != length(b)) {
    stop("a and b must be the same length; got ", length(a), " and ", length(b), ".", call. = FALSE)
  }
  n <- length(a)
  if (n < 4L) stop("HSIC needs at least 4 observations, got ", n, ".", call. = FALSE)
  gram <- function(v) {
    d2 <- outer(v, v, "-")^2
    med <- stats::median(d2[upper.tri(d2)])
    exp(-d2 / (2 * max(med / 2, 1e-12)))
  }
  H <- diag(n) - 1 / n
  sum(diag(gram(a) %*% H %*% gram(b) %*% H)) / n^2
}

# Internal: leave-one-out Nadaraya-Watson residuals.
#' Internal: leave-one-out Nadaraya-Watson residuals
#'
#' A step of the causal_native implementation. Called by \code{morie_anm_direction}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{outer}.
#' @param y A matrix; passed to \code{\%*\%}.
#' @return A numeric value.
#' @export
.anm_resid <- function(x, y) {
  d2 <- outer(x, x, "-")^2
  h <- sqrt(max(stats::median(d2[upper.tri(d2)]), 1e-12)) * 0.5
  W <- exp(-d2 / (2 * max(h, 1e-9)^2))
  diag(W) <- 0
  den <- rowSums(W)
  den[den <= 0] <- 1
  y - as.vector(W %*% y) / den
}

#' Causal direction from an additive noise model
#'
#' Fits \eqn{Y = f(X) + N} and \eqn{X = g(Y) + N} and reports the
#' direction whose residual is independent of its putative cause.
#' Independence is HSIC, not correlation.
#'
#' Three limits. The asymmetry vanishes for linear-Gaussian data, where
#' both directions admit independent noise, so \code{conclusive} reports
#' the tie rather than breaking it. A hidden common cause still yields a
#' named direction, which will be meaningless. And a saturating link
#' defeats the method: measured at 6/6 correct with a cubic link in
#' either orientation but 0/6 with \eqn{\tanh(3y)}, because outside the
#' active range the cause carries almost no information.
#'
#' Mirrors \code{morie.fn.anmod}.
#'
#' @param x,y Numeric vectors.
#' @param B Permutations for the independence p-values.
#' @return Named list with \code{direction}, \code{conclusive},
#'   \code{hsic_xy}, \code{hsic_yx}, \code{p_xy}, \code{p_yx}, \code{n}.
#' @references Hoyer PO, Janzing D, Mooij JM, Peters J & Scholkopf B
#'   (2009). \emph{NIPS 21}, 689-696.
#' @examples
#' set.seed(1)
#' xv <- runif(150, -2, 2)
#' morie_anm_direction(xv, xv^3 + rnorm(150, 0, 0.5), B = 49)$direction
#' @export
morie_anm_direction <- function(x, y, B = 200L) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) {
    stop("x and y must be the same length; got ", length(x), " and ", length(y), ".", call. = FALSE)
  }
  n <- length(x)
  if (n < 10L) stop("Need at least 10 observations to fit both directions, got ", n, ".", call. = FALSE)
  if (!all(is.finite(c(x, y)))) stop("x and y must be finite.", call. = FALSE)
  B <- as.integer(B)
  if (B < 1L) stop("B must be at least 1, got ", B, ".", call. = FALSE)

  r_xy <- .anm_resid(x, y)
  r_yx <- .anm_resid(y, x)
  h_xy <- morie_hsic(x, r_xy)
  h_yx <- morie_hsic(y, r_yx)
  pp <- function(a, r, obs) {
    (1 + sum(vapply(
      seq_len(B), function(i) morie_hsic(a, sample(r)) >= obs,
      logical(1)
    ))) / (1 + B)
  }
  p_xy <- pp(x, r_xy, h_xy)
  p_yx <- pp(y, r_yx, h_yx)

  if (p_xy > 0.05 && p_yx <= 0.05) {
    dir <- "X->Y"
    conc <- TRUE
  } else if (p_yx > 0.05 && p_xy <= 0.05) {
    dir <- "Y->X"
    conc <- TRUE
  } else {
    dir <- if (h_xy < h_yx) "X->Y" else "Y->X"
    conc <- FALSE
  }
  list(
    direction = dir, conclusive = conc, hsic_xy = h_xy, hsic_yx = h_yx,
    p_xy = p_xy, p_yx = p_yx, n = n, B = B,
    method = "Bivariate ANM with HSIC independence (Hoyer et al. 2009)"
  )
}
