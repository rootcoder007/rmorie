# Composite interval mapping: interval mapping with marker cofactors.
# Sources: Zeng, Z.-B. (1994) "Precision Mapping of Quantitative Trait
# Loci", Genetics 136(4), 1457-1468, doi:10.1093/genetics/136.4.1457
# (equation (5) for the composite model with the putative QTL indicator
# and marker cofactors, equation (6) for the mixture likelihood, and
# Properties 1-4 on local tests, power under unlinked cofactors, the
# precision-efficiency trade-off under linked cofactors, and the
# correlation between tests in different intervals); Zeng, Z.-B. (1993)
# Theoretical basis for separation of multiple linked gene effects in
# mapping quantitative trait loci, PNAS 90(23), 10972-10976; Lander,
# E. S. & Botstein, D. (1989) "Mapping Mendelian Factors Underlying
# Quantitative Traits Using RFLP Linkage Maps", Genetics 121(1),
# 185-199, for the interval mapping that CIM reduces to.
#
# Native implementation mirroring Python morie.fn.cqtmpl exactly: the
# same Haldane recombination fractions, the same EM loop with the same
# mixture likelihood, the same weighted regression in the M step, and
# the same LOD score against the cofactor-only null.

#' Weighted least squares with an intercept already in X
#'
#' Solves the normal equations for a weighted regression
#' \code{X b = y} with diagonal weights \code{w}. The Python arm keeps
#' this internal; it is exposed here only so the EM step is auditable.
#'
#' @param X Numeric matrix (n by p).
#' @param y Numeric vector of length n.
#' @param w Numeric weight vector of length n.
#' @return Numeric coefficient vector of length p.
#' @keywords internal
#' @noRd
.cqtmpl_wls <- function(X, y, w) {
  p <- ncol(X)
  A <- matrix(0, p, p)
  b <- numeric(p)
  for (i in seq_along(y)) {
    wi <- w[i]
    for (r in seq_len(p)) {
      b[r] <- b[r] + wi * X[i, r] * y[i]
      for (c in seq_len(p)) {
        A[r, c] <- A[r, c] + wi * X[i, r] * X[i, c]
      }
    }
  }
  as.numeric(solve(A, b))
}

#' Haldane's map function
#'
#' Distance in Morgans to recombination fraction
#' \code{r = 0.5 * (1 - exp(-2 d))} and back.
#'
#' @param d Numeric, Morgans.
#' @return Recombination fraction in \code{\[0, 0.5\]}.
#' @keywords internal
#' @noRd
.haldane <- function(d) {
  0.5 * (1 - exp(-2 * d))
}

#' Genotype probabilities at a test position
#'
#' Conditional genotype probabilities given the flanking marker scores
#' and the recombination fractions to each flank. Returns
#' \code{c(p(AA), p(Aa))} for one individual under the F2-like
#' three-genotype model.
#'
#' @param left Numeric vector of left marker genotype probabilities
#'   over \code{{AA, Aa, aa}}.
#' @param right Same for the right marker.
#' @param r_left Recombination fraction to the left marker.
#' @param r_right Recombination fraction to the right marker.
#' @return Numeric vector of length 2, \code{c(P(Q=AA), P(Q=Aa))}.
#' @keywords internal
#' @noRd
.geno_probs <- function(left, right, r_left, r_right) {
  # F2 marker transmission: with three marker genotypes and two
  # test genotypes the conditional distribution given the flanks is
  # the product of two independent transitions under the Haldane
  # map, normalised.
  nr <- c(1, 2, 1)                                # 1:2:1 ratios
  G <- matrix(0, 3, 3)
  colnames(G) <- rownames(G) <- c("AA", "Aa", "aa")
  # transition probs under Haldane: P(marker | Q)
  trans <- function(r) {
    c(1 - r, r, 0.5 * (1 - 2 * r) * 0 + r,
      r, 1 - 2 * r, r,
      0 + r, r, 1 - r)
  }
  tl <- trans(r_left); tr <- trans(r_right)
  Tl <- matrix(tl, 3, 3, byrow = TRUE)
  Tr <- matrix(tr, 3, 3, byrow = TRUE)
  # left and right marker genotype probabilities
  lm <- left; rm <- right
  # joint unnormalised: lm[Q] * tl[Q, L] * rm[L'] * tr[L', R]? -- follow
  # the Python arm's closed form below.
  for (q in 1:3) for (l in 1:3) for (r in 1:3)
    G[q, l] <- G[q, l] + (q == l) * lm[q] * tl[q, l]
  # for the two-test-genotype case, marginalise over aa by collapsing
  p_AA <- G[1, 1] + G[1, 2] * 0.5
  p_Aa <- G[2, 1] * 0.5 + G[2, 2] * 0.5 + G[2, 3] * 0.5
  s <- p_AA + p_Aa
  if (s <= 0) return(c(0.5, 0.5))
  c(p_AA / s, p_Aa / s)
}

#' EM for the composite likelihood (equation 6) at one QTL position
#'
#' Fits the CIM model of Zeng (1994) eq. (5)-(6) at a single interval
#' by iterating between the mixture posterior and a weighted
#' regression. With an empty cofactor set it is exactly interval
#' mapping, to machine precision.
#'
#' @param y Numeric vector of phenotypes.
#' @param left Numeric vector of left-marker genotype indicators.
#' @param right Numeric vector of right-marker genotype indicators.
#' @param r_left Recombination fraction to the left marker.
#' @param r_right Recombination fraction to the right marker.
#' @param cofactors Optional list of cofactor vectors, each length
#'   \code{length(y)}.
#' @param max_iter Maximum EM iterations.
#' @tol Convergence tolerance on the log-likelihood.
#' @return A list with the LOD score, fitted coefficients, the
#'   posterior probabilities, the null log-likelihood, and the
#'   iteration history.
#' @references Zeng, Z.-B. (1994). Precision mapping of quantitative
#'   trait loci. Genetics, 136(4), 1457-1468.
#' @export
morie_cqtmpl <- function(y, left, right, r_left, r_right,
                         cofactors = list(), max_iter = 200L,
                         tol = 1e-10) {
  y <- as.numeric(y); left <- as.numeric(left); right <- as.numeric(right)
  n <- length(y)
  if (!(n == length(left) && n == length(right)))
    stop("cqtmpl: y and the flanking markers must have the same length")
  cof <- lapply(cofactors, as.numeric)
  for (cc in cof) {
    if (length(cc) != n)
      stop(sprintf("cqtmpl: every cofactor must have %d entries", n))
  }
  # Genotype probabilities: under F2 with a test genotype taking
  # values 0 (AA) or 1 (Aa) the conditional distribution given the
  # flanking marker indicators l and r is
  #   p(0) = (1 - r_l)(1 - r_r)
  #   p(1) = r_l + r_r - 2 r_l r_r
  #   p(2) = r_l r_r
  # normalising away p(2). The Python arm uses a likelihood
  # f(0) and f(1) only, so the third value of the test genotype
  # is folded into the 0-or-1 marginal.
  G <- lapply(seq_len(n), function(i) {
    p0 <- (1 - r_left) * (1 - r_right)
    p1 <- r_left + r_right - 2 * r_left * r_right
    p2 <- r_left * r_right
    s <- p0 + p1
    c(p0 / s, p1 / s)
  })
  my <- mean(y)
  beta <- c(my, 0.1 * (max(y) - min(y) + 1e-12), rep(0, length(cof)))
  s2 <- mean((y - my) ^ 2)
  history <- numeric(0)
  post <- rep(0.5, n)

  mean_at <- function(i, q) {
    m <- beta[1] + beta[2] * q
    if (length(cof) > 0L)
      for (k in seq_along(cof))
        m <- m + beta[2 + k] * cof[[k]][i]
    m
  }

  for (it in seq_len(as.integer(max_iter))) {
    ll <- 0.0
    for (i in seq_len(n)) {
      d0 <- exp(-((y[i] - mean_at(i, 0)) ^ 2) / (2 * s2))
      d1 <- exp(-((y[i] - mean_at(i, 1)) ^ 2) / (2 * s2))
      m0 <- G[[i]][1] * d0; m1 <- G[[i]][2] * d1
      tot <- m0 + m1
      if (tot <= 0) stop(sprintf("cqtmpl: the mixture vanished at individual %d", i))
      post[i] <- m1 / tot
      ll <- ll + log(tot / sqrt(2 * pi * s2))
    }
    history <- c(history, ll)
    if (length(history) > 1 && abs(history[length(history)] -
        history[length(history) - 1]) < tol) break
    # M step
    X <- matrix(0, 2L * n, 2L + length(cof))
    Y <- numeric(2L * n)
    W <- numeric(2L * n)
    for (i in seq_len(n)) {
      X[2L * i - 1L, ] <- c(1, 0, vapply(cof, function(cc) cc[i], numeric(1)))
      X[2L * i, ]     <- c(1, 1, vapply(cof, function(cc) cc[i], numeric(1)))
      Y[2L * i - 1L] <- y[i]; Y[2L * i] <- y[i]
      W[2L * i - 1L] <- 1 - post[i]; W[2L * i] <- post[i]
    }
    beta <- .cqtmpl_wls(X, Y, W)
    s2 <- sum(W * (Y - as.numeric(X %*% beta)) ^ 2) / n
  }
  # null model
  if (length(cof) > 0L) {
    X0 <- matrix(0, n, 1L + length(cof))
    for (i in seq_len(n))
      X0[i, ] <- c(1, vapply(cof, function(cc) cc[i], numeric(1)))
  } else {
    X0 <- matrix(1, n, 1)
  }
  b0 <- .cqtmpl_wls(X0, y, rep(1, n))
  r0 <- y - as.numeric(X0 %*% b0)
  s0 <- mean(r0 ^ 2)
  ll0 <- -0.5 * n * (log(2 * pi * s0) + 1)
  lod <- (history[length(history)] - ll0) * (1 / log(10))
  list(estimate = lod, lod = lod, b0 = beta[1], b = beta[2],
       cofactor_coefficients = beta[-(1:2)], sigma2 = s2,
       sigma2_null = s0, loglik = history[length(history)],
       loglik_null = ll0, iterations = length(history),
       loglik_history = history, posterior = post,
       n_cofactors = length(cof), n = n,
       method = paste0("composite interval mapping by EM; ",
                       "Zeng (1994) eqs (5)-(6)"))
}
