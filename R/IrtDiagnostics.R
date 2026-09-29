.irx_clip <- function(p) pmin(pmax(p, 1e-9), 1 - 1e-9)

.irx_probit <- function(Z, y, b, prior_sd = NULL, max_iter = 100) {
  for (it in seq_len(max_iter)) {
    eta <- as.vector(Z %*% b)
    m <- .irx_clip(stats::pnorm(eta))
    d <- stats::dnorm(eta)
    w <- d^2 / (m * (1 - m))
    s <- d * (y - m) / (m * (1 - m))
    A <- crossprod(Z, Z * w)
    g <- as.vector(crossprod(Z, s))
    if (!is.null(prior_sd)) {
      A <- A + diag(1 / prior_sd^2, 2)
      g <- g - b / prior_sd^2
    }
    det <- A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1]
    if (det == 0) break
    st <- c(A[2, 2] * g[1] - A[1, 2] * g[2], A[1, 1] * g[2] - A[2, 1] * g[1]) / det
    b <- b + st
    if (max(abs(st)) < 1e-12 * max(1, max(abs(b)))) break
  }
  b
}

.irx_nll <- function(t, idx, y, tau) {
  p <- .irx_clip(stats::pnorm(idx / exp(t)))
  (if (is.null(tau)) 0 else 0.5 * t^2 / tau^2) - sum(y * log(p) + (1 - y) * log(1 - p))
}

.irx_score <- function(t, idx, y, tau) {
  z <- idx / exp(t)
  p <- stats::pnorm(z)
  k <- p > 1e-9 & p < 1 - 1e-9
  d <- stats::dnorm(z)
  (if (is.null(tau)) 0 else t / tau^2) - sum(((y * d / p - (1 - y) * d / (1 - p)) * (-z))[k])
}

#' Item response diagnostics: unpredictable voters, Lord's DIF test, SE of
#' theta and average variance extracted
#'
#' R arm of \code{morie.fn.hsirt}, \code{lordzs}, \code{semthe} and
#' \code{ave}. \code{Hsirt}: Lauderdale's heteroskedastic probit, per
#' legislator scales \eqn{\psi_i} in \eqn{\Phi((\beta_j x_i - \alpha_j)/\psi_i)}
#' given ideal points, alternating probit Fisher scoring for the items and
#' Brent plus Newton for each \eqn{\log\psi_i}, geometric mean of psi 1;
#' posterior mode under normal priors of standard deviation
#' \code{psi_prior_sd} on log psi and \code{item_prior_sd} on the items (NULL
#' for plain ML, which is degenerate for perfectly predicted voters).
#' \code{Lordzs}: Lord's chi-square test of equal item parameters in two
#' groups, \eqn{d'(\Sigma_R + \Sigma_F)^{-1} d}. \code{Semthe}: standard
#' error of an ability estimate from the 2PL test information.
#' \code{Ave}: average variance extracted, the mean squared standardised
#' loading.
#'
#' @param votes Binary vote matrix (NA missing).
#' @param ideal_points Fixed ideal points.
#' @param item_params Optional list of alpha and beta vectors.
#' @param max_iter Alternation rounds.
#' @param psi_prior_sd Prior standard deviation of log psi.
#' @param item_prior_sd Prior standard deviation of the item parameters.
#' @param b_R,b_F Item parameters in the reference and focal groups.
#' @param V_R,V_F Their covariances (V_R alone is the summed covariance).
#' @param theta Ability value(s).
#' @param items Two-column matrix of (a, b) item parameters.
#' @param loads Standardised factor loadings.
#' @return A list (the Python result's fields) or a number (\code{Ave}).
#' @references Lauderdale, B. E. (2010). Unpredictable voters in ideal point
#'   estimation. Political Analysis 18, 151-171.
#'
#'   Lord, F. M. (1980). Applications of Item Response Theory to Practical
#'   Testing Problems. Erlbaum.
#'
#'   Fornell, C. and Larcker, D. F. (1981). Evaluating structural equation
#'   models with unobservable variables and measurement error. Journal of
#'   Marketing Research 18, 39-50.
#' @examples
#' Lordzs(c(1.2, 0.3), c(0.9, 0.1), diag(0.02, 2), diag(0.03, 2))$statistic
#' Semthe(0, rbind(c(1, 0)))$se
#' Ave(c(0.7, 0.8, 0.9))
#' @export
Hsirt <- function(votes, ideal_points, item_params = NULL, max_iter = 50, psi_prior_sd = 1, item_prior_sd = 5) {
  V <- as.matrix(votes)
  x <- as.numeric(ideal_points)
  n <- nrow(V)
  q <- ncol(V)
  if (n != length(x)) stop("votes must be (n, q) with one ideal point per row.")
  if (any(!is.na(V) & !(V %in% c(0, 1)))) stop("votes must be binary 0/1 (NA for missing).")
  if (!is.null(item_params)) {
    alpha <- as.numeric(item_params[[1]])
    beta <- as.numeric(item_params[[2]])
    fixed <- TRUE
  } else {
    alpha <- rep(0, q)
    beta <- rep(1, q)
    fixed <- FALSE
  }
  psi <- rep(1, n)
  fit_items <- function(psi) {
    ab <- vapply(seq_len(q), function(j) {
      r <- which(!is.na(V[, j]))
      y <- V[r, j]
      if (length(r) < 3 || min(y) == max(y)) return(c(0, 0))
      .irx_probit(cbind(-1 / psi[r], x[r] / psi[r]), y, c(0, 1), item_prior_sd)
    }, numeric(2))
    list(alpha = ab[1, ], beta = ab[2, ])
  }
  for (it in seq_len(max_iter)) {
    if (!fixed) {
      ab <- fit_items(psi)
      alpha <- ab$alpha
      beta <- ab$beta
    }
    new <- psi
    for (i in seq_len(n)) {
      cc <- which(!is.na(V[i, ]))
      y <- V[i, cc]
      if (length(cc) < 3 || min(y) == max(y)) next
      idx <- beta[cc] * x[i] - alpha[cc]
      t <- .sxd_brent(function(s) .irx_nll(s, idx, y, psi_prior_sd), -3, 3)[1]
      for (k in 1:20) {
        g <- .irx_score(t, idx, y, psi_prior_sd)
        dg <- (.irx_score(t + 1e-6, idx, y, psi_prior_sd) - .irx_score(t - 1e-6, idx, y, psi_prior_sd)) / 2e-6
        if (!(dg > 0)) break
        tn <- t - g / dg
        if (!(tn > -3 && tn < 3)) break
        st <- abs(tn - t)
        t <- tn
        if (st < 1e-15) break
      }
      new[i] <- exp(t)
    }
    new <- new / exp(mean(log(new)))
    done <- max(abs(new - psi)) < 1e-6
    psi <- new
    if (done) break
  }
  if (!fixed) {
    ab <- fit_items(psi)
    alpha <- ab$alpha
    beta <- ab$beta
  }
  P <- .irx_clip(stats::pnorm(sweep(outer(x, beta), 2, alpha) / psi))
  ll <- sum((V * log(P) + (1 - V) * log(1 - P))[!is.na(V)])
  list(psi = psi, alpha = alpha, beta = beta, loglik = ll, n = n, q = q)
}

#' @rdname Hsirt
#' @export
Lordzs <- function(b_R, b_F, V_R, V_F = NULL) {
  d <- as.numeric(b_R) - as.numeric(b_F)
  S <- as.matrix(V_R)
  if (!is.null(V_F)) S <- S + as.matrix(V_F)
  if (any(dim(S) != length(d))) stop("covariance must match the number of parameters")
  stat <- sum(d * solve(S, d))
  if (stat < 0) stop("negative quadratic form: the summed covariance is not positive definite")
  list(statistic = stat, pvalue = stats::pchisq(stat, length(d), lower.tail = FALSE), df = length(d), difference = d)
}

#' @rdname Hsirt
#' @export
Semthe <- function(theta, items) {
  items <- as.matrix(items)
  if (ncol(items) != 2) stop("items must be (k, 2) rows of (a, b)")
  info <- vapply(theta, function(t) {
    P <- 1 / (1 + exp(-items[, 1] * (t - items[, 2])))
    sum(items[, 1]^2 * P * (1 - P))
  }, 0)
  if (any(info <= 0)) stop("test information is zero; SE undefined")
  list(se = 1 / sqrt(info), estimate = 1 / sqrt(info), information = info, theta = theta, n_items = nrow(items))
}

#' @rdname Hsirt
#' @export
Ave <- function(loads) {
  mean(as.numeric(loads)^2)
}
