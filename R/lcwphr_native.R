# R arm of lcwphr -- latent class analysis by EM with inverse-probability-
# weighted class-specific treatment effects.
# Lanza, S. T., Coffman, D. L. & Xu, S. (2013) Structural Equation Modeling
# 20(3), 361-383; Goodman, L. A. (1974) Biometrika 61(2), 215-231;
# Robins, J. M., Hernan, M. A. & Brumback, B. (2000) Epidemiology 11(5),
# 550-560.
# Mirrors src/morie/fn/lcwphr.py.

.lcwphr_EPS <- 1e-12

.lcwphr_rows <- function(x) {
  if (is.matrix(x)) {
    m <- x
  } else if (is.data.frame(x)) {
    m <- as.matrix(x)
  } else if (is.list(x)) {
    m <- do.call(rbind, lapply(x, as.numeric))
  } else {
    m <- matrix(as.numeric(x), nrow = 1L)
  }
  storage.mode(m) <- "double"
  m
}

.lcwphr_cholsolve <- function(A, b) {
  Lc <- chol(A)
  as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
}

# Logistic regression by IRLS; the ridge is scaled to the design.
.lcwphr_logit_irls <- function(X, y, max_iter = 100L, tol = 1e-11,
                               ridge_rel = 1e-8) {
  n <- nrow(X); p <- ncol(X)
  beta <- numeric(p)
  for (i in seq_len(as.integer(max_iter))) {
    eta <- as.numeric(X %*% beta)
    mu <- 1.0 / (1.0 + exp(-pmax(-500.0, pmin(500.0, eta))))
    w <- pmax(mu * (1.0 - mu), 1e-10)
    z <- eta + (y - mu) / w
    A <- crossprod(X * w, X)
    scale_ <- sum(diag(A)) / p
    diag(A) <- diag(A) + ridge_rel * max(scale_, .lcwphr_EPS)
    new <- .lcwphr_cholsolve(A, as.numeric(crossprod(X, w * z)))
    shift <- max(abs(new - beta))
    beta <- new
    if (shift < tol) break
  }
  eta <- as.numeric(X %*% beta)
  list(beta = beta,
       fitted = 1.0 / (1.0 + exp(-pmax(-500.0, pmin(500.0, eta)))))
}

#' morie_lcwphr_latent_class_weighted
#'
#' Part of the lcwphr_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param K See Usage.
#' @param trim Defaults to \code{0}.
#' @param stabilize Defaults to \code{TRUE}.
#' @param max_iter Defaults to \code{500L}.
#' @param tol Defaults to \code{1e-11}.
#' @return A list with \code{estimate}, \code{class_ate}, \code{class_mean_treated}, \code{class_mean_control}, \code{naive_class_ate}, \code{naive_class_mean_treated}, \code{naive_class_mean_control}, \code{ate}, \code{naive_ate}, \code{marginal_ate}, \code{unweighted_ate}, \code{class_prevalence}, \code{item_probabilities}, \code{posterior}, \code{labels}, \code{propensity}, \code{propensity_coefficients}, \code{weights}, \code{effective_sample_size}, \code{weight_max}, \code{weight_mean}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n}, \code{Q}, \code{stabilized}, \code{trim}, \code{method}, \code{note}.
#' @export
morie_lcwphr_latent_class_weighted <- function(y, A, H, K, trim = 0.0,
                                               stabilize = TRUE,
                                               max_iter = 500L,
                                               tol = 1e-11) {
  yv <- as.numeric(y)
  av <- as.numeric(A)
  Hm <- .lcwphr_rows(H)
  n <- length(yv)
  if (n == 0L) stop("lcwphr: no observations")
  if (length(av) != n || nrow(Hm) != n)
    stop(sprintf(paste0("lcwphr: y, A and H must agree in length ",
                        "(%d, %d, %d)"), n, length(av), nrow(Hm)))
  Q <- ncol(Hm)
  for (i in seq_len(n)) for (q in seq_len(Q))
    if (!(Hm[i, q] %in% c(0.0, 1.0)))
      stop(sprintf(paste0("lcwphr: the manifest indicators must be binary; ",
                          "H[%d][%d] = %g"), i - 1L, q - 1L, Hm[i, q]))
  if (any(!(av %in% c(0.0, 1.0))))
    stop("lcwphr: the treatment must be binary")
  if (!any(av > 0.5) || !any(av < 0.5))
    stop(paste0("lcwphr: both treatment arms must be occupied -- no ",
                "contrast is identified from one arm"))
  K <- as.integer(K)
  if (K < 1L) stop("lcwphr: K must be at least 1")
  tr <- as.numeric(trim)
  if (!(tr >= 0.0 && tr < 0.5)) stop("lcwphr: trim must be in [0, 0.5)")

  # deterministic initialisation: rank subjects by how many indicators they
  # endorse and cut into K groups. No random start to disagree over.
  tot <- rowSums(Hm)
  ord <- order(tot, seq_len(n))
  lab0 <- integer(n)
  for (rank in seq_len(n))
    # R binds %/% TIGHTER than *, so (rank-1L) * K %/% n would be
    # (rank-1L) * (K %/% n) = 0 and the whole initial partition would
    # collapse into one class. The parentheses are load-bearing.
    lab0[ord[rank]] <- min(((rank - 1L) * K) %/% n, K - 1L)
  pi_ <- numeric(K)
  rho <- matrix(0.0, K, Q)
  for (j in seq_len(K)) {
    idx <- which(lab0 == (j - 1L))
    if (length(idx) == 0L) idx <- ord[min(j, n)]
    pi_[j] <- length(idx) / n
    for (q in seq_len(Q)) {
      # shrunk towards 1/2 so no probability starts at a boundary, where
      # the EM update has nowhere to move
      s <- sum(Hm[idx, q])
      rho[j, q] <- (s + 0.5) / (length(idx) + 1.0)
    }
  }

  post <- matrix(0.0, n, K)
  ll <- -Inf; path <- numeric(0); it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    ll_new <- 0.0
    for (i in seq_len(n)) {
      lp <- numeric(K)
      for (j in seq_len(K)) {
        s <- log(max(pi_[j], 1e-300))
        for (q in seq_len(Q)) {
          r <- min(max(rho[j, q], 1e-12), 1.0 - 1e-12)
          s <- s + if (Hm[i, q] > 0.5) log(r) else log(1.0 - r)
        }
        lp[j] <- s
      }
      mx <- max(lp)
      tot_i <- sum(exp(lp - mx))
      ll_new <- ll_new + mx + log(tot_i)
      post[i, ] <- exp(lp - mx) / tot_i
    }
    path <- c(path, ll_new)
    if (it > 1L && abs(ll_new - ll) <= tol * (abs(ll) + 1.0)) {
      ll <- ll_new; converged <- TRUE; break
    }
    ll <- ll_new
    for (j in seq_len(K)) {
      nk <- sum(post[, j])
      pi_[j] <- nk / n
      nk <- max(nk, 1e-300)
      for (q in seq_len(Q)) rho[j, q] <- sum(post[, j] * Hm[, q]) / nk
    }
  }

  # canonical order: prevalence descending, ties by the first indicator
  ordk <- order(-pi_, -rho[, 1], seq_len(K))
  pi_ <- pi_[ordk]
  rho <- rho[ordk, , drop = FALSE]
  post <- post[, ordk, drop = FALSE]
  # REPORTED labels follow the Python spec and are 0-based
  labels <- apply(post, 1L, which.max) - 1L

  # ---- propensity for treatment given the same indicators
  Xp <- cbind(1.0, Hm)
  pf <- .lcwphr_logit_irls(Xp, av)
  ps <- if (tr > 0.0) pmin(pmax(pf$fitted, tr), 1.0 - tr) else
    pmin(pmax(pf$fitted, 1e-8), 1.0 - 1e-8)
  marg <- sum(av) / n
  den <- ifelse(av > 0.5, ps, 1.0 - ps)
  nmr <- if (isTRUE(stabilize)) ifelse(av > 0.5, marg, 1.0 - marg) else 1.0
  w <- nmr / den

  contrast <- function(weights) {
    num1 <- sum(weights * av * yv); den1 <- sum(weights * av)
    num0 <- sum(weights * (1.0 - av) * yv); den0 <- sum(weights * (1.0 - av))
    if (den1 <= .lcwphr_EPS || den0 <= .lcwphr_EPS)
      return(c(NaN, NaN, NaN))
    c(num1 / den1 - num0 / den0, num1 / den1, num0 / den0)
  }

  class_ate <- numeric(K); class_m1 <- numeric(K); class_m0 <- numeric(K)
  naive_ate <- numeric(K); naive_m1 <- numeric(K); naive_m0 <- numeric(K)
  for (j in seq_len(K)) {
    r1 <- contrast(post[, j] * w)
    class_ate[j] <- r1[1]; class_m1[j] <- r1[2]; class_m0[j] <- r1[3]
    r0 <- contrast(post[, j])
    naive_ate[j] <- r0[1]; naive_m1[j] <- r0[2]; naive_m0[j] <- r0[3]
  }

  ate <- sum(pi_ * class_ate)
  naive <- sum(pi_ * naive_ate)
  marginal_ate <- contrast(w)[1]
  unweighted_ate <- contrast(rep(1.0, n))[1]

  nfree <- K - 1L + K * Q
  bic <- -2.0 * ll + nfree * log(n)
  ess <- sum(w) ^ 2 / max(sum(w * w), 1e-300)
  ent <- -sum(post * log(pmax(post, 1e-300)))

  list(estimate = class_ate, class_ate = class_ate,
       class_mean_treated = class_m1, class_mean_control = class_m0,
       naive_class_ate = naive_ate,
       naive_class_mean_treated = naive_m1,
       naive_class_mean_control = naive_m0,
       ate = ate, naive_ate = naive,
       marginal_ate = marginal_ate, unweighted_ate = unweighted_ate,
       class_prevalence = pi_, item_probabilities = rho,
       posterior = post, labels = as.numeric(labels),
       propensity = ps, propensity_coefficients = pf$beta,
       weights = w, effective_sample_size = ess,
       weight_max = max(w), weight_mean = sum(w) / n,
       loglik = ll, loglik_path = path, bic = bic, entropy = ent,
       n_parameters = as.integer(nfree), iterations = as.integer(it),
       converged = converged,
       K = as.integer(K), n = as.integer(n), Q = as.integer(Q),
       stabilized = isTRUE(stabilize), trim = tr,
       method = paste0("latent class analysis by EM with inverse-",
                       "probability-weighted class-specific treatment ",
                       "effects, subjects counted in proportion to ",
                       "posterior membership (Lanza, Coffman & Xu 2013; ",
                       "Robins et al. 2000)"),
       note = paste0("class_ate and naive_class_ate coincide when treatment ",
                     "was unrelated to the indicators and separate when it ",
                     "was not -- the gap is what the weighting is for; ",
                     "classes are returned in prevalence order because the ",
                     "model is identified only up to relabelling"))
}

.lcwphr_cheatsheet <- function() {
  paste0("lcwphr: morie_lcwphr_latent_class_weighted(y, A, H, K) -> latent ",
         "classes plus IPW class-specific treatment effects (Lanza, Coffman ",
         "& Xu 2013, Structural Equation Modeling 20:361-383)")
}

morie_lcwphr <- morie_lcwphr_latent_class_weighted
