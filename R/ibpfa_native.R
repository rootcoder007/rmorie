# The Indian buffet process: latent features without fixing how many.
#
# Sources:
#   Griffiths, T. L. & Ghahramani, Z. (2011) "The Indian Buffet
#   Process: An Introduction and Review", JMLR 12, 1185-1224.
#   Griffiths, T. L. & Ghahramani, Z. (2006) "Infinite Latent Feature
#   Models and the Indian Buffet Process", NIPS 2005, 475-482.
#   Teh, Y. W., Gorur, D. & Ghahramani, Z. (2007) "Stick-breaking
#   Construction for the Indian Buffet Process", AISTATS 2007,
#   PMLR 2, 556-563.

#' sample_ibp
#'
#' A step of the ibpfa_native implementation. Called by \code{morie_ibpfa}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0L}.
#' @return A list with \code{Z}, \code{K}, \code{counts}, \code{alpha}, \code{n},
#' \code{features_per_object}, \code{note}.
#' @export
sample_ibp <- function(n, alpha, seed = 0L) {
  N <- as.integer(n)
  a <- as.numeric(alpha)
  if (N < 1L || a <= 0.0) {
    stop("ibpfa: need n >= 1 and alpha > 0")
  }
  e <- .ghc_rng(seed)
  rows <- list()
  counts <- integer(0)
  for (i in 1:N) {
    row <- integer(0)
    if (length(counts) > 0L) {
      u <- .ghc_unif(e, length(counts))
      take <- as.integer(u < counts / i)
      for (kk in seq_along(counts)) {
        row <- c(row, take[kk])
        counts[kk] <- counts[kk] + take[kk]
      }
    }
    lam <- a / i
    # inverse-CDF Poisson via Knuth; cap at 100 as in the Python.
    L <- exp(-lam)
    p <- .ghc_unif(e, 1L)
    cum <- L
    new <- 0L
    while (p > cum && new < 100L) {
      new <- new + 1L
      L <- L * lam / new
      cum <- cum + L
    }
    if (new > 0L) {
      row <- c(row, rep(1L, new))
      counts <- c(counts, rep(1L, new))
    }
    rows[[i]] <- row
  }
  K <- length(counts)
  Z <- matrix(0L, N, K)
  for (i in 1:N) {
    ri <- rows[[i]]
    if (length(ri) > 0L) {
      Z[i, seq_along(ri)] <- ri
    }
  }
  fpo <- rowSums(Z)
  list(Z = Z, K = K, counts = as.integer(counts), alpha = a, n = N,
       features_per_object = as.integer(fpo),
       note = paste("the number of features is INFERRED, not fixed"))
}

#' expected_features
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{expected_total_features}, \code{harmonic},
#' \code{expected_per_object}, \code{expected_nonzeros}, \code{note}.
#' @export
expected_features <- function(n, alpha) {
  N <- as.integer(n)
  a <- as.numeric(alpha)
  if (N < 1L || a <= 0.0) {
    stop("ibpfa: need n >= 1 and alpha > 0")
  }
  H <- sum(1.0 / (1:N))
  list(expected_total_features = a * H, harmonic = H,
       expected_per_object = a, expected_nonzeros = a * N,
       note = paste("total grows like alpha log n; per object it is CONSTANT at alpha"))
}

#' left_ordered_form
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Z A matrix; passed to \code{as.matrix}.
#' @return A list with \code{Z}, \code{order}, \code{note}.
#' @export
left_ordered_form <- function(Z) {
  M <- as.matrix(Z)
  storage.mode(M) <- "integer"
  if (length(M) == 0L) stop("ibpfa: the matrix is empty")
  n <- nrow(M)
  K <- ncol(M)
  hist <- matrix(0L, K, 2)
  for (kk in 1:K) {
    h <- 0L
    for (i in 1:n) {
      h <- bitwShiftL(h, 1L) | as.integer(M[i, kk])
    }
    hist[kk, 1L] <- h
    hist[kk, 2L] <- kk
  }
  ord <- order(-hist[, 1L], hist[, 2L])
  list(Z = M[, ord, drop = FALSE], order = as.integer(ord),
       note = paste("columns are an unordered SET; left-ordering picks the canonical representative"))
}

#' ibp_log_probability
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Z A matrix; passed to \code{as.matrix}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{lp}, as built in the body.
#' @export
ibp_log_probability <- function(Z, alpha) {
  M <- as.matrix(Z)
  storage.mode(M) <- "integer"
  n <- nrow(M)
  K <- if (length(M) > 0L) ncol(M) else 0L
  a <- as.numeric(alpha)
  if (a <= 0.0) stop("ibpfa: alpha must be positive")
  H <- sum(1.0 / (1:n))
  lp <- K * log(a) - a * H
  for (kk in 1:K) {
    m_k <- sum(M[, kk])
    if (m_k == 0L) next
    lp <- lp + lgamma(n - m_k + 1) + lgamma(m_k) - lgamma(n + 1)
  }
  lp
}

#' gibbs_feature_update
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Z A matrix; passed to \code{as.matrix}.
#' @param i Numeric; combined arithmetically in the body.
#' @param kk See Usage.
#' @param likelihood Accepted by the signature and not used anywhere in the body.
#' @param alpha Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{p_on}, \code{prior}, \code{z}, \code{note}.
#' @export
gibbs_feature_update <- function(Z, i, kk, likelihood, alpha) {
  M <- as.matrix(Z)
  storage.mode(M) <- "integer"
  n <- nrow(M)
  m_minus <- sum(M[-i, kk])
  if (m_minus == 0L) {
    return(list(z = 0L, prior = 0.0,
                note = paste("a feature held by nobody else is dropped; new ones arrive through the Poisson draw")))
  }
  prior <- m_minus / n
  on <- M
  off <- M
  on[i, kk] <- 1L
  off[i, kk] <- 0L
  l1 <- as.numeric(likelihood(on)) + log(max(prior, 1e-12))
  l0 <- as.numeric(likelihood(off)) + log(max(1.0 - prior, 1e-12))
  mx <- max(l1, l0)
  p1 <- exp(l1 - mx) / (exp(l1 - mx) + exp(l0 - mx))
  list(p_on = p1, prior = prior, z = as.integer(p1 > 0.5),
       note = paste("exchangeability is what licenses treating i as the last customer"))
}

indianbuffet <- sample_ibp
indian_buffet_factor <- sample_ibp

#' morie_ibpfa
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Passed to \code{sample_ibp}.
#' @param alpha Passed to \code{sample_ibp}.
#' @param seed Passed to \code{sample_ibp}. Defaults to \code{0L}.
#' @return The value of \code{sample_ibp}.
#' @export
morie_ibpfa <- function(n, alpha, seed = 0L) {
  sample_ibp(n, alpha, seed)
}

#' .ibpfa_cheatsheet
#'
#' A step of the ibpfa_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .ibpfa_cheatsheet()
#' res
.ibpfa_cheatsheet <- function() {
  paste(paste0(
    "ibpfa: objects have SEVERAL latent features, and how many ex",
    "ist is unknown -- so use a distribution over binary matrices",
    " with unboundedly many columns. Customer i takes an existing",
    " dish with probability m_k/i (popularity self-reinforces) an",
    "d Poisson(alpha/i) NEW dishes (the flow decays as 1/i). Two ",
    "different numbers: expected TOTAL features alpha*H_n ~ alpha",
    " log n, expected features PER OBJECT constant at alpha. The ",
    "left-ordered form is EXCHANGEABLE, which is what licenses Gi",
    "bbs sampling by treating any object as the last to arrive."
  ))
}
