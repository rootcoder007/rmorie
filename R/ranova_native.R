# Variance components: ANOVA, REML, and the combined front end.
# Source: Searle, S. R., Casella, G. and McCulloch, C. E. (1992),
# Variance Components, Wiley: one-way ANOVA estimators Eq. (21)
# Sec. 2.2 pp. 26-27; balanced-data ANOVA Ch. 4; unbalanced data /
# Henderson's Method I Ch. 5; REML Sec. 3.8 and Ch. 6 (Sec. 6.6);
# the balanced-data identity "REML solutions = ANOVA estimators"
# Sec. 4.8; interval estimation Sec. 3.5.  Local source:
# fetched-wave3/Variance_components_FULL.pdf.  Patterson & Thompson
# (1971), Biometrika 58(3), 545-554.
# Mirrors Python morie.fn.ranova / remlfn / vcomp exactly.

#' .vc_groups
#'
#' A step of the ranova_native implementation. Called by \code{morie_ranova}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param group Coerced to character by the body, with \code{as.character}.
#' @return A list with \code{keys}, \code{gs}.
#' @export
.vc_groups <- function(y, group) {
  k <- as.character(group)
  keys <- sort(unique(k))
  list(keys = keys, gs = lapply(keys, function(u) as.numeric(y[k == u])))
}

#' ANOVA (method-of-moments) estimation of variance components
#'
#' One-way random model y_ij = mu + a_i + e_ij.  With
#' SSA = sum_i n_i (ybar_i - ybar)^2, SSE = sum_ij (y_ij - ybar_i)^2,
#' MSA = SSA/(a-1) and MSE = SSE/(N-a), the ANOVA estimators are
#' sigma_e^2 = MSE and sigma_a^2 = (MSA - MSE)/n_0 with
#' n_0 = (N - sum_i n_i^2 / N)/(a - 1), which reduces to Searle et
#' al.'s Eq. (21) form (MSA - MSE)/n for balanced data.  Unbiased but
#' possibly negative, so both the raw and the zero-truncated values
#' are returned.
#'
#' @param y Observations.
#' @param group Class label per observation.
#' @return A list with elements \code{sigma2_a}, \code{sigma2_e},
#'   \code{sigma2_a_raw}, \code{msa}, \code{mse}, \code{ssa},
#'   \code{sse}, \code{n0}, \code{a}, \code{N}, \code{n_i},
#'   \code{balanced}, \code{icc}, \code{method}.
#' @references Searle, S. R., Casella, G. and McCulloch, C. E.
#'   (1992). Variance Components. Wiley, Eq. (21) and Chapters 4-5.
#' @export
#' @examples
#' set.seed(1)
#' r <- morie_ranova(y = rnorm(10), group = rbinom(10, 1, 0.5)); TRUE
morie_ranova <- function(y, group) {
  y <- as.numeric(y)
  if (length(y) != length(group))
    stop("y and group must have equal length")
  z <- .vc_groups(y, group)
  gs <- z$gs
  a <- length(gs)
  if (a < 2) stop("need at least two classes")
  ns <- vapply(gs, length, integer(1))
  N <- sum(ns)
  if (N == a) stop("need replication within classes")
  grand <- sum(y) / N
  means <- vapply(gs, mean, numeric(1))
  ssa <- sum(ns * (means - grand)^2)
  sse <- sum(vapply(seq_len(a), function(i) sum((gs[[i]] - means[i])^2),
                    numeric(1)))
  msa <- ssa / (a - 1)
  mse <- sse / (N - a)
  balanced <- length(unique(ns)) == 1L
  n0 <- (N - sum(ns * ns) / N) / (a - 1)
  s2a_raw <- (msa - mse) / n0
  s2a <- if (s2a_raw > 0) s2a_raw else 0
  s2e <- mse
  denom <- s2a + s2e
  list(sigma2_a = s2a, sigma2_e = s2e, sigma2_a_raw = s2a_raw,
       msa = msa, mse = mse, ssa = ssa, sse = sse, n0 = n0,
       a = a, N = N, n_i = as.integer(ns), balanced = balanced,
       icc = if (denom > 0) s2a / denom else 0,
       method = "ANOVA variance components (Searle et al. 1992, Eq. 21)")
}

#' .reml_loglik
#'
#' A step of the ranova_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param gs A vector; its length is taken and its elements indexed.
#' @param ns A vector; indexed elementwise.
#' @param s2a Numeric; combined arithmetically in the body.
#' @param s2e Numeric; passed to \code{log}.
#' @return A list with \code{ll}, \code{mu}.
#' @export
.reml_loglik <- function(gs, ns, s2a, s2e) {
  logdetV <- 0; xvx <- 0; xvy <- 0; yvy <- 0
  for (i in seq_along(gs)) {
    g <- gs[[i]]; n <- ns[i]
    d <- s2e + n * s2a
    logdetV <- logdetV + (n - 1) * log(s2e) + log(d)
    s <- sum(g); ss <- sum(g * g)
    xvx <- xvx + n / d
    xvy <- xvy + s / d
    yvy <- yvy + ss / s2e - (s2a / (s2e * d)) * s * s
  }
  mu <- xvy / xvx
  ypy <- yvy - xvy * xvy / xvx
  list(ll = -0.5 * (logdetV + log(xvx) + ypy), mu = mu)
}

#' REML estimation of variance components (one-way random model)
#'
#' Maximises the restricted log-likelihood
#' -2 l_R = log|V| + log|X'V^-1 X| + y'Py with X = 1 (Searle et al.
#' Sec. 6.6).  For BALANCED data their Sec. 4.8 proves the REML
#' solutions equal the ANOVA estimators, so the closed form is used
#' there -- exact, where a numerical maximiser cannot resolve the
#' argmax because l_R is flat to within double precision.  For
#' unbalanced data l_R is maximised numerically over the log
#' variances.
#'
#' On unbalanced data the two language arms agree on l_R to
#' ~6e-14 but on the argmax only to ~3e-7, because l_R is very flat
#' near its maximum; the objective is the verified quantity.
#'
#' @param y Observations.
#' @param group Class label per observation.
#' @param tol Convergence tolerance.
#' @param max_iter Maximum optimiser iterations.
#' @param solver Which route to the REML solution: "closed" uses
#'   Searle et al. Sec. 4.8 (on balanced data the REML solutions ARE
#'   the ANOVA estimators) and errors on unbalanced data; "optim"
#'   always maximises l_R numerically, valid for any design; "auto"
#'   (default) uses the closed form where it applies and the
#'   optimiser otherwise. Both routes are kept so either can be
#'   chosen or compared.
#' @return A list with elements \code{sigma2_a}, \code{sigma2_e},
#'   \code{mu}, \code{loglik}, \code{n_iter}, \code{converged},
#'   \code{icc}, \code{a}, \code{N}, \code{closed_form},
#'   \code{method}.
#' @references Searle, S. R., Casella, G. and McCulloch, C. E.
#'   (1992). Variance Components. Wiley, Sections 4.8 and 6.6.
#'   Patterson, H. D. and Thompson, R. (1971). Biometrika, 58,
#'   545-554.
#' @export
