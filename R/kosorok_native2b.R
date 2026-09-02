# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kosorok shelf, part 2b: Z- and M-estimator theory (Ch. 2) and
# semiparametric efficiency (Ch. 3). Mirrors the thirteen ksr*
# modules added alongside.
#
# Collision scan: kosorok_native2b.R and all thirteen exported names
# were free in both R trees.
#
# Spec: Kosorok, M. R., Introduction to Empirical Processes and
# Semiparametric Inference, Springer. Equation and page numbers are
# the book's; Eq. (2.11) was checked verbatim against the text.

#' Empirical distribution of regression residuals
#'
#' \eqn{\hat F(t) = n^{-1}\sum 1\{Y_i - \hat\beta'Z_i \le t\}}
#' (Eq. 1.2, p. 4). The estimated parameter INSIDE the indicator is
#' the point: if beta were known this would be an ordinary empirical
#' df and Donsker's theorem would apply directly. Plugging in
#' beta-hat adds a term to the limit, so \eqn{\sqrt n(\hat F - F)} is
#' NOT the standard Brownian bridge and treating it as one gives
#' wrong standard errors. Mirrors \code{morie.fn.ksr021}.
#'
#' @param y numeric responses.
#' @param z covariates, vector or matrix.
#' @param beta estimated coefficients.
#' @param t evaluation points; the sorted residuals when NULL.
#' @return list: t, F_hat, residuals, limit_is_brownian_bridge,
#'   correction_note, n, method.
#' @references Kosorok, Ch. 1, Eq. (1.2), p. 4.
#' @examples
#' z <- cbind(rnorm(50), rnorm(50))
#' morie_residual_edf(z %*% c(1, -0.5) + rnorm(50), z,
#'                    c(1, -0.5))$limit_is_brownian_bridge
#' @export
morie_residual_edf <- function(y, z, beta, t = NULL) {
  yv <- as.numeric(y)
  Z <- if (is.matrix(z)) z else matrix(as.numeric(z), ncol = 1L)
  if (nrow(Z) != length(yv)) Z <- t(Z)
  if (nrow(Z) != length(yv)) {
    stop("z must have one row per entry of y.", call. = FALSE)
  }
  b <- as.numeric(beta)
  if (length(b) != ncol(Z)) {
    stop(sprintf("beta has %d entries for %d columns.", length(b), ncol(Z)),
         call. = FALSE)
  }
  resid <- yv - as.numeric(Z %*% b)
  tv <- if (is.null(t)) sort(resid) else as.numeric(t)
  list(t = tv, F_hat = vapply(tv, function(v) mean(resid <= v), numeric(1)),
       residuals = resid, limit_is_brownian_bridge = FALSE,
       correction_note = paste("plugging in beta-hat adds a term to the limit of",
                               "sqrt(n)(F_hat - F); it is not the standard bridge"),
       n = length(yv),
       method = "Residual empirical df (Eq. 1.2); the estimated beta inside the indicator changes the limit")
}

#' Cox partial-likelihood score process
#'
#' \eqn{U_n(t,\beta) = n^{-1}\sum_i \int_0^t \[Z_i - E_n(s,\beta)\]
#' dN_i(s)} with \eqn{E_n} the risk-set weighted average covariate
#' (Eq. 1.4, p. 5). Indexed by t, so it is a stochastic PROCESS whose
#' weak convergence -- not merely asymptotic normality of the
#' endpoint -- licenses Cox inference. Its root defines the
#' estimator, which makes this a Z-estimation problem. Mirrors
#' \code{morie.fn.ksr023}.
#'
#' @param beta coefficients.
#' @param z covariates.
#' @param time follow-up times.
#' @param event binary 0/1 event indicators.
#' @param t_grid times to report at; the event times when NULL.
#' @return list: t_grid, U, U_final, E_bar, is_process,
#'   root_defines_estimator, n_events, n, method.
#' @references Kosorok, Ch. 1, Eq. (1.4), p. 5.
#' @examples
#' z <- rnorm(60)
#' tt <- rexp(60) / exp(0.7 * z)
#' morie_cox_score_process(0.7, z, tt, rep(1, 60))$is_process
#' @export
morie_cox_score_process <- function(beta, z, time, event, t_grid = NULL) {
  b <- as.numeric(beta)
  tv <- as.numeric(time)
  ev <- as.numeric(event)
  Z <- if (is.matrix(z)) z else matrix(as.numeric(z), ncol = 1L)
  if (nrow(Z) != length(tv)) Z <- t(Z)
  if (nrow(Z) != length(tv)) {
    stop("z must have one row per follow-up time.", call. = FALSE)
  }
  if (length(ev) != length(tv)) {
    stop(sprintf("event has %d entries for %d times.", length(ev), length(tv)),
         call. = FALSE)
  }
  if (!all(ev %in% c(0, 1))) stop("event must be binary 0/1.", call. = FALSE)
  if (length(b) != ncol(Z)) {
    stop(sprintf("beta has %d entries for %d columns.", length(b), ncol(Z)),
         call. = FALSE)
  }
  n <- nrow(Z)
  p <- ncol(Z)
  w <- exp(as.numeric(Z %*% b))
  et <- sort(tv[ev == 1])
  if (length(et) == 0L) {
    stop("no events: the score process is identically zero.", call. = FALSE)
  }
  tg <- if (is.null(t_grid)) et else as.numeric(t_grid)
  contrib <- matrix(0, length(et), p)
  ebar <- matrix(0, length(et), p)
  for (k in seq_along(et)) {
    s <- et[k]
    at <- tv >= s
    sw <- sum(w[at])
    if (sw <= 0) next
    e <- colSums(w[at] * Z[at, , drop = FALSE]) / sw
    ebar[k, ] <- e
    for (i in which(tv == s & ev == 1)) contrib[k, ] <- contrib[k, ] + Z[i, ] - e
  }
  cum <- apply(contrib, 2L, cumsum) / n
  if (is.null(dim(cum))) cum <- matrix(cum, ncol = p)
  U <- t(vapply(tg, function(v) {
    if (v < et[1L]) rep(0, p) else cum[max(findInterval(v, et), 1L), ]
  }, numeric(p)))
  if (p == 1L) U <- matrix(U, ncol = 1L)
  list(t_grid = tg, U = U, U_final = cum[nrow(cum), ], E_bar = ebar,
       is_process = TRUE, root_defines_estimator = TRUE,
       n_events = sum(ev), n = n,
       method = "Cox score process (Eq. 1.4); indexed by t, so its weak convergence is what matters")
}

#' Kaplan-Meier Z-estimator map
#'
#' \eqn{\Psi(S)(t) = S_0(t)L(t) + \int_0^t \[S_0(u)/S(u)\]dG(u)S(t) -
#' S(t)} (Eq. 2.11, p. 26), implemented exactly as printed.
#'
#' \eqn{S_0}, \eqn{L} and \eqn{G} are SUPPLIED rather than inferred.
#' The section fixes them for its own censoring model, and the
#' passage stating (2.11) does not define them unambiguously enough
#' to reconstruct; plausible empirical stand-ins were tried and do
#' NOT make Kaplan-Meier a root, so guessing would have produced a
#' module that looked right and was wrong.
#'
#' Survival analysis becomes Z-ESTIMATION: the estimator is
#' characterised as the zero of a map between function spaces, and
#' Chapter 2's theory then supplies consistency, weak convergence and
#' the bootstrap at once. The parameter is a FUNCTION and the norm is
#' uniform. Mirrors \code{morie.fn.ksr047}.
#'
#' @param S candidate survival function on t_grid.
#' @param t_grid the grid everything is supplied on.
#' @param S0,L,G the section's components, on t_grid.
#' @return list: t_grid, psi, sup_norm, parameter_is, norm,
#'   components_supplied, n, method.
#' @references Kosorok, Ch. 2, Eq. (2.11), p. 26.
#' @examples
#' g <- seq(0.1, 2, length.out = 20)
#' morie_survival_psi(exp(-g), g, exp(-g), exp(-0.5 * g),
#'                    1 - exp(-0.3 * g))$sup_norm
#' @export
morie_survival_psi <- function(S, t_grid, S0, L, G) {
  tg <- as.numeric(t_grid)
  if (length(tg) < 2L) {
    stop(sprintf("need at least 2 grid points, got %d.", length(tg)),
         call. = FALSE)
  }
  parts <- list(S = as.numeric(S), S0 = as.numeric(S0),
                L = as.numeric(L), G = as.numeric(G))
  for (nm in names(parts)) {
    if (length(parts[[nm]]) != length(tg)) {
      stop(sprintf("%s has %d entries for %d grid points.",
                   nm, length(parts[[nm]]), length(tg)), call. = FALSE)
    }
  }
  dG <- diff(c(0, parts$G))
  safe <- ifelse(parts$S > 0, parts$S, Inf)
  psi <- parts$S0 * parts$L + cumsum(parts$S0 / safe * dG) * parts$S - parts$S
  list(t_grid = tg, psi = psi, sup_norm = max(abs(psi)),
       parameter_is = "a FUNCTION, so the norm is uniform", norm = "supremum",
       components_supplied = TRUE, n = length(tg),
       method = "Kaplan-Meier as a Z-estimator (Eq. 2.11); its root is the estimator")
}

#' M-estimator asymptotic normality
#'
#' \eqn{\sqrt n(\hat\theta_n - \theta_0) \rightsquigarrow -V^{-1}Z}
#' (Thm. 2.13, p. 29). The limit is a SANDWICH
#' \eqn{V^{-1}\Sigma V^{-1}}: V is the curvature of the criterion and
#' \eqn{\Sigma = P\dot m\dot m'} the variability of its gradient.
#' They coincide only for a correctly specified log-likelihood -- the
#' information equality -- and assuming they do elsewhere is the
#' standard way to get standard errors wrong. Mirrors
#' \code{morie.fn.ksr057}.
#'
#' @param m_dot_scores n by p gradient at theta_0.
#' @param V curvature matrix; Sigma is used when NULL, which ASSUMES
#'   the information equality.
#' @return list: Sigma, V, avar, se, information_equality_assumed,
#'   information_equality_holds, n, p, method.
#' @references Kosorok, Thm. 2.13, p. 29.
#' @examples
#' morie_m_normality(matrix(rnorm(200), ncol = 2))$information_equality_assumed
#' @export
morie_m_normality <- function(m_dot_scores, V = NULL) {
  S <- as.matrix(m_dot_scores)
  n <- nrow(S)
  p <- ncol(S)
  if (n < 2L) stop(sprintf("need at least 2 observations, got %d.", n),
                   call. = FALSE)
  Sigma <- crossprod(S) / n
  assumed <- is.null(V)
  Vm <- if (assumed) Sigma else as.matrix(V)
  if (!identical(dim(Vm), c(p, p))) {
    stop(sprintf("V must be %d by %d.", p, p), call. = FALSE)
  }
  Vi <- solve(Vm)
  avar <- Vi %*% Sigma %*% Vi
  list(Sigma = Sigma, V = Vm, avar = avar,
       se = sqrt(pmax(diag(avar), 0) / n),
       information_equality_assumed = assumed,
       information_equality_holds = isTRUE(all.equal(Vm, Sigma,
                                                     tolerance = 1e-6)),
       n = n, p = p,
       method = "M-estimator normality (Thm. 2.13); the limit is a SANDWICH, not V^{-1} alone")
}

#' Semiparametric efficiency
#'
#' \eqn{\sqrt n(\theta_n - \theta) \rightsquigarrow -\tilde
#' I^{-1}Z} with \eqn{\tilde I = P\tilde\ell\tilde\ell'} the
#' EFFICIENT information (Thm. 3.1, p. 44). The efficient score is
#' the ordinary score with its projection onto the nuisance tangent
#' space removed, and that projection is the entire cost of not
#' knowing the nuisance: efficient information is never larger than
#' full information, with equality only in the adaptive case.
#' Mirrors \code{morie.fn.ksr072}.
#'
#' @param scores n by p scores for the parameter of interest.
#' @param nuisance_scores n by q scores spanning the nuisance tangent
#'   space; without them the problem is parametric.
#' @return list: efficient_information, full_information,
#'   efficient_scores, avar, se, information_loss, adaptive, n, p,
#'   method.
#' @references Kosorok, Thm. 3.1, p. 44 and Ch. 3.
#' @examples
#' morie_semipar_efficiency(matrix(rnorm(100), ncol = 1),
#'                          matrix(rnorm(200), ncol = 2))$adaptive
#' @export
morie_semipar_efficiency <- function(scores, nuisance_scores = NULL) {
  S <- as.matrix(scores)
  if (nrow(S) < ncol(S)) S <- t(S)
  n <- nrow(S)
  p <- ncol(S)
  if (n < 2L) stop(sprintf("need at least 2 observations, got %d.", n),
                   call. = FALSE)
  full <- crossprod(S) / n
  if (is.null(nuisance_scores)) {
    eff <- S
  } else {
    B <- as.matrix(nuisance_scores)
    if (nrow(B) != n) B <- t(B)
    cf <- qr.coef(qr(B), S)
    cf[is.na(cf)] <- 0
    eff <- S - B %*% cf
  }
  eff_info <- crossprod(eff) / n
  avar <- solve(eff_info)
  loss <- sum(diag(full)) - sum(diag(eff_info))
  list(efficient_information = eff_info, full_information = full,
       efficient_scores = eff, avar = avar,
       se = sqrt(pmax(diag(avar), 0) / n), information_loss = loss,
       adaptive = abs(loss) < 1e-9,
       ordering = "efficient information <= full information, always",
       n = n, p = p,
       method = "Semiparametric efficiency (Thm. 3.1); the projection IS the cost of the nuisance")
}

#' Joint convergence of parameter and nuisance
#'
#' \eqn{\sqrt n(\hat\theta_n - \theta_0, \hat\eta_n - \eta_0)
#' \rightsquigarrow -\dot\Psi_0^{-1}Z} (Cor. 3.2, p. 47), under the
#' no-bias condition and stochastic equicontinuity.
#'
#' JOINTLY, not separately. The two estimates solve the same
#' estimating equation and are correlated, so a valid region for a
#' function of both needs the joint law; combining marginal limits as
#' if independent understates the variability. One operator inverse
#' gives both blocks, which is why their dependence is determined
#' rather than assumed. Mirrors \code{morie.fn.ksr073}.
#'
#' @param psi_dot the derivative operator, square and invertible.
#' @param scores n by d stacked influence contributions.
#' @param n sample size; from scores when NULL.
#' @return list: avar, se, correlation, jointly, operator_invertible,
#'   conditions, warning, n, d, method.
#' @references Kosorok, Cor. 3.2, p. 47.
#' @examples
#' morie_joint_convergence(diag(2), matrix(rnorm(200), ncol = 2))$jointly
#' @export
morie_joint_convergence <- function(psi_dot, scores, n = NULL) {
  S <- as.matrix(scores)
  if (nrow(S) < ncol(S)) S <- t(S)
  nn <- if (is.null(n)) nrow(S) else as.integer(n)
  if (nn < 2L) stop(sprintf("n must be at least 2, got %s.", nn),
                    call. = FALSE)
  d <- ncol(S)
  D <- as.matrix(psi_dot)
  if (!identical(dim(D), c(d, d))) {
    stop(sprintf("psi_dot must be %d by %d.", d, d), call. = FALSE)
  }
  ok <- qr(D)$rank == d
  Sigma <- crossprod(S) / nrow(S)
  Di <- solve(D)
  avar <- Di %*% Sigma %*% t(Di)
  sd <- sqrt(pmax(diag(avar), 0))
  corr <- avar / outer(sd, sd)
  corr[!is.finite(corr)] <- 0
  list(avar = avar, se = sd / sqrt(nn), correlation = corr, jointly = TRUE,
       operator_invertible = ok,
       conditions = "the no-bias condition (3.6) and stochastic equicontinuity",
       warning = paste("theta-hat and eta-hat solve the SAME equation and are",
                       "correlated; combining marginal limits as if independent",
                       "understates variability"),
       n = nn, d = d,
       method = "Joint convergence (Cor. 3.2); one operator inverse gives both blocks and their dependence")
}
