#' Applied methods: FTC integral, Fredholm inverse problem, scale-test ARE,
#' custody credit, posterior variances, utility shocks, PMM, TD3 and CV2
#'
#' `definite_integral` is `F(b) - F(a)` with `F` from `SymbolicIntegrate`
#' (a composite Simpson value is reported as a check and used when no
#' antiderivative is found); `horowitz_fredholm_eq` the Tikhonov solution
#' `(A'A + alpha I)^-1 A'm` of a discretised Fredholm equation of the first
#' kind; `gibbons_are_scale_tests` the ARE of Mood's (15/(2 pi^2)) and
#' Klotz's (1) scale tests against F at the normal; `custody_days_credit`
#' the s. 719(3.1) enhanced credit (ratio in 1 to 1.5, R v Summers);
#' `irt_variance_legislator` posterior variances of MCMC draws by column;
#' `simulate_utility_shocks` Gumbel or normal random-utility errors from
#' Philox uniforms; `mi_pmm` predictive mean matching; `td3` a front-end to
#' `morie_geron_td3`; `cv2_genomic` CV2 cross-validation of RR-BLUP with
#' environment effects.
#'
#' @param expr Elementary expression in `x`.
#' @param x Variable name (or input matrix / data for the other functions).
#' @param a,b Integration limits.
#' @param n_check Simpson panels.
#' @param m,k,alpha,weights Fredholm right-hand side, kernel matrix,
#'   Tikhonov parameter and quadrature weights.
#' @param distribution Only "normal".
#' @param pretrial_days,credit_ratio Days in pre-sentence custody and ratio.
#' @param chain_theta Draws by legislators.
#' @param n,sigma,dist,seed Number of draws, scale, "gumbel" or "normal",
#'   Philox seed.
#' @param y,X,R,K Outcome, covariates, response indicator, donor pool size.
#' @param env,actor,critic1,critic2,... Environment, initial policy and
#'   critics, further arguments of `morie_geron_td3`.
#' @param markers,n_folds,lam Marker rows, number of folds, ridge penalty.
#' @return Lists with the components of the Python arm.
#' @references Apostol, T. M. (1967). Calculus, vol. 1. Wiley. Tikhonov, A.
#'   N. (1963). Solution of incorrectly formulated problems and the
#'   regularization method. Soviet Mathematics Doklady 4, 1035-1038. Gibbons,
#'   J. D. and Chakraborti, S. (2021). Nonparametric Statistical Inference,
#'   6th ed. CRC Press. R. v. Summers, 2014 SCC 26. Clinton, J., Jackman, S.
#'   and Rivers, D. (2004). The statistical analysis of roll call data. APSR
#'   98, 355-370. Train, K. E. (2009). Discrete Choice Methods with
#'   Simulation. Cambridge University Press. Little, R. J. A. (1988).
#'   Missing-data adjustments in large surveys. JBES 6, 287-296. Fujimoto,
#'   S., van Hoof, H. and Meger, D. (2018). Addressing function approximation
#'   error in actor-critic methods. ICML. Burgueno, J. et al. (2012). Genomic
#'   prediction of breeding values when modeling genotype x environment
#'   interaction. Crop Science 52, 707-719.
#' @examples
#' definite_integral("x*exp(2*x)", "x", 0, 1)$estimate
#' custody_days_credit(c(10, 30, 45))$credited
#' @export
definite_integral <- function(expr, x = "x", a = 0, b = 1, n_check = 2000) {
  e <- .si_parse(expr)
  m <- n_check + n_check %% 2
  h <- (b - a) / m
  env <- function(v) stats::setNames(list(v), x)
  vals <- vapply(0:m, function(i) .si_ev(e, env(a + i * h)), 0)
  simpson <- h / 3 * (vals[1] + vals[m + 1] + 4 * sum(vals[seq(2, m, by = 2)]) + 2 * sum(vals[seq(3, m - 1, by = 2)]))
  F <- SymbolicIntegrate(expr, x)$antiderivative
  if (is.null(F)) return(list(estimate = simpson, antiderivative = NULL, numeric = simpson, method = "Simpson"))
  Fe <- .si_parse(F)
  list(estimate = .si_ev(Fe, env(b)) - .si_ev(Fe, env(a)), antiderivative = F, numeric = simpson, method = "FTC")
}

#' @rdname definite_integral
#' @export
horowitz_fredholm_eq <- function(m, k, alpha = 1e-3, weights = NULL) {
  K <- unname(as.matrix(k)) * 1
  if (length(m) != nrow(K)) stop("m must have one value per row of k")
  if (!(alpha > 0)) stop("alpha must be positive")
  d <- if (is.null(weights)) rep(1, ncol(K)) else as.numeric(weights)
  A <- sweep(K, 2, d, "*")
  g <- as.vector(solve(crossprod(A) + alpha * diag(ncol(A)), crossprod(A, m)))
  list(g_hat = g, residual_norm = sqrt(sum((A %*% g - m)^2)), alpha = alpha,
       method = "Tikhonov-regularised Fredholm equation of the first kind")
}

#' @rdname definite_integral
#' @export
gibbons_are_scale_tests <- function(distribution = "normal") {
  if (distribution != "normal") stop("scale-test AREs are tabulated here for the normal only")
  list(are_mood_f = 15 / (2 * pi^2), are_klotz_f = 1, distribution = "normal",
       method = "ARE(Mood, F) = 15/(2 pi^2); ARE(Klotz, F) = 1 (Sec. 13.3.3)")
}

#' @rdname definite_integral
#' @export
custody_days_credit <- function(pretrial_days, credit_ratio = 1.5) {
  if (credit_ratio < 1 || credit_ratio > 1.5) stop("credit_ratio must lie in [1, 1.5] (Criminal Code s. 719(3.1))")
  d <- as.numeric(pretrial_days)
  if (any(d < 0)) stop("pre-trial days must be non-negative")
  cr <- d * credit_ratio
  list(measure = "custody_days_credit", estimate = mean(cr), n = length(d), credited = cr, total_pretrial = sum(d),
       total_credited = sum(cr), credit_ratio = credit_ratio)
}

#' @rdname definite_integral
#' @export
irt_variance_legislator <- function(chain_theta) {
  C <- as.matrix(chain_theta)
  if (nrow(C) < 2) stop("at least two draws are needed")
  v <- apply(C, 2, stats::var)
  list(value = unname(v), variances = unname(v), sd = unname(sqrt(v)), mean_variance = mean(v),
       n_legislators = ncol(C), n_samples = nrow(C))
}

#' @rdname definite_integral
#' @export
simulate_utility_shocks <- function(n = 100, sigma = 1, dist = "gumbel", seed = 0) {
  u <- .morie_random_uniform(n, seed = seed, stream = 0)
  e <- switch(dist, gumbel = -sigma * log(-log(u)), normal = sigma * stats::qnorm(u),
              stop("dist must be 'gumbel' or 'normal'"))
  list(value = e, shocks = e, n = n, sigma = sigma, dist = dist, mean = mean(e), std = sqrt(mean((e - mean(e))^2)))
}

#' @rdname definite_integral
#' @export
mi_pmm <- function(y, X, R, K = 5, seed = 0) {
  y <- as.numeric(y)
  D <- cbind(1, as.matrix(X))
  obs <- which(R == 1)
  mis <- which(R == 0)
  if (K < 1 || K > length(obs)) stop("K must lie between 1 and the number of observed cases")
  beta <- as.vector(solve(crossprod(D[obs, , drop = FALSE]), crossprod(D[obs, , drop = FALSE], y[obs])))
  yhat <- as.vector(D %*% beta)
  u <- .morie_random_uniform(max(length(mis), 1), seed = seed, stream = 0)
  out <- y
  donors <- integer(0)
  for (t in seq_along(mis)) {
    i <- mis[t]
    pool <- obs[order(abs(yhat[i] - yhat[obs]), obs)][seq_len(K)]
    d <- pool[min(K, floor(u[t] * K) + 1)]
    donors <- c(donors, d - 1L)
    out[i] <- y[d]
  }
  list(imputed = out, donors = donors, missing = mis - 1L, coefficients = beta, method = "predictive mean matching (type 0)")
}

#' @rdname definite_integral
#' @export
td3 <- function(env, actor = NULL, critic1 = NULL, critic2 = NULL, ...) {
  morie_geron_td3(env, policy = actor, Q1 = critic1, Q2 = critic2, ...)
}

#' @rdname definite_integral
#' @export
cv2_genomic <- function(y, markers, env, n_folds = 5, lam = 1, seed = 0) {
  y <- as.numeric(y)
  M <- unname(as.matrix(markers)) * 1
  n <- length(y)
  envs <- sort(unique(as.character(env)), method = "radix")
  E <- if (length(envs) > 1) outer(as.character(env), envs[-1], "==") * 1 else matrix(0, n, 0)
  D <- cbind(1, E, M)
  pen <- c(rep(0, 1 + ncol(E)), rep(lam, ncol(M)))
  u <- .morie_random_uniform(n, seed = seed, stream = 0)
  perm <- seq_len(n)
  for (i in (n - 1):1) {
    j <- floor(u[i + 1] * (i + 1))
    tmp <- perm[i + 1]
    perm[i + 1] <- perm[j + 1]
    perm[j + 1] <- tmp
  }
  fold <- integer(n)
  fold[perm] <- (seq_len(n) - 1) %% n_folds
  pas <- vapply(seq_len(n_folds) - 1, function(f) {
    tr <- fold != f
    te <- fold == f
    beta <- solve(crossprod(D[tr, , drop = FALSE]) + diag(pen), crossprod(D[tr, , drop = FALSE], y[tr]))
    stats::cor(y[te], as.vector(D[te, , drop = FALSE] %*% beta))
  }, 0)
  list(pa = mean(pas), pa_folds = pas, folds = fold, n = n,
       method = "CV2 (records held out), RR-BLUP with environment effects")
}
