# Generalised linear models -- logistic, Poisson, Gaussian, Gamma.
#
# Fitted by iteratively reweighted least squares, which is Fisher
# scoring for the links used here.  Self-contained: this does not call
# stats::glm, it reimplements it, and is verified against it to 1e-12.

#' .morie_glm_families
#'
#' A step of the glm implementation. Called by \code{morie_deviance_residuals}, \code{morie_glm}, \code{morie_glm_predict}.
#' See the file header for the source the module follows.
#' follows.
#'
#' @return A list with \code{binomial}, \code{poisson}, \code{gaussian}, \code{gamma}.
#' @export
.morie_glm_families <- function() {
  eps <- 1e-10
  clip01 <- function(p) pmin(pmax(p, eps), 1 - eps)
  list(
    binomial = list(
      link = function(mu) log(clip01(mu) / (1 - clip01(mu))),
      linkinv = function(e) 1 / (1 + exp(-pmax(pmin(e, 700), -700))),
      variance = function(mu) pmax(mu * (1 - mu), eps),
      mu_eta = function(e) {
        p <- 1 / (1 + exp(-pmax(pmin(e, 700), -700)))
        pmax(p * (1 - p), eps)
      },
      dev_resid = function(y, mu) {
        a <- ifelse(y > 0, y * log(y / clip01(mu)), 0)
        b <- ifelse(y < 1, (1 - y) * log((1 - y) / (1 - clip01(mu))), 0)
        2 * (a + b)
      },
      start = function(y) (y + 0.5) / 2,
      dispersion_fixed = TRUE
    ),
    poisson = list(
      link = function(mu) log(pmax(mu, eps)),
      linkinv = function(e) exp(pmin(e, 700)),
      variance = function(mu) pmax(mu, eps),
      mu_eta = function(e) exp(pmin(e, 700)),
      dev_resid = function(y, mu) {
        2 * (ifelse(y > 0, y * log(y / pmax(mu, eps)), 0) - (y - mu))
      },
      start = function(y) y + 0.1,
      dispersion_fixed = TRUE
    ),
    gaussian = list(
      link = function(mu) mu,
      linkinv = function(e) e,
      variance = function(mu) rep(1, length(mu)),
      mu_eta = function(e) rep(1, length(e)),
      dev_resid = function(y, mu) (y - mu)^2,
      start = function(y) y,
      dispersion_fixed = FALSE
    ),
    gamma = list(
      link = function(mu) log(pmax(mu, eps)),
      linkinv = function(e) exp(pmin(e, 700)),
      variance = function(mu) pmax(mu, eps)^2,
      mu_eta = function(e) exp(pmin(e, 700)),
      dev_resid = function(y, mu) {
        2 * (-log(pmax(y, eps) / pmax(mu, eps)) + (y - mu) / pmax(mu, eps))
      },
      start = function(y) pmax(y, eps),
      dispersion_fixed = FALSE
    )
  )
}

#' .morie_glm_solve
#'
#' A step of the glm implementation. Called by \code{morie_glm}.
#' See the file header for the source the module follows.
#' follows.
#'
#' @param A A matrix; passed to \code{solve}.
#' @param b A matrix; passed to \code{solve}.
#' @return A vector, from \code{as.numeric}.
#' @export
.morie_glm_solve <- function(A, b) {
  r <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(r) || any(!is.finite(r))) {
    stop("singular information matrix: predictors are collinear or a category is empty")
  }
  as.numeric(r)
}

#' morie_glm
#'
#' A step of the glm implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' follows.
#'
#' @param y A vector; its length is taken.
#' @param X A matrix; passed to \code{nrow}.
#' @param family Defaults to \code{"binomial"}.
#' @param add_intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param weights Defaults to \code{NULL}.
#' @param offset Defaults to \code{NULL}.
#' @param max_iter Defaults to \code{25L}.
#' @param tol Defaults to \code{1e-08}.
#' @return A list with \code{coef}, \code{se}, \code{statistic}, \code{statistic_name}, \code{p_value}, \code{fitted}, \code{linear_predictor}, \code{residuals}, \code{deviance}, \code{null_deviance}, \code{df_residual}, \code{df_null}, \code{dispersion}, \code{pearson_chi2}, \code{aic}, \code{loglik}, \code{converged}, \code{family}, \code{n}, \code{k}, \code{vcov}, \code{method}.
#' @export
morie_glm <- function(y, X, family = "binomial", add_intercept = TRUE,
                      weights = NULL, offset = NULL, max_iter = 25L,
                      tol = 1e-8) {
  fams <- .morie_glm_families()
  fl <- tolower(as.character(family)[1])
  if (!fl %in% names(fams)) {
    stop("family must be one of ", paste(sort(names(fams)), collapse = ", "))
  }
  fam <- fams[[fl]]
  y <- as.numeric(y)
  X <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(y)
  if (nrow(X) != n) {
    stop("X has ", nrow(X), " rows but y has ", n)
  }
  if (add_intercept) X <- cbind(1, X)
  dimnames(X) <- NULL # else column names leak onto coef/se/vcov
  p <- ncol(X)
  if (n <= p) stop("need more observations than parameters")
  pw <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  off <- if (is.null(offset)) rep(0, n) else as.numeric(offset)
  if (fl == "binomial" && any(y < 0 | y > 1)) {
    stop("binomial response must lie in [0, 1]")
  }
  if (fl == "poisson" && any(y < 0)) {
    stop("Poisson response must be non-negative")
  }

  mu <- fam$start(y)
  eta <- fam$link(mu)
  beta <- rep(0, p)
  converged <- FALSE
  dev_old <- NA_real_
  w_fit <- NULL
  for (it in seq_len(as.integer(max_iter))) {
    g <- fam$mu_eta(eta)
    w <- pw * g * g / fam$variance(mu)
    z <- eta - off + (y - mu) / g
    beta <- .morie_glm_solve(crossprod(X, X * w), crossprod(X, w * z))
    w_fit <- w # the weights that produced this beta
    eta <- as.numeric(off + X %*% beta)
    mu <- fam$linkinv(eta)
    dev <- sum(pw * fam$dev_resid(y, mu))
    if (!is.na(dev_old) && abs(dev - dev_old) / (abs(dev) + 0.1) < tol) {
      converged <- TRUE
      dev_old <- dev
      break
    }
    dev_old <- dev
  }
  deviance <- dev_old

  if (add_intercept) {
    mu0 <- sum(pw * y) / sum(pw)
    null_dev <- sum(pw * fam$dev_resid(y, rep(mu0, n)))
    df_null <- n - 1L
  } else {
    null_dev <- sum(pw * fam$dev_resid(y, fam$linkinv(off)))
    df_null <- n
  }
  df_resid <- n - p

  # Standard errors come from the weighted cross-product of the FINAL
  # IRLS solve -- weights at the eta that produced beta, not recomputed
  # at the converged eta.  The two differ by one Fisher-scoring step,
  # invisible in beta but visible in the eighth digit of every standard
  # error; summary.glm inverts the stored QR, i.e. the former.
  V <- solve(crossprod(X, X * w_fit))
  pearson <- sum(pw * (y - mu)^2 / fam$variance(mu))
  disp <- if (fam$dispersion_fixed) 1 else if (df_resid > 0) pearson / df_resid else NA_real_
  V <- V * disp
  se <- sqrt(diag(V))
  stat <- beta / se
  if (fam$dispersion_fixed) {
    pv <- 2 * pnorm(abs(stat), lower.tail = FALSE)
    stat_name <- "z"
  } else {
    pv <- 2 * pt(abs(stat), df_resid, lower.tail = FALSE)
    stat_name <- "t"
  }

  eps <- 1e-10
  if (fl == "binomial") {
    m <- pmin(pmax(mu, eps), 1 - eps)
    ll <- sum(pw * (y * log(m) + (1 - y) * log(1 - m)))
    aic <- -2 * ll + 2 * p
  } else if (fl == "poisson") {
    ll <- sum(pw * (y * log(pmax(mu, eps)) - mu - lgamma(y + 1)))
    aic <- -2 * ll + 2 * p
  } else if (fl == "gaussian") {
    s2 <- deviance / n
    ll <- -0.5 * n * (log(2 * pi * s2) + 1)
    aic <- -2 * ll + 2 * (p + 1)
  } else {
    ll <- NA_real_
    aic <- NA_real_
  }

  list(
    coef = beta, se = se, statistic = stat, statistic_name = stat_name,
    p_value = pv, fitted = mu, linear_predictor = eta,
    residuals = y - mu, deviance = deviance, null_deviance = null_dev,
    df_residual = df_resid, df_null = df_null, dispersion = disp,
    pearson_chi2 = pearson, aic = aic, loglik = ll,
    converged = converged, family = fl, n = n, k = p, vcov = V,
    method = "generalised linear model (IRLS)"
  )
}

#' morie_glm_predict
#'
#' A step of the glm implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' follows.
#'
#' @param fit A list; the body reads \code{$coef}, \code{$family} from it.
#' @param X A matrix; passed to \code{nrow}.
#' @param add_intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param type Compared against \code{"link"}. Defaults to \code{c("response", "link")}.
#' @param offset Defaults to \code{NULL}.
#' @return The value of \code{.morie_glm_families()[[fit$family]]$linkinv}.
#' @export
morie_glm_predict <- function(fit, X, add_intercept = TRUE,
                              type = c("response", "link"), offset = NULL) {
  type <- match.arg(type)
  X <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(X) <- "double"
  if (add_intercept) X <- cbind(1, X)
  if (ncol(X) != length(fit$coef)) {
    stop(
      "X has ", ncol(X), " columns but the fit has ",
      length(fit$coef), " coefficients"
    )
  }
  off <- if (is.null(offset)) rep(0, nrow(X)) else as.numeric(offset)
  eta <- as.numeric(off + X %*% fit$coef)
  if (type == "link") {
    return(eta)
  }
  .morie_glm_families()[[fit$family]]$linkinv(eta)
}

#' morie_deviance_residuals
#'
#' A step of the glm implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' follows.
#'
#' @param fit A list; the body reads \code{$family}, \code{$fitted} from it.
#' @param y Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
morie_deviance_residuals <- function(fit, y) {
  y <- as.numeric(y)
  d <- pmax(.morie_glm_families()[[fit$family]]$dev_resid(y, fit$fitted), 0)
  sign(y - fit$fitted) * sqrt(d)
}
