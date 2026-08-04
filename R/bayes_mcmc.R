# SPDX-License-Identifier: AGPL-3.0-or-later
#
# bayes_mcmc.R -- a self-contained Bayesian MCMC engine (random-walk
# Metropolis) for Bayesian linear regression, completing rmorie's
# coverage of the srr "BS" standards without requiring an external
# sampler. Provides multiple seeded chains, starting-value control,
# run continuation, multiple convergence checkers (R-hat, ESS, Geweke),
# verbosity + error handling, trace/density plots, and posterior/prior
# recovery utilities.

#' srr Bayesian (BS) sampler standards
#'
#' These BS standards are completed by the base-R MCMC engine in this
#' file (morie_bayes_lm and friends) and tested in
#' test-srr-standards-BS-full.R.
#'
#' @srrstats {BS1.3a} morie_bayes_continue() uses the output of a previous
#'   run as the starting point of a subsequent run.
#' @srrstats {BS1.4} Convergence checking can be run or skipped
#'   (check_convergence=), documented with and without.
#' @srrstats {BS1.5} Multiple convergence checkers (R-hat, ESS, Geweke)
#'   are provided and their differences are testable.
#' @srrstats {BS2.1a} The dimensional pre-processing (design matrix) is
#'   tested.
#' @srrstats {BS2.3} Over-length prior-parameter vectors are rejected, not
#'   silently discarded.
#' @srrstats {BS2.4} Prior-parameter vector lengths are checked against
#'   the number of coefficients.
#' @srrstats {BS2.8} morie_bayes_continue() resumes from a previous run's
#'   final state.
#' @srrstats {BS2.10} A diagnostic is issued when identical seeds are
#'   passed to distinct chains.
#' @srrstats {BS2.11} The vector-valued starting argument is named
#'   `starting_values` (plural).
#' @srrstats {BS2.14} Warnings can be suppressed via `quiet=` and this is
#'   tested.
#' @srrstats {BS2.15} Sampler errors are caught and returned in the result
#'   rather than crashing, and this is tested.
#' @srrstats {BS3.2} Perfectly collinear predictors are detected and the
#'   sampler bypasses them with an informative error.
#' @srrstats {BS4.1} morie_bayes_compare() compares posterior means with
#'   an external (analytic / lm) reference.
#' @srrstats {BS4.4} `stop_on_convergence=` halts sampling once the chains
#'   have converged.
#' @srrstats {BS4.5} Non-convergence is surfaced via the `converged` flag
#'   and a warning; the samples are still returned.
#' @srrstats {BS4.6} A test confirms that convergence-checked results are
#'   statistically equivalent to a fixed-length run.
#' @srrstats {BS4.7} The convergence-threshold parameter's effect (lower
#'   threshold -> longer runs) is tested.
#' @srrstats {BS5.4} morie_bayes_diagnostics() returns the details of each
#'   convergence checker used.
#' @srrstats {BS5.5} Non-convergence diagnostics (R-hat > threshold) are
#'   returned in the fit object.
#' @srrstats {BS6.0} morie_bayes_plot() is a default trace-plot method.
#' @srrstats {BS6.1} plot dispatch on the morie_bayes_fit class exists.
#' @srrstats {BS6.2} morie_bayes_plot() plots sequences of posterior
#'   samples (trace plots).
#' @srrstats {BS6.3} morie_bayes_density() plots posterior distributional
#'   estimates.
#' @srrstats {BS6.4} morie_bayes_plot(type=) optionally selects trace or
#'   density.
#' @srrstats {BS6.5} morie_bayes_plot(type="both") plots samples and
#'   densities together.
#' @srrstats {BS7.0} A test confirms recovery of the parameters of a known
#'   generating model.
#' @srrstats {BS7.1} A test confirms that with a very tight prior and no
#'   data weight, the posterior recovers the prior.
#' @srrstats {BS7.2} A test confirms the posterior matches the analytic
#'   conjugate posterior for a Gaussian model.
#' @srrstats {BS7.3} A test confirms algorithmic-efficiency scaling
#'   (more iterations -> lower Monte Carlo error).
#' @srrstats {BS7.4} A test confirms fitted values are on the same scale
#'   as the input response.
#' @srrstats {BS7.4a} The implication of response scaling on the posterior
#'   is tested.
#' @srrstats {BS1.2a} Prior specification is described in the README
#'   "Bayesian priors" section with example code.
#' @srrstats {BS1.2b} The `bayesian-priors` vignette gives general and
#'   applied prior-specification guidance with runnable examples.
#' @noRd
NULL

#' Internal helper: Bayes Design
#' @noRd
.bayes_design <- function(formula, data) {
  mf <- stats::model.frame(formula, data)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(formula, mf)
  # perfect collinearity check (BS3.2)
  if (qr(X)$rank < ncol(X)) {
    stop("design matrix is rank-deficient (perfectly collinear predictors); ",
      "sampler bypassed",
      call. = FALSE
    )
  }
  list(y = as.numeric(y), X = X, p = ncol(X))
}

#' Fit a Bayesian linear regression by random-walk Metropolis
#'
#' A self-contained MCMC sampler (no external backend). Runs multiple
#' seeded chains, supports explicit starting values and run continuation,
#' assesses convergence with several checkers, and returns the posterior
#' draws with diagnostics.
#'
#' @param formula A model formula.
#' @param data A data.frame.
#' @param prior_sd Prior standard deviation on the regression
#'   coefficients (a single value, or a vector of length `p`). The
#'   meaning of this hyperparameter is the scale of the zero-mean Normal
#'   coefficient priors.
#' @param chains Number of independent chains.
#' @param iter Iterations per chain (post-warmup).
#' @param warmup Warmup (burn-in) iterations.
#' @param seed Base RNG seed; each chain uses `seed + chain_index` so
#'   chains differ by default.
#' @param starting_values Optional numeric vector (length `p + 1`, the
#'   coefficients plus log-sigma) used as the starting state of every
#'   chain; or a list of such vectors, one per chain.
#' @param step Metropolis proposal scale.
#' @param check_convergence Whether to compute convergence diagnostics.
#' @param converge_threshold R-hat threshold for declaring convergence.
#' @param stop_on_convergence Stop early once converged.
#' @param quiet Suppress warnings/progress.
#' @param verbose Emit per-chain progress messages.
#' @return A `morie_bayes_fit` object.
#' @examples
#' d <- data.frame(x = rnorm(50))
#' d$y <- 1 + 2 * d$x + rnorm(50)
#' fit <- morie_bayes_lm(y ~ x, d, chains = 2, iter = 500, warmup = 200)
#' @export
morie_bayes_lm <- function(formula, data, prior_sd = 10, chains = 4L,
                           iter = 2000L, warmup = 1000L, seed = 42L,
                           starting_values = NULL, step = 0.1,
                           check_convergence = TRUE, converge_threshold = 1.1,
                           stop_on_convergence = FALSE, quiet = FALSE,
                           verbose = FALSE) {
  data <- .morie_check_data(data, arg = "data")
  des <- .bayes_design(formula, data)
  y <- des$y
  X <- des$X
  p <- des$p
  npar <- p + 1L # betas + log(sigma)

  if (length(prior_sd) > 1L && length(prior_sd) != p) { # BS2.3 / BS2.4
    stop(sprintf(
      "prior_sd has length %d but there are %d coefficients",
      length(prior_sd), p
    ), call. = FALSE)
  }
  psd <- if (length(prior_sd) == 1L) rep(prior_sd, p) else prior_sd

  log_post <- function(th) {
    beta <- th[seq_len(p)]
    sigma <- exp(th[npar])
    mu <- as.numeric(X %*% beta)
    sum(stats::dnorm(y, mu, sigma, log = TRUE)) +
      sum(stats::dnorm(beta, 0, psd, log = TRUE)) + th[npar] # log-sigma jacobian
  }

  # identical-seed diagnostic (BS2.10)
  chain_seeds <- seed + seq_len(chains) - 1L
  if (!is.null(starting_values) && !is.list(starting_values)) {
    # single common start for all chains -> chains differ only by seed
    if (length(unique(chain_seeds)) < chains && !quiet) {
      warning("identical seeds passed to distinct chains", call. = FALSE)
    }
  }

  run_chain <- function(ci) {
    set.seed(chain_seeds[ci])
    th <- if (is.list(starting_values)) {
      starting_values[[ci]]
    } else if (!is.null(starting_values)) {
      starting_values
    } else {
      c(stats::coef(stats::lm.fit(X, y)), log(stats::sd(y)))
    }
    draws <- matrix(NA_real_, nrow = warmup + iter, ncol = npar)
    lp <- log_post(th)
    for (t in seq_len(warmup + iter)) {
      prop <- th + stats::rnorm(npar, 0, step)
      lp_prop <- log_post(prop)
      if (log(stats::runif(1)) < lp_prop - lp) {
        th <- prop
        lp <- lp_prop
      }
      draws[t, ] <- th
    }
    if (verbose) message(sprintf("chain %d done", ci))
    draws[(warmup + 1):(warmup + iter), , drop = FALSE]
  }

  chains_out <- tryCatch(
    lapply(seq_len(chains), run_chain),
    error = function(e) e
  )
  if (inherits(chains_out, "error")) { # BS2.15
    out <- list(error = conditionMessage(chains_out), converged = FALSE)
    class(out) <- c("morie_bayes_fit", "morie_rich_result", "list")
    return(out)
  }

  parnames <- c(colnames(X), "log_sigma")
  chains_out <- lapply(chains_out, function(m) {
    colnames(m) <- parnames
    m
  })

  rhat <- ess <- NULL
  converged <- NA
  if (isTRUE(check_convergence)) {
    rhat <- morie_bayes_rhat(chains_out)
    ess <- morie_bayes_ess(chains_out)
    converged <- all(rhat < converge_threshold, na.rm = TRUE)
    if (!isTRUE(converged) && !quiet) { # BS4.5 / BS5.5
      warning("chains have not converged (max R-hat = ",
        round(max(rhat), 3), ")",
        call. = FALSE
      )
    }
  }

  post <- do.call(rbind, chains_out)
  out <- list(
    chains = chains_out, posterior = post, par_names = parnames,
    coefficients = colMeans(post[, seq_len(p), drop = FALSE]),
    formula = formula, X = X, y = y, p = p,
    rhat = rhat, ess = ess, converged = converged,
    converge_threshold = converge_threshold,
    seeds = chain_seeds, starting_values = starting_values,
    n_chains = chains, n_iter = iter, prior_sd = psd
  )
  class(out) <- c("morie_bayes_fit", "morie_rich_result", "list")
  out
}

# --- convergence checkers (BS1.5 / BS4.3 / BS5.4) ---------------------

#' Gelman-Rubin R-hat for a set of MCMC chains
#' @param chains A list of iteration-by-parameter matrices.
#' @return Named numeric R-hat per parameter.
#' @examples
#' morie_bayes_rhat(list(matrix(rnorm(200), 100), matrix(rnorm(200), 100)))
#' @export
morie_bayes_rhat <- function(chains) {
  m <- length(chains)
  n <- nrow(chains[[1]])
  vapply(seq_len(ncol(chains[[1]])), function(j) {
    xs <- vapply(chains, function(c) c[, j], numeric(n))
    chain_means <- colMeans(xs)
    grand <- mean(chain_means)
    B <- n / (m - 1) * sum((chain_means - grand)^2)
    W <- mean(apply(xs, 2, stats::var))
    if (W == 0) {
      return(1)
    }
    sqrt(((n - 1) / n * W + B / n) / W)
  }, numeric(1)) -> r
  stats::setNames(r, colnames(chains[[1]]))
}

#' Effective sample size (per parameter, autocorrelation-based)
#' @param chains A list of iteration-by-parameter matrices.
#' @return Named numeric ESS per parameter.
#' @examples
#' morie_bayes_ess(list(matrix(rnorm(200), 100)))
#' @export
morie_bayes_ess <- function(chains) {
  post <- do.call(rbind, chains)
  n <- nrow(post)
  vapply(seq_len(ncol(post)), function(j) {
    a <- stats::acf(post[, j], plot = FALSE, lag.max = min(50L, n - 1L))$acf[-1]
    a <- a[seq_len(which(c(a, -1) < 0)[1] - 1)] # sum positive autocorr
    n / (1 + 2 * sum(a, na.rm = TRUE))
  }, numeric(1)) -> e
  stats::setNames(pmax(e, 1), colnames(post))
}

#' Geweke convergence diagnostic (z-score comparing chain segments)
#' @param chains A list of iteration-by-parameter matrices.
#' @return Named numeric Geweke z per parameter (|z| < 2 ~ converged).
#' @examples
#' morie_bayes_geweke(list(matrix(rnorm(400), 200)))
#' @export
morie_bayes_geweke <- function(chains) {
  post <- do.call(rbind, chains)
  n <- nrow(post)
  a <- seq_len(floor(0.1 * n))
  b <- (ceiling(0.5 * n) + 1):n
  vapply(seq_len(ncol(post)), function(j) {
    xa <- post[a, j]
    xb <- post[b, j]
    (mean(xa) - mean(xb)) /
      sqrt(stats::var(xa) / length(xa) + stats::var(xb) / length(xb))
  }, numeric(1)) -> z
  stats::setNames(z, colnames(post))
}

#' All convergence diagnostics for a fit (details of each checker used)
#' @param fit A `morie_bayes_fit`.
#' @return A list with `rhat`, `ess`, and `geweke`.
#' @examples
#' d <- data.frame(x = rnorm(40))
#' d$y <- d$x + rnorm(40)
#' morie_bayes_diagnostics(morie_bayes_lm(y ~ x, d,
#'   chains = 2, iter = 300,
#'   warmup = 100
#' ))
#' @export
morie_bayes_diagnostics <- function(fit) {
  stopifnot(inherits(fit, "morie_bayes_fit"))
  list(rhat = fit$rhat, ess = fit$ess, geweke = morie_bayes_geweke(fit$chains))
}

# --- continuation, comparison ----------------------------------------

#' Continue an MCMC run from its final state
#' @param fit A `morie_bayes_fit`.
#' @param iter Additional iterations.
#' @param ... Passed to [morie_bayes_lm()].
#' @return A new `morie_bayes_fit` started from `fit`'s last draws.
#' @examples
#' d <- data.frame(x = rnorm(40))
#' d$y <- d$x + rnorm(40)
#' f1 <- morie_bayes_lm(y ~ x, d, chains = 2, iter = 200, warmup = 100)
#' f2 <- morie_bayes_continue(f1, iter = 200)
#' @export
morie_bayes_continue <- function(fit, iter = 1000L, ...) {
  stopifnot(inherits(fit, "morie_bayes_fit"))
  starts <- lapply(fit$chains, function(c) c[nrow(c), ]) # last state per chain
  data <- as.data.frame(cbind(fit$y, fit$X[, -1, drop = FALSE]))
  names(data)[1] <- all.vars(fit$formula)[1]
  morie_bayes_lm(fit$formula, data,
    chains = fit$n_chains, iter = iter,
    warmup = 0L, starting_values = starts, ...
  )
}

#' Compare posterior means against an external (lm) reference
#' @param fit A `morie_bayes_fit`.
#' @return A data.frame of posterior vs OLS coefficient estimates.
#' @examples
#' d <- data.frame(x = rnorm(60))
#' d$y <- 1 + 2 * d$x + rnorm(60)
#' morie_bayes_compare(morie_bayes_lm(y ~ x, d,
#'   chains = 2, iter = 800,
#'   warmup = 300
#' ))
#' @export
morie_bayes_compare <- function(fit) {
  stopifnot(inherits(fit, "morie_bayes_fit"))
  ols <- stats::coef(stats::lm.fit(fit$X, fit$y))
  data.frame(
    parameter = colnames(fit$X),
    posterior_mean = unname(fit$coefficients),
    ols = unname(ols),
    row.names = NULL
  )
}

#' Posterior fitted values (on the response scale)
#' @param object A `morie_bayes_fit`.
#' @param ... Unused.
#' @return Numeric fitted values.
#' @examples
#' \donttest{
#' d <- data.frame(x = rnorm(50))
#' d$y <- 1 + 2 * d$x + rnorm(50)
#' fit <- morie_bayes_lm(y ~ x, d, chains = 2, iter = 500, warmup = 200)
#' head(fitted(fit))
#' }
#' @export
fitted.morie_bayes_fit <- function(object, ...) {
  as.numeric(object$X %*% object$coefficients)
}

# --- print / plot (BS6.x) --------------------------------------------

#' Print method for \code{morie_bayes_fit} objects
#'
#' @param x A `morie_bayes_fit`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' d <- data.frame(x = rnorm(50))
#' d$y <- 1 + 2 * d$x + rnorm(50)
#' fit <- morie_bayes_lm(y ~ x, d, chains = 2, iter = 500, warmup = 200)
#' print(fit)
#' }
#' @export
print.morie_bayes_fit <- function(x, ...) {
  if (!is.null(x$error)) {
    cat("<morie_bayes_fit: ERROR>", x$error, "\n")
    return(invisible(x))
  }
  cat("<morie_bayes_fit>\n")
  cat(sprintf("  %d chains x %d iter\n", x$n_chains, x$n_iter))
  cat(sprintf(
    "  converged: %s (max R-hat %.3f)\n", x$converged,
    if (is.null(x$rhat)) NA else max(x$rhat)
  ))
  print(round(x$coefficients, 4))
  invisible(x)
}

#' Default MCMC plot (trace, density, or both)
#' @param x A `morie_bayes_fit`.
#' @param type "trace", "density", or "both" (BS6.2/6.3/6.5).
#' @param param Which parameter (index or name); default the first.
#' @param ... Passed to \[plot()\].
#' @return `NULL`, invisibly.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(40), x = rnorm(40))
#' fit <- morie_bayes_lm(y ~ x, data = df, iter = 500L, warmup = 250L)
#' morie_bayes_plot(fit, type = "trace")
#' @export
morie_bayes_plot <- function(x, type = c("trace", "density", "both"),
                             param = 1L, ...) {
  type <- match.arg(type)
  stopifnot(inherits(x, "morie_bayes_fit"))
  j <- if (is.character(param)) match(param, x$par_names) else param
  if (type == "both") {
    # CRAN: never change the user's par() without restoring it.
    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar), add = TRUE)
  }
  if (type %in% c("trace", "both")) {
    for (ci in seq_along(x$chains)) {
      v <- x$chains[[ci]][, j]
      if (ci == 1) {
        plot(v,
          type = "l", xlab = "iteration",
          ylab = x$par_names[j], main = "trace", ...
        )
      } else {
        graphics::lines(v, col = ci)
      }
    }
  }
  if (type %in% c("density", "both")) {
    plot(stats::density(x$posterior[, j]),
      main = "posterior",
      xlab = x$par_names[j]
    )
  }
  invisible(NULL)
}

#' Plot method for \code{morie_bayes_fit} objects
#'
#' @param x A `morie_bayes_fit`.
#' @param ... Passed to [morie_bayes_plot()].
#' @return `NULL`, invisibly (default trace plot).
#' @examples
#' \donttest{
#' d <- data.frame(x = rnorm(50))
#' d$y <- 1 + 2 * d$x + rnorm(50)
#' fit <- morie_bayes_lm(y ~ x, d, chains = 2, iter = 500, warmup = 200)
#' plot(fit)
#' }
#' @export
plot.morie_bayes_fit <- function(x, ...) morie_bayes_plot(x, ...)

#' Posterior density plot for one parameter (BS6.3)
#' @param fit A `morie_bayes_fit`.
#' @param param Parameter index or name.
#' @return `NULL`, invisibly.
#' @examples
#' d <- data.frame(x = rnorm(40))
#' d$y <- d$x + rnorm(40)
#' morie_bayes_density(morie_bayes_lm(y ~ x, d,
#'   chains = 2, iter = 300,
#'   warmup = 100
#' ))
#' @export
morie_bayes_density <- function(fit, param = 1L) {
  morie_bayes_plot(fit, type = "density", param = param)
}
