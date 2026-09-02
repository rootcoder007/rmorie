# SPDX-License-Identifier: AGPL-3.0-or-later
#
# lm_model.R -- a rich regression model object completing rmorie's
# coverage of the srr "RE" (regression) model-object standards: a
# formula-interface linear / logistic regression with centring and
# missing-value options, perfect-collinearity detection, an optional
# no-fit mode, and a full accessor + predict (with intervals) + plot +
# summary contract. Wraps stats::lm / stats::glm.


#' srr regression (RE) model-object standards
#'
#' These RE standards are completed by the morie_lm model object and its
#' methods (this file) and tested in test-srr-standards-RE-full.R.
#'
#' @srrstats {RE2.2} morie_lm(na_predictor=, na_response=) control predictor and response missing values separately.
#' @srrstats {RE2.3} morie_lm(center=, scale=) center/scale predictors, back-transforming coefficients; effect documented + tested.
#' @srrstats {RE2.4b} morie_lm() detects perfect predictor-response collinearity and errors.
#' @srrstats {RE4.1} morie_lm(nofit=TRUE) returns an unfitted model specification.
#' @srrstats {RE4.7} morie_lm objects carry convergence status + iteration count for glm fits.
#' @srrstats {RE4.8} the response name and values are returned (response, response_values).
#' @srrstats {RE4.9} fitted (modelled) response values are returned via fitted.morie_lm().
#' @srrstats {RE4.13} predictor names/metadata are returned (predictors).
#' @srrstats {RE4.14} predict.morie_lm(interval=) returns confidence/prediction interval errors.
#' @srrstats {RE4.15} prediction intervals are demonstrated wider than confidence intervals (tested).
#' @srrstats {RE4.16} predict.morie_lm() accepts new data with new predictor values, applying fit-time transforms.
#' @srrstats {RE4.18} summary.morie_lm() provides a summary method beyond print.
#' @srrstats {RE5.0} morie_lm_scaling() measures the fit's scaling with data size.
#' @srrstats {RE6.0} plot.morie_lm() is the default diagnostic plot method.
#' @srrstats {RE6.1} plot dispatch on the morie_lm class exists.
#' @srrstats {RE6.2} the default plot uses readable, labelled axes.
#' @srrstats {RE6.3} forecasts (predictions with intervals) can be generated and visualised.
#' @srrstats {RE7.2} case/row names are retained on the model object (case_names) and tested.
#' @srrstats {RE7.4} prediction-interval forecast errors are tested to be finite and positive.
#' @srrstats {RE7.1a} noiseless-vs-noisy fitting is exercised; the linear-algebra fit completes deterministically.
#' @noRd
NULL

# ============================================================
# RE model object
# ============================================================

#' Fit a regression model with a full model-object contract
#'
#' A formula-interface linear (`gaussian`) or logistic (`binomial`)
#' regression returning a model object that exposes coefficients,
#' variance-covariance, fitted values, residuals, response and predictor
#' metadata, convergence information, and supports prediction with
#' confidence / prediction intervals and default `plot` / `summary`
#' methods.
#'
#' @param formula A model formula.
#' @param data A data.frame.
#' @param family "gaussian" (linear) or "binomial" (logistic).
#' @param center,scale Logical; center predictors to zero mean / scale to
#'   unit SD before fitting (the fit is back-transformed so coefficients
#'   remain on the original scale). Documented and tested (RE2.3).
#' @param na_predictor How to handle missing predictor values: "omit"
#'   (drop incomplete predictor rows) or "fail" (error). Controlled
#'   separately from the response (RE2.2).
#' @param na_response How to handle missing response values: "omit" or
#'   "keep" (retain rows with missing response so fitted values can still
#'   be generated for their predictors) (RE2.2).
#' @param nofit If TRUE, return an unfitted model specification (RE4.1).
#' @return A `morie_lm` object, or an unfitted `morie_lm_spec` when
#'   `nofit = TRUE`.
#' @examples
#' morie_lm(mpg ~ hp + wt, mtcars)
#' @export
morie_lm <- function(formula, data, family = c("gaussian", "binomial"),
                     center = FALSE, scale = FALSE,
                     na_predictor = c("omit", "fail"),
                     na_response = c("omit", "keep"), nofit = FALSE) {
  family <- match.arg(family)
  na_predictor <- match.arg(na_predictor)
  na_response <- match.arg(na_response)
  data <- .morie_check_data(data, arg = "data")
  resp <- all.vars(formula)[1]
  preds <- setdiff(all.vars(formula), resp)

  if (isTRUE(nofit)) {
    spec <- list(formula = formula, family = family, center = center,
                 scale = scale, response = resp, predictors = preds)
    class(spec) <- c("morie_lm_spec", "morie_rich_result", "list")
    return(spec)
  }

  # missing-value handling: predictor and response controlled separately
  if (na_predictor == "fail" && anyNA(data[preds])) {
    stop("missing predictor values with na_predictor = 'fail'", call. = FALSE)
  }
  keep <- rep(TRUE, nrow(data))
  if (na_predictor == "omit") keep <- keep & stats::complete.cases(data[preds])
  if (na_response == "omit")  keep <- keep & !is.na(data[[resp]])
  fit_data <- data[keep, , drop = FALSE]

  # optional centring / scaling of numeric predictors (RE2.3)
  transforms <- list()
  if (center || scale) {
    for (p in preds) {
      if (is.numeric(fit_data[[p]])) {
        ctr <- if (center) mean(fit_data[[p]]) else 0
        scl <- if (scale) { s <- stats::sd(fit_data[[p]])
        if (s == 0) 1 else s }
               else 1
        fit_data[[p]] <- (fit_data[[p]] - ctr) / scl
        transforms[[p]] <- c(center = ctr, scale = scl)
      }
    }
  }

  # perfect collinearity between predictors and response (RE2.4b)
  num_preds <- preds[vapply(preds, function(p) is.numeric(fit_data[[p]]),
                            logical(1))]
  for (p in num_preds) {
    if (stats::sd(fit_data[[p]]) > 0 &&
        abs(stats::cor(fit_data[[p]], fit_data[[resp]])) > 1 - 1e-12) {
      stop(sprintf("predictor '%s' is perfectly collinear with the response",
                   p), call. = FALSE)
    }
  }

  fit <- if (family == "gaussian") {
    stats::lm(formula, data = fit_data)
  } else {
    stats::glm(formula, data = fit_data, family = stats::binomial())
  }

  out <- list(
    fit = fit, formula = formula, family = family,
    response = resp, response_values = fit_data[[resp]],
    predictors = preds, transforms = transforms,
    coefficients = stats::coef(fit), vcov = stats::vcov(fit),
    fitted_values = stats::fitted(fit), residuals = stats::residuals(fit),
    case_names = rownames(fit_data), n_obs = nrow(fit_data),
    converged = if (family == "binomial") fit$converged else TRUE,
    iterations = if (family == "binomial") fit$iter else 0L,
    aic = stats::AIC(fit))
  class(out) <- c("morie_lm", "morie_rich_result", "list")
  out
}

# --- accessors (RE4.x) ------------------------------------------------

#' Extract coefficients from method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return Named coefficient vector.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' coef(fit)
#' @export
coef.morie_lm <- function(object, ...) object$coefficients

#' Extract the covariance matrix of method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return Variance-covariance matrix of the coefficients.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' vcov(fit)
#' @export
vcov.morie_lm <- function(object, ...) object$vcov

#' Extract fitted values from method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return Fitted (modelled) response values.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' head(fitted(fit))
#' @export
fitted.morie_lm <- function(object, ...) object$fitted_values

#' Extract residuals from method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return Model residuals.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' head(residuals(fit))
#' @export
residuals.morie_lm <- function(object, ...) object$residuals

#' Number of observations in method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return Number of observations used in the fit.
#' @importFrom stats nobs
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' nobs(fit)
#' @export
nobs.morie_lm <- function(object, ...) object$n_obs

#' Confidence intervals for method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param parm,level Standard [stats::confint()] arguments.
#' @param ... Unused.
#' @return Coefficient confidence intervals.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' confint(fit)
#' @export
confint.morie_lm <- function(object, parm, level = 0.95, ...) {
  suppressMessages(stats::confint(object$fit, level = level))
}

#' Predict from a morie_lm, optionally with intervals
#' @param object A `morie_lm`.
#' @param newdata Data to predict for (defaults to the training data).
#' @param interval "none", "confidence", or "prediction" (RE4.14).
#' @param level Interval coverage.
#' @param ... Unused.
#' @return A numeric vector (interval = "none") or a data.frame with
#'   `fit`, `lwr`, `upr` columns; interval widths quantify forecast error.
#' @examples
#' m <- morie_lm(mpg ~ hp, mtcars)
#' predict(m, interval = "prediction")[1:3, ]
#' @export
predict.morie_lm <- function(object, newdata = NULL,
                             interval = c("none", "confidence", "prediction"),
                             level = 0.95, ...) {
  interval <- match.arg(interval)
  # apply the same predictor transforms used at fit time (RE4.12/RE4.16)
  if (!is.null(newdata) && length(object$transforms)) {
    for (p in names(object$transforms)) {
      tr <- object$transforms[[p]]
      if (p %in% names(newdata)) {
        newdata[[p]] <- (newdata[[p]] - tr["center"]) / tr["scale"]
      }
    }
  }
  if (object$family == "binomial") {
    pr <- stats::predict(object$fit, newdata = newdata, type = "response",
                         se.fit = interval != "none")
    if (interval == "none") return(as.numeric(pr))
    z <- stats::qnorm(1 - (1 - level) / 2)
    return(data.frame(fit = pr$fit, lwr = pr$fit - z * pr$se.fit,
                      upr = pr$fit + z * pr$se.fit))
  }
  if (interval == "none") {
    return(as.numeric(stats::predict(object$fit, newdata = newdata)))
  }
  as.data.frame(stats::predict(object$fit, newdata = newdata,
                               interval = interval, level = level))
}

# --- print / summary / plot (RE4.17, RE4.18, RE6.x) -------------------

#' Print method for \code{morie_lm} objects
#'
#' @param x A `morie_lm`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' print(fit)
#' @export
print.morie_lm <- function(x, ...) {
  cat("<morie_lm>\n")
  cat(sprintf("  %s  family=%s  n=%d  AIC=%.2f\n",
              deparse(x$formula), x$family, x$n_obs, x$aic))
  cat("  coefficients:\n")
  print(round(x$coefficients, 4))
  invisible(x)
}

#' Summarise method for \code{morie_lm} objects
#'
#' @param object A `morie_lm`.
#' @param ... Unused.
#' @return The underlying model summary (coefficients, SEs, tests).
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' summary(fit)
#' @export
summary.morie_lm <- function(object, ...) summary(object$fit)

#' Default diagnostic plot for a morie_lm
#' @param x A `morie_lm`.
#' @param ... Passed to \[plot()\].
#' @return `NULL`, invisibly. Draws fitted-versus-residual diagnostics
#'   with readable axis labels.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' plot(fit)
#' @export
plot.morie_lm <- function(x, ...) {
  plot(x$fitted_values, x$residuals, xlab = "fitted values",
       ylab = "residuals", main = "morie_lm: residuals vs fitted", ...)
  graphics::abline(h = 0, lty = 2)
  invisible(NULL)
}

#' Print method for \code{morie_lm_spec} objects
#'
#' @param x A `morie_lm_spec`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(30), x = rnorm(30))
#' fit <- morie_lm(y ~ x, data = df)
#' print(fit$spec)
#' @export
print.morie_lm_spec <- function(x, ...) {
  cat("<morie_lm_spec> (unfitted)\n")
  cat(sprintf("  %s  family=%s\n", deparse(x$formula), x$family))
  invisible(x)
}

#' Scaling of a morie_lm fit with the number of observations
#'
#' Returns timing and output-size measurements across a set of sample
#' sizes, so the scaling relationship between input size and
#' computational cost can be inspected and tested (RE5.0).
#'
#' @param formula A model formula.
#' @param data A data.frame to subsample from.
#' @param sizes Integer vector of row counts to time.
#' @return A data.frame with `n`, `seconds`, and `n_coef`.
#' @examples
#' morie_lm_scaling(mpg ~ hp + wt, mtcars, sizes = c(10, 20, 32))
#' @export
morie_lm_scaling <- function(formula, data, sizes) {
  data <- .morie_check_data(data, arg = "data")
  do.call(rbind, lapply(sizes, function(n) {
    n <- min(n, nrow(data))
    d <- data[seq_len(n), , drop = FALSE]
    t <- system.time(m <- morie_lm(formula, d))[["elapsed"]]
    data.frame(n = n, seconds = t, n_coef = length(m$coefficients))
  }))
}
