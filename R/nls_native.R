# SPDX-License-Identifier: AGPL-3.0-or-later
# Nonlinear least squares from a model formula, native.
# Source: Bates, D. M. and Watts, D. G. (1988), Nonlinear Regression
# Analysis and Its Applications, Wiley: Sec. 2.2 (Gauss-Newton with step
# halving) and Sec. 2.2.3 (the relative-offset convergence criterion);
# Bates, D. M. and Chambers, J. M. (1992), Nonlinear models, ch. 10 of
# Statistical Models in S (the nls iteration: QR of the gradient, the
# increment qr.coef(QR, r), the step factor halved on failure and doubled
# (up to 1) after a success, convergence when
# ||Q1'r|| / ||Q2'r|| <= tol, tested before each step); Levenberg, K.
# (1944), Quart. Appl. Math. 2, 164-168, and Marquardt, D. W. (1963),
# SIAM J. Appl. Math. 11, 431-441 (the damped alternative, with More's
# (1978) diagonal scaling).
#
# The formula's right-hand side is turned into a function with
#   as.function(c(start, rhs))
# whose enclosing environment holds the data (parent: base R plus a few
# distribution functions), so the expression is evaluated as a function
# body; no code is ever built from text.  The Jacobian is analytic,
# from stats::deriv() on the right-hand side, when deriv() knows every
# function in it; otherwise it is by central differences.

# Functions available to a model formula beyond base R.
.nlsn_funs <- c("pnorm", "dnorm", "qnorm", "plogis", "dlogis", "qlogis")

#' Environment holding the data for a model formula
#'
#' @param data A data frame or named list.
#' @return An environment whose parent holds base R and a few
#'   distribution functions from stats.
#' @keywords internal
.nlsn_env <- function(data) {
  fe <- new.env(parent = baseenv())
  for (f in .nlsn_funs) assign(f, getExportedValue("stats", f), envir = fe)
  list2env(as.list(data), parent = fe)
}

#' Remove I() wrappers from an expression
#'
#' @param e A language object.
#' @return The expression with every \code{I(x)} replaced by \code{(x)}.
#' @keywords internal
.nlsn_strip_I <- function(e) {
  if (!is.call(e)) return(e)
  if (identical(e[[1L]], as.name("I")) && length(e) == 2L)
    return(call("(", .nlsn_strip_I(e[[2L]])))
  for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] <- .nlsn_strip_I(e[[i]])
  e
}

#' Build the model, response and Jacobian functions for morie_nls
#'
#' @param formula Two-sided (or one-sided) model formula.
#' @param data Data frame or list.
#' @param start Named list or vector of starting values.
#' @param jacobian \code{"auto"}, \code{"analytic"} or \code{"central"}.
#' @return A list with \code{f} (theta -> fitted values), \code{J}
#'   (theta -> Jacobian), \code{y}, \code{theta0}, \code{pnames},
#'   \code{jacobian} (the type used) and \code{make_f} (rebuilds f for
#'   new data).
#' @keywords internal
.nlsn_model <- function(formula, data, start, jacobian = "auto") {
  if (!inherits(formula, "formula")) stop("formula must be a formula.", call. = FALSE)
  if (is.null(names(start)) || any(!nzchar(names(start))))
    stop("start must be a named list or vector of starting values.", call. = FALSE)
  start <- as.list(start)
  lens <- lengths(start)
  if (any(lens < 1L)) stop("every starting value must be non-empty.", call. = FALSE)
  theta0 <- as.numeric(unlist(start, use.names = FALSE))
  if (any(!is.finite(theta0))) stop("starting values must be finite numbers.", call. = FALSE)
  pnames <- names(unlist(start))
  snames <- names(start)
  one <- length(formula) == 2L
  rhs <- formula[[if (one) 2L else 3L]]
  data <- as.list(data)
  miss <- setdiff(all.vars(rhs), c(snames, names(data)))
  if (length(miss))
    stop("variables not found in data or start: ", paste(miss, collapse = ", "), call. = FALSE)
  env <- .nlsn_env(data)
  make_f <- function(e) {
    fn <- as.function(c(start, rhs))
    environment(fn) <- e
    fn
  }
  fmod <- make_f(env)
  idx <- split(seq_along(theta0), rep(seq_along(lens), lens))
  as_args <- function(th) {
    a <- lapply(idx, function(k) th[k])
    names(a) <- snames
    a
  }
  f <- function(th) as.numeric(do.call(fmod, as_args(th)))
  if (one) {
    y <- 0
  } else {
    fl <- as.function(list(formula[[2L]]))
    environment(fl) <- env
    y <- as.numeric(fl())
  }
  f0 <- f(theta0)
  n <- max(length(f0), length(y))
  if (!(length(y) %in% c(1L, n)) || !(length(f0) %in% c(1L, n)))
    stop("the left- and right-hand sides have different lengths.", call. = FALSE)
  y <- rep_len(y, n)
  p <- length(theta0)
  central <- function(th) {
    J <- matrix(0, n, p)
    for (j in seq_len(p)) {
      h <- .Machine$double.eps^(1 / 3) * max(abs(th[j]), 1)
      tp <- th
      tm <- th
      tp[j] <- th[j] + h
      tm[j] <- th[j] - h
      J[, j] <- rep_len(f(tp) - f(tm), n) / (2 * h)
    }
    J
  }
  J <- NULL
  used <- "central"
  if (jacobian != "central" && all(lens == 1L)) {
    dfun <- tryCatch(stats::deriv(.nlsn_strip_I(rhs), pnames, function.arg = pnames),
                     error = function(e) NULL)
    if (!is.null(dfun)) {
      environment(dfun) <- env
      Ja <- function(th) {
        v <- do.call(dfun, as.list(stats::setNames(th, pnames)))
        g <- attr(v, "gradient")
        g <- matrix(as.numeric(g), nrow = NROW(g))
        if (nrow(g) != n) g <- matrix(rep(colSums(matrix(g, ncol = p)), each = n), n, p)
        g
      }
      g0 <- tryCatch(Ja(theta0), error = function(e) NULL)
      if (!is.null(g0) && all(dim(g0) == c(n, p))) {
        J <- Ja
        used <- "analytic"
      }
    }
  }
  if (is.null(J)) {
    if (jacobian == "analytic")
      stop("stats::deriv() cannot differentiate this model; use jacobian = 'central'.", call. = FALSE)
    J <- central
  }
  list(f = function(th) rep_len(f(th), n), J = J, y = y, theta0 = theta0,
       pnames = pnames, jacobian = used, make_f = make_f, as_args = as_args,
       n = n, p = p)
}

#' Relative-offset convergence criterion
#'
#' @param QR QR decomposition of the Jacobian.
#' @param r Residual vector.
#' @param p Number of parameters.
#' @param so Scale offset term ((n - p) * scaleOffset^2).
#' @return The relative offset of Bates and Watts (1988).
#' @keywords internal
.nlsn_conv <- function(QR, r, p, so) {
  rr <- qr.qty(QR, r)
  sqrt(sum(rr[seq_len(p)]^2) / (so + sum(rr[-seq_len(p)]^2)))
}

#' Nonlinear least squares from a model formula (native)
#'
#' Fits \eqn{y = f(x, \theta) + \epsilon} by least squares, the model
#' being written as a formula such as \code{y ~ a * (1 - exp(-b * x))}
#' whose parameters are the names in \code{start}.  Gauss-Newton follows
#' the nls algorithm of Bates and Chambers (1992): increments from the QR
#' decomposition of the gradient, step halving with a step factor that
#' doubles back after each success, and the relative-offset convergence
#' criterion of Bates and Watts (1988).  Levenberg-Marquardt is available
#' for poor starting values.  The Jacobian is analytic (via
#' \code{stats::deriv}) when possible, otherwise by central differences.
#'
#' @param formula Model formula, \code{response ~ expression}; a
#'   one-sided \code{~ expression} minimises the sum of squares of the
#'   expression.  Functions from base R and \code{pnorm}, \code{dnorm},
#'   \code{qnorm}, \code{plogis}, \code{dlogis}, \code{qlogis} may be used.
#' @param data Data frame (or named list) holding the variables.
#' @param start Named list or vector of starting values; vector-valued
#'   entries give indexed parameters (\code{b[1]}, \code{b[2]}, named
#'   \code{b1}, \code{b2} in the output).
#' @param algorithm \code{"gauss-newton"} (default) or
#'   \code{"levenberg-marquardt"}.
#' @param control List of settings: \code{maxiter} (50), \code{tol}
#'   (1e-5, on the relative offset), \code{minFactor} (1/1024),
#'   \code{scaleOffset} (0), \code{warnOnly} (FALSE: non-convergence is an
#'   error), \code{jacobian} ("auto", "analytic" or "central"), and for
#'   Levenberg-Marquardt \code{lambda} (initial damping, 1e-3) and
#'   \code{maxlambda} (1e16).
#' @return An object of class \code{"morie_nls"}: \code{coefficients},
#'   \code{coef_table} (Estimate, Std. Error, t value, Pr(>|t|)),
#'   \code{sigma} (residual standard error), \code{df} (p, n - p),
#'   \code{cov.unscaled}, \code{vcov}, \code{fitted.values},
#'   \code{residuals}, \code{deviance}, \code{convInfo} (isConv,
#'   finIter, finTol, stopCode, stopMessage), \code{algorithm},
#'   \code{jacobian}, \code{formula}, \code{call}.
#' @references Bates, D. M. and Watts, D. G. (1988). Nonlinear
#'   Regression Analysis and Its Applications. Wiley.
#'
#'   Bates, D. M. and Chambers, J. M. (1992). Nonlinear models. In
#'   Chambers, J. M. and Hastie, T. J. (eds), Statistical Models in S.
#'   Wadsworth & Brooks/Cole.
#'
#'   Marquardt, D. W. (1963). An algorithm for least-squares estimation
#'   of nonlinear parameters. SIAM Journal on Applied Mathematics 11,
#'   431-441.
#'
#'   More, J. J. (1978). The Levenberg-Marquardt algorithm:
#'   implementation and theory. Lecture Notes in Mathematics 630, 105-116.
#' @examples
#' set.seed(1)
#' d <- data.frame(x = seq(0, 5, length.out = 40))
#' d$y <- 3 * (1 - exp(-0.8 * d$x)) + rnorm(40, sd = 0.05)
#' fit <- morie_nls(y ~ a * (1 - exp(-b * x)), d, start = list(a = 1, b = 1))
#' summary(fit)
#' predict(fit, data.frame(x = c(1, 2)))
#' @export
morie_nls <- function(formula, data, start,
                      algorithm = c("gauss-newton", "levenberg-marquardt"),
                      control = list()) {
  algorithm <- match.arg(algorithm)
  cl <- match.call()
  ctrl <- list(maxiter = 50L, tol = 1e-5, minFactor = 1 / 1024, scaleOffset = 0,
               warnOnly = FALSE, jacobian = "auto", lambda = 1e-3, maxlambda = 1e16)
  bad <- setdiff(names(control), names(ctrl))
  if (length(bad)) stop("unknown control settings: ", paste(bad, collapse = ", "), call. = FALSE)
  ctrl[names(control)] <- control
  ctrl$jacobian <- match.arg(ctrl$jacobian, c("auto", "analytic", "central"))
  m <- .nlsn_model(formula, data, start, ctrl$jacobian)
  n <- m$n
  p <- m$p
  if (n <= p) stop("need more observations than parameters.", call. = FALSE)
  y <- m$y
  so <- if (ctrl$scaleOffset) (n - p) * ctrl$scaleOffset^2 else 0
  th <- m$theta0
  r <- y - m$f(th)
  if (any(!is.finite(r))) stop("missing or infinite values in the model at the starting values.",
                               call. = FALSE)
  dev <- sum(r^2)
  QR <- qr(m$J(th))
  if (QR$rank < p) stop("singular gradient matrix at initial parameter estimates.", call. = FALSE)
  conv <- .nlsn_conv(QR, r, p, so)
  stopCode <- 0L
  msg <- "converged"
  isConv <- FALSE
  it <- 0L
  if (algorithm == "gauss-newton") {
    fac <- 1
    for (i in seq_len(ctrl$maxiter + 1L) - 1L) {
      it <- i
      if (conv <= ctrl$tol) {
        isConv <- TRUE
        break
      }
      if (i == ctrl$maxiter) break
      incr <- qr.coef(QR, r)
      ok <- FALSE
      while (fac >= ctrl$minFactor) {
        cand <- th + fac * incr
        rc <- y - m$f(cand)
        dc <- sum(rc^2)
        if (is.finite(dc) && dc <= dev) {
          th <- cand
          r <- rc
          dev <- dc
          fac <- min(2 * fac, 1)
          ok <- TRUE
          break
        }
        fac <- fac / 2
      }
      if (!ok) {
        stopCode <- 2L
        msg <- sprintf("step factor %g reduced below 'minFactor' of %g", fac, ctrl$minFactor)
        it <- i + 1L
        break
      }
      QR <- qr(m$J(th))
      if (QR$rank < p) {
        stopCode <- 1L
        msg <- "singular gradient"
        it <- i + 1L
        break
      }
      conv <- .nlsn_conv(QR, r, p, so)
    }
  } else {
    lam <- ctrl$lambda
    Jm <- m$J(th)
    for (i in seq_len(ctrl$maxiter + 1L) - 1L) {
      it <- i
      if (conv <= ctrl$tol) {
        isConv <- TRUE
        break
      }
      if (i == ctrl$maxiter) break
      dsc <- sqrt(colSums(Jm^2))
      dsc[dsc == 0] <- 1
      ok <- FALSE
      while (lam <= ctrl$maxlambda) {
        A <- rbind(Jm, diag(sqrt(lam) * dsc, p))
        incr <- qr.coef(qr(A), c(r, numeric(p)))
        cand <- th + incr
        rc <- y - m$f(cand)
        dc <- sum(rc^2)
        if (is.finite(dc) && dc <= dev) {
          th <- cand
          r <- rc
          dev <- dc
          lam <- max(lam / 10, 1e-12)
          ok <- TRUE
          break
        }
        lam <- lam * 10
      }
      if (!ok) {
        stopCode <- 2L
        msg <- sprintf("damping parameter increased beyond 'maxlambda' of %g", ctrl$maxlambda)
        it <- i + 1L
        break
      }
      Jm <- m$J(th)
      QR <- qr(Jm)
      if (QR$rank < p) {
        stopCode <- 1L
        msg <- "singular gradient"
        it <- i + 1L
        break
      }
      conv <- .nlsn_conv(QR, r, p, so)
    }
  }
  if (!isConv && stopCode == 0L) {
    stopCode <- 3L
    msg <- sprintf("number of iterations exceeded maximum of %d", ctrl$maxiter)
  }
  if (!isConv) {
    if (ctrl$warnOnly) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
  }
  names(th) <- m$pnames
  rdf <- n - p
  resvar <- dev / rdf
  XtXinv <- chol2inv(qr.R(QR))
  po <- order(QR$pivot)
  XtXinv <- XtXinv[po, po, drop = FALSE]
  dimnames(XtXinv) <- list(m$pnames, m$pnames)
  se <- sqrt(diag(XtXinv) * resvar)
  tv <- th / se
  tab <- cbind(th, se, tv, 2 * stats::pt(abs(tv), rdf, lower.tail = FALSE))
  dimnames(tab) <- list(m$pnames, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  fitted <- y - r
  structure(list(coefficients = th, coef_table = tab, sigma = sqrt(resvar),
                 df = c(p, rdf), cov.unscaled = XtXinv, vcov = XtXinv * resvar,
                 fitted.values = fitted, residuals = r, deviance = dev,
                 convInfo = list(isConv = isConv, finIter = it, finTol = conv,
                                 stopCode = stopCode, stopMessage = msg),
                 algorithm = algorithm, jacobian = m$jacobian, control = ctrl,
                 formula = formula, start = start, n = n, call = cl,
                 .make_f = m$make_f),
            class = "morie_nls")
}

#' @rdname morie_nls
#' @param x,object A \code{"morie_nls"} fit.
#' @param ... Unused.
#' @export
print.morie_nls <- function(x, ...) {
  cat("Nonlinear regression model (morie native, ", x$algorithm, ")\n", sep = "")
  cat("  model: ", paste(deparse(x$formula), collapse = " "), "\n", sep = "")
  print(x$coefficients)
  cat(" residual sum-of-squares: ", format(x$deviance), "\n\n", sep = "")
  ci <- x$convInfo
  if (ci$isConv) cat("Number of iterations to convergence:", ci$finIter,
                     "\nAchieved convergence tolerance:", format(ci$finTol), "\n")
  else cat("Not converged:", ci$stopMessage, "\n")
  invisible(x)
}

#' @rdname morie_nls
#' @export
summary.morie_nls <- function(object, ...) {
  structure(list(formula = object$formula, coefficients = object$coef_table,
                 sigma = object$sigma, df = object$df,
                 cov.unscaled = object$cov.unscaled, convInfo = object$convInfo,
                 residuals = object$residuals, algorithm = object$algorithm,
                 jacobian = object$jacobian),
            class = "summary.morie_nls")
}

#' @rdname morie_nls
#' @export
print.summary.morie_nls <- function(x, ...) {
  cat("\nFormula: ", paste(deparse(x$formula), collapse = " "), "\n\nParameters:\n", sep = "")
  stats::printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE)
  cat("\nResidual standard error:", format(signif(x$sigma, 4)), "on", x$df[2],
      "degrees of freedom\n")
  ci <- x$convInfo
  if (ci$isConv) cat("\nNumber of iterations to convergence:", ci$finIter,
                     "\nAchieved convergence tolerance:", format(ci$finTol), "\n")
  else cat("\nNot converged:", ci$stopMessage, "\n")
  cat("(", x$algorithm, ", ", x$jacobian, " Jacobian)\n", sep = "")
  invisible(x)
}

#' @rdname morie_nls
#' @export
coef.morie_nls <- function(object, ...) object$coefficients

#' @rdname morie_nls
#' @export
vcov.morie_nls <- function(object, ...) object$vcov

#' @rdname morie_nls
#' @export
fitted.morie_nls <- function(object, ...) object$fitted.values

#' @rdname morie_nls
#' @export
residuals.morie_nls <- function(object, ...) object$residuals

#' @rdname morie_nls
#' @export
deviance.morie_nls <- function(object, ...) object$deviance

#' @rdname morie_nls
#' @export
df.residual.morie_nls <- function(object, ...) object$df[2L]

#' @rdname morie_nls
#' @export
logLik.morie_nls <- function(object, ...) {
  n <- object$n
  val <- -n / 2 * (log(2 * pi) + 1 - log(n) + log(object$deviance))
  structure(val, nobs = n, df = object$df[1L] + 1L, class = "logLik")
}

#' @rdname morie_nls
#' @param newdata Optional data frame (or list) of predictor values.
#' @export
predict.morie_nls <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted.values)
  fn <- object$.make_f(.nlsn_env(newdata))
  th <- object$coefficients
  lens <- lengths(object$start)
  idx <- split(seq_along(th), rep(seq_along(lens), lens))
  a <- lapply(idx, function(k) unname(th[k]))
  names(a) <- names(object$start)
  as.numeric(do.call(fn, a))
}
